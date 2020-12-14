# -- Load Packages ----

suppressMessages({
  library(data.table)
  library(DT)
  library(forecast)
  library(tseries)
  library(faosws)
  library(faoswsFlag)
  library(faoswsProcessing)
  library(faoswsUtil)
  library(faoswsImputation)
  library(imputeTS)
  library(ggplot2)
  library(sendmailR)
})

# -- Token QA ----

if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = '4e9d9a2e-5258-48b6-974f-3656d1af8217')
}

# -- Expand Year ----
expandYear <- function (data, areaVar = "geographicAreaM49", elementVar = "measuredElement", 
                        itemVar = "measuredItemCPC", yearVar = "timePointYears", 
                        valueVar = "Value", obsflagVar = "flagObservationStatus", 
                        methFlagVar = "flagMethod", newYears = NULL) 
{
  key = c(elementVar, areaVar, itemVar)
  keyDataFrame = data[, key, with = FALSE]
  keyDataFrame = keyDataFrame[with(keyDataFrame, order(get(key)))]
  keyDataFrame = keyDataFrame[!duplicated(keyDataFrame)]
  yearDataFrame = unique(data[, get(yearVar)])
  if (!is.null(newYears)) {
    yearDataFrame = unique(c(yearDataFrame, newYears, newYears - 
                               1, newYears - 2))
  }
  yearDataFrame = data.table(yearVar = yearDataFrame)
  colnames(yearDataFrame) = yearVar
  completeBasis = data.table(merge.data.frame(keyDataFrame, 
                                              yearDataFrame))
  expandedData = merge(completeBasis, data, by = colnames(completeBasis), 
                       all.x = TRUE)
  expandedData = fillRecord(expandedData, areaVar = areaVar, 
                            itemVar = itemVar, yearVar = yearVar,
                            flagObsVar = obsflagVar, 
                            flagMethodVar = methFlagVar)
  seriesToBlock = expandedData[(get(methFlagVar) != "u"), ]
  seriesToBlock[, `:=`(lastYearAvailable, max(get(yearVar))), 
                by = key]
  seriesToBlock[, `:=`(flagComb, paste(get(obsflagVar), get(methFlagVar), 
                                       sep = ";"))]
  seriesToBlock = seriesToBlock[get(yearVar) == lastYearAvailable & 
                                  flagComb == "M;-"]
  if (nrow(seriesToBlock) > 0) {
    seriesToBlock = seriesToBlock[, {
      max_year = max(as.integer(.SD[, timePointYears]))
      data.table(timePointYears = seq.int(max_year + 1, 
                                          newYears), Value = NA_real_, flagObservationStatus = "M", 
                 flagMethod = "-")[max_year < newYears]
    }, by = key]
    expandedData = merge(expandedData, seriesToBlock, by = c(areaVar, 
                                                             elementVar, itemVar, yearVar), all.x = TRUE, suffixes = c("", 
                                                                                                                       "_MDash"))
    expandedData[!is.na(flagMethod_MDash), `:=`(flagMethod, 
                                                flagMethod_MDash)]
    expandedData = expandedData[, colnames(data), with = FALSE]
  }
  expandedData
}

# -- Pull preparation data SLC ----

domainPP <- 'prod_prices'
datasetVal <- 'annual_producer_prices_validated'
datasetPrep <- 'annual_producer_prices_prep' #'annual_producer_prices_quest'
SLCelement <- '5531'

preppriceKey = DatasetKey(
  domain = domainPP,
  dataset = datasetPrep,
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = GetCodeList(domainPP, datasetPrep, 'geographicAreaM49')[, code]),
    Dimension(name = "measuredElement", 
              keys = GetCodeList(domainPP, datasetPrep, 'measuredElement')[code == SLCelement, code]),
    Dimension(name = "measuredItemCPC",
              keys = GetCodeList(domainPP, datasetPrep, 'measuredItemCPC')[, code]),
    Dimension(name = "timePointYears", 
              keys = GetCodeList(domainPP, datasetPrep, 'timePointYears')[, code]))
  
)

impYear <- 2018#max(prep_price0$timePointYears)
prep_price0 <- GetData(preppriceKey, flags = TRUE)

# Impute last three years, i.e. delete previous imputations (or only last year?)
prep_price0[!flagObservationStatus %in% c('', 'X') & 
              timePointYears %in% as.character((impYear-2):impYear),
            c('Value', 'flagObservationStatus', 'flagMethod') := 
                list(NA, 'M', 'u')]

# Get space for imputations
prep_price <- expandYear(prep_price0, newYears = impYear)
# If one year not in the selected ones is missing the system ignore them
prep_price <- prep_price[!prep_price[!timePointYears %in% as.character((impYear-2):impYear) & 
             flagMethod == 'u'], on = names(prep_price)]

# -- Auxiliary variable ----

# Macro Indicators
GDP_VAcode <- c('8005', '8028') # GDP and VA codes

MIKey = DatasetKey(
  domain = 'macro_stats',
  dataset = 'ess_eco_macroind_complete',
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = GetCodeList('macro_stats', 
                                 'ess_eco_macroind_complete', 
                                 'geographicAreaM49')[, code]),
    Dimension(name = "measuredElementGdp", 
              keys = GetCodeList('macro_stats', 
                                 'ess_eco_macroind_complete', 
                                 'measuredElementGdp')[code %in% GDP_VAcode, code]),
    Dimension(name = "timePointYears", 
              keys = GetCodeList('macro_stats', 
                                 'ess_eco_macroind_complete', 
                                 'timePointYears')[code %in% unique(prep_price$timePointYears), code]))
  )

macro_ind <- GetData(MIKey, flags = TRUE)
# Get 2 separated columns
macro_ind <- dcast(macro_ind, geographicAreaM49 + timePointYears ~ measuredElementGdp, 
                   value.var = 'Value' )

setnames(macro_ind, c('8005', '8028'), c('ValueGDP', 'ValueVA'))

# GDP <- macro_ind[,.(geographicAreaM49, timePointYears, ValueGDP)]
# VA <- macro_ind[,.(geographicAreaM49, timePointYears, ValueVA)]
# 
# GDP[!is.na(ValueGDP) , LogGDP := log(ValueGDP)]
# VA[!is.na(ValueVA) , LogVA := log(ValueVA)]

prep12 <- merge(prep_price, macro_ind, by = c("geographicAreaM49","timePointYears"),
                all.x = T)

prep12[!is.na(ValueGDP) , LogGDP := log(ValueGDP)]
prep12[!is.na(ValueVA) , LogVA := log(ValueVA)]


# Yield
yieldcode <- '5421'

yieldKey = DatasetKey(
  domain = 'agriculture',
  dataset = 'aproduction',
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = GetCodeList('agriculture', 
                                 'aproduction', 
                                 'geographicAreaM49')[, code]),
    Dimension(name = "measuredElement", 
              keys = GetCodeList('agriculture', 
                                 'aproduction', 
                                 'measuredElement')[code == yieldcode, code]),
    Dimension(name = "measuredItemCPC",
              keys = GetCodeList('agriculture', 'aproduction', 'measuredItemCPC')[, code]),
    Dimension(name = "timePointYears", 
              keys = GetCodeList('agriculture', 
                                 'aproduction', 
                                 'timePointYears')[code %in% unique(prep_price$timePointYears), code]))
)

yield <- GetData(yieldKey, flags = TRUE)
setnames(yield, 'Value', 'ValueYield')
yield[, c("measuredElement", "flagObservationStatus", "flagMethod")] <- NULL

prep123 <- merge(prep12, yield, 
                 by = c("geographicAreaM49", 
                        "timePointYears",
                        "measuredItemCPC"), all.x = T)

# TOI

toi <- ReadDatatable('toi_data')

prepcov <- merge(prep123, toi, by.x = c("geographicAreaM49", 
                                        "timePointYears"),
                 by.y = c("geographicaream49", 
                          "timepointyears"), all.x = T)




# -- Missing value ----
prepcov <- prepcov[order(timePointYears)]
prepcov[!is.na(Value), LogValue := log(Value)]

# series with missing data

geocommmiss <- unique(prepcov[flagObservationStatus == 'M' & flagMethod == 'u', .(geographicAreaM49, measuredItemCPC)])


# -- Applying Price ratios (Value_PR) ----

tcf <- ReadDatatable('pp_tcf')
setnames(tcf, c("country_code", "cpc2convert"), 
         c('geographicAreaM49', 'measuredItemCPC'))

prepcov <- merge(prepcov, tcf[,c('geographicAreaM49', 'measuredItemCPC',
                      "cpc_reference", 'tcf'), with = F],
      by = c('geographicAreaM49', 'measuredItemCPC'), all.x = T)

tcfneed <- prepcov[!is.na(tcf) & is.na(Value) & flagObservationStatus == 'M' & flagMethod == 'u']
refneed <- unique(tcfneed[,.(geographicAreaM49, cpc_reference, timePointYears)])
refneed <- merge(refneed, prepcov[,.(geographicAreaM49,
                                     measuredItemCPC,
                                     timePointYears, Value)], 
                 by.x = c('geographicAreaM49', 'cpc_reference', 'timePointYears'), 
                 by.y = c('geographicAreaM49', 'measuredItemCPC', 'timePointYears'),
                 all.x = T)
refneed <- refneed[!is.na(Value)]
convprices <- merge(prepcov, refneed, 
                    by =  c('geographicAreaM49', 'cpc_reference', 'timePointYears'),
                    all.x = T, suffixes = c('', '_ref'))

# convprices[is.na(Value) & !is.na(tcf) & !is.na(Value_ref) & flagObservationStatus == 'M' & flagMethod == 'u', 
#            c('Value_pr', 'flagObservationStatus', 'flagMethod',
#              'LogValue') := list(tcf*Value_ref, 'I', 'i', log(tcf*Value_ref))]

convprices[is.na(Value) & 
             !is.na(tcf) & 
             !is.na(Value_ref) & 
             flagObservationStatus == 'M' & flagMethod == 'u', 
           PriceRatio := tcf*Value_ref]

convprices[,c("cpc_reference", "tcf", "Value_ref")] <- NULL

prepcov <- convprices

# -- Start loop ----

fulldata <- data.table()


geo = '100'
selComm = '0111'




for(geo in unique(geocommmiss$geographicAreaM49)){
  series_geo <- prepcov[geographicAreaM49 == geo]
  
  #xreg_geo <- unique(xregdata[geographicAreaM49 == geo])
  
  
  for(selComm in unique(geocommmiss$measuredItemCPC)){
    
  series_comm <- series_geo[measuredItemCPC == selComm]
  series_comm <- series_comm[,colSums(is.na(series_comm))<nrow(series_comm), with = F]
    
 # series_comm <- series_comm0 # series_comm0[!is.na(LogValue)]
  series_comm_lm <- copy(series_comm)
  series_comm_lm$timePointYears <- as.numeric(series_comm_lm$timePointYears)
  xreg_comm <- series_comm[,c("timePointYears", "geographicAreaM49", "measuredItemCPC",    
                                "LogGDP", "LogVA", "ValueYield", "toi_aff"), with = F] # , "TOI_AFF"
    
    # xreg_comm <- series_comm[,c("timePointYears", "geographicAreaM49", "measuredItemCPC",    
    #                        "LogGDP", "LogVA", "ValueYield", "toi_aff"), with = F] # , "TOI_AFF"
    
    # xreg_comm <- xreg_geo[measuredItemCPC == selComm & 
    #                         timePointYears %in% unique(series_comm$timePointYears)]
    
    xreg_comm <- xreg_comm[,colSums(is.na(xreg_comm))<nrow(xreg_comm), with = F]
    xreg_commsig <- copy(xreg_comm)
    
    # xreg_comm <- xreg_comm0 # xreg_comm[,colSums(is.na(xreg_comm))<nrow(xreg_comm), with = F]
    # xreg_commsig <- xreg_commsig0 # copy(xreg_comm)
    
    tryCatch({
      # -- Variable selection ----
      
    ppseries <- ts(series_comm$LogValue, start = min(as.numeric(series_comm$timePointYears)))
      
      # dfcomb <- list()
      # 
      # # Exclude Year, country and commodity (columns 1:3)
      # xreg_comm <- xreg_comm[ , colSums(is.na(xreg_comm)) == 0, with = F]
      # xreg_commsig <- copy(xreg_comm)
      # 
      # vec2comb <- 4:ncol(xreg_comm)
      # 
      # 
      # for(h in 1:length(vec2comb)){
      #   dfcomb[[h]] <- combn(vec2comb, h)
      #   
      # }
      
    # Covariates available
    vec2comb <- names(xreg_comm)[4:ncol(xreg_comm)]
    
    # Create a list with possible covariate combinations
    dfcomb <- list()
    
    for(h in 1:length(vec2comb)){
      dfcomb[[h]] <- as.data.frame(t(combn(vec2comb, h)))
    }
    
    covcomb <- rbindlist(dfcomb, use.names = T, fill = T)  
    
    
      # modlist <- list()
      # lmlist <- list()
      # ncolj <- 0
      # for( j in 1:length(dfcomb)){
      #   
      #   combj <- dfcomb[[j]]
      #   ncolj <- ncolj + ncol(combj)
      #   for(k in 1:ncol(combj)){
      #     modvar1 <- unique(c(1,2,3, vec2comb[!vec2comb %in% combj[,k]]))
      #     xreg2use <- xreg_comm[ , -modvar1, with = F]
      #     modlist[[((ncolj - ncol(combj)) + k)]] <- auto.arima(ppseries, seasonal = FALSE, xreg = xreg2use)
      #     formula <- as.formula(paste('LogValue ~ timePointYears + ',
      #                                 paste(names(xreg2use), collapse = ' + '), collapse = ''))
      #     lmlist[[((ncolj - ncol(combj)) + k)]] <- lm(formula = formula, data = series_comm_lm)
      #     print(((ncolj - ncol(combj)) + k))
      #   }
      #   
      #   
      # }
      # rm(ncolj)
      # 
      # modcomp <- data.table()
      # lmmodcomp <- data.table()
      # 
      # for(mods in 1:length(modlist)){
      #   lmmodcomp <- rbind(lmmodcomp, 
      #                    data.table(Vars = paste(names(lmlist[[mods]]$coefficients), 
      #                                            collapse = ' '),
      #                               R2 = summary(lmlist[[mods]])$r.squared))
      #   
      #   modcomp <- rbind(modcomp, 
      #                      data.table(Vars = paste(colnames(modlist[[mods]]$xreg), 
      #                                              collapse = ' '),
      #                                 AIC = modlist[[mods]]$aic))
      #   
      # }
      # 
      # 
      # varsel <- unlist(strsplit(modcomp[AIC == min(AIC),]$Vars, ' '))
      # varsel <- varsel[varsel != 'drift']
      # 
      # varsellm <- unlist(strsplit(lmmodcomp[R2 == max(R2),]$Vars, ' '))
      # varsellm <- varsellm[varsellm != "(Intercept)"]
      # 
      # #xreg_commsig <- xreg_commsig[,colSums(is.na(xreg_commsig))<nrow(xreg_commsig), with = F]
      # 
      # xreg_commsig <- xreg_commsig[,varsel, with = F]
      # 
      # mod1 <- auto.arima(ppseries, seasonal = FALSE, xreg = xreg_commsig)
      # mod2 <- auto.arima(ppseries, seasonal = FALSE, xreg = xreg_comm[ ,-(1:3), with = F])
      # 
      # if(mod1$aic < mod2$aic){
      #   mod <- mod1 
      #   newxreg1 <- xreg_commsig
      # } else{ 
      #   mod <- mod2
      #   newxreg1 <- xreg_comm[ ,-(1:3), with = F] }
    
    # Models using the possible combinations in covcomb
    modlist <- list()
    lmlist <- list()
    
    for(j in 1:nrow(covcomb)){
      cov2use <- as.matrix(covcomb[j,])
      xreg2use <- xreg_comm[ , cov2use[!is.na(cov2use)], with = F]
      modj <- auto.arima(ppseries, seasonal = FALSE, xreg = xreg2use)
      
      modlist[[j]] <- modj
      
      formula <- as.formula(paste('LogValue ~ timePointYears + ',
                                  paste(names(xreg2use), collapse = ' + '), collapse = ''))
      lmlist[[j]] <- lm(formula = formula, data = series_comm_lm)
    }
    
    # Best ARIMAX with lower AIC
    bestmod <- modlist[[which(unlist(lapply(modlist, function(x){x$aic})) ==
                                min(unlist(lapply(modlist, function(x){x$aic}))))]]
    
    # Best LM with highest Rsquared
    R2 <- unlist(lapply(lmlist, function(x){summary(x)$r.squared}))
    bestlm <- lmlist[[which(unlist(lapply(lmlist, function(x){summary(x)$r.squared})) == 
                              max(R2[R2<1]))]]
    
      
      nh <- impYear - max(as.numeric(series_comm[!is.na(LogValue)]$timePointYears))
      
      #-- ARIMA forescast ----
      # First option
      # Covariates for predictive years
      varselected <- names(bestmod$coef)[names(bestmod$coef) %in% names(xreg_comm)]
      xreg_comm_pred <- xreg_commsig[(nrow(xreg_commsig)-nh+1):nrow(xreg_commsig),
                                varselected, with = F]
      
      pred <- forecast(bestmod, h = nh, xreg = xreg_comm_pred)$mean
      
      #-- Linear model ----
      series_comm_lm_pred <- copy(series_comm_lm)
      series_comm_lm_pred <- series_comm_lm_pred[(nrow(series_comm)-nh+1):nrow(series_comm)]
      series_comm_lm_pred$timePointYears <- as.numeric(series_comm_lm_pred$timePointYears)
      
      predlm <- forecast(bestlm, h = nh, series_comm_lm_pred)$mean
      
      #-- Kalman ----
      
      predKalm <- na.kalman(x = ppseries, model = 'auto.arima', xreg = xreg_commsig[, varselected, with = F])
      #pred1bb <- na.kalman(x = ppseries, model = 'auto.arima', xreg = 
      #                      xreg_comm[ ,-c(1:3), with = F])
      
      # -- Ensemble approach ----
      impPar <- defaultImputationParameters()
      impPar$imputationValueColumn="Value"
      impPar$imputationFlagColumn="flagObservationStatus"
      impPar$imputationMethodColumn="flagMethod"
      impPar$byKey=c("geographicAreaM49", "measuredItemCPC")
      impPar$estimateNoData=FALSE
      
      # If the data series contains only zero and missing value then it is considered to contain no information for imputation.
      
      pp_ensemble <- removeNoInfo(series_comm,
                                  value="Value",
                                  observationFlag = "flagObservationStatus",
                                  byKey = c(impPar$byKey))
      names(pp_ensemble)
      pp_ensemble_sub <- pp_ensemble[,c("geographicAreaM49",
                                        "timePointYears",
                                        "measuredItemCPC",
                                        "Value",
                                        "flagObservationStatus",
                                        "flagMethod"), with = F]
      
      # If no missing data the commodityDB does not change
      pp_ensemble_imp <- imputeVariable(data = pp_ensemble_sub,
                                        imputationParameters = impPar)
      
      predEns <- pp_ensemble_imp$Value
      
      # -- Commodity group information ----
      
      cpchierarchy <- ReadDatatable('cpc_hierarchy_ebx5')
      
      hierarchyComm <- cpchierarchy[apply(cpchierarchy, 1, function(r) any(r %in% selComm))]
      
      colComm <-  names(hierarchyComm)[
        names(hierarchyComm)== 
          names(hierarchyComm)[(apply(cpchierarchy, 2, 
                                      function(r) any(r %in% selComm)))]]
      
      if(colComm == "code_l4"){
        hier1 <- unique(hierarchyComm[ , "code_l3", with = F]) 
        codes2compare <- c(cpchierarchy[code_l3 == hier1$code_l3 & !is.na(code_l4), ]$code_l4,
                           cpchierarchy[code_l3 == hier1$code_l3 & !is.na(code_l5), ]$code_l5,
                           cpchierarchy[code_l3 == hier1$code_l3 & !is.na(code_l6), ]$code_l6)
        
        
      
        } else {
        hier1 <- unique(hierarchyComm[ , "code_l4", with = F])
      codes2compare <- c(cpchierarchy[code_l4 == hier1$code_l4 & !is.na(code_l5), ]$code_l5,
                          cpchierarchy[code_l4 == hier1$code_l4 & !is.na(code_l6), ]$code_l6)
      }
      
      commoditygroup <- series_geo[!is.na(Value) &
                                     measuredItemCPC %in% codes2compare]
      
      # If there are data ok otherwise hierarchy up (if code_l4)
      if(nrow(commoditygroup[timePointYears %in% 
                             as.character((impYear-2):impYear)]) == 0
         & names(hier1) == 'colde_l4'){
        
        hier1 <- unique(hierarchyComm[ , "code_l3", with = F]) 
        codes2compare <- c(cpchierarchy[code_l3 == hier1$code_l3 & !is.na(code_l4), ]$code_l4,
                           cpchierarchy[code_l3 == hier1$code_l3 & !is.na(code_l5), ]$code_l5,
                           cpchierarchy[code_l3 == hier1$code_l3 & !is.na(code_l6), ]$code_l6)
        
        commoditygroup <- series_geo[!is.na(Value) &
                                       measuredItemCPC %in% codes2compare]
        
      }else if(nrow(commoditygroup[timePointYears %in% 
                                   as.character((impYear-2):impYear)]) == 0
               & names(hier1) == 'colde_l3'){
      print('Commodity group approch not applicable') #Should be an NA in the final file just to have all results 
      }
      
      # calculate growth rate and the mean or median applied to product
      commoditygroup$timePointYears <- as.numeric(commoditygroup$timePointYears)
      commoditygroup <- commoditygroup[order(timePointYears)]
     # commoditygroup[, cgGR := diff(Value)/shift(Value)]
      
      commoditygroup[ , cgGR := c(NA, diff(Value))/shift(Value),
                      by = c("geographicAreaM49",
                             "measuredElement",
                             "measuredItemCPC")]
      
      commoditygroup[ , c('meanbyyear',
                          'medianbyyear'):= list(mean(cgGR, na.rm = T),
                                                 median(cgGR, na.rm = T)),
                      by = 'timePointYears']

      commoditygroup2merge <- unique(commoditygroup[,.(timePointYears, medianbyyear)])
      commoditygroup2merge$timePointYears <- as.character(commoditygroup2merge$timePointYears)
      
      years <- series_comm[flagMethod == 'u']$timePointYears
      cgmethod <- copy(series_comm)
      
      cgmethod <- merge(cgmethod, commoditygroup2merge,
                        by = 'timePointYears', all.x = T)
      cgmethod <- cgmethod[order(timePointYears)]
      cgmethod[, ValueCG := shift(Value)*(1+medianbyyear)]
      cgmethod[is.na(Value), Value := ValueCG]               
      
      # -- CPI ----
      
      cpiKey = DatasetKey(
        domain = domainPP,
        dataset = 'consumer_price_indices',
        dimensions = list(
          Dimension(name = "geographicAreaM49",
                    keys = GetCodeList(domainPP, 'consumer_price_indices', 'geographicAreaM49')[, code]),
          Dimension(name = "measuredElement", 
                    keys = GetCodeList(domainPP, 'consumer_price_indices', 'measuredElement')[code %in% c('23012', '23013'), code]),
          Dimension(name = "timePointYears",
                    keys = GetCodeList(domainPP, 'consumer_price_indices', 'timePointYears')[, code]),
          Dimension(name = "timePointMonths", 
                    keys = GetCodeList(domainPP, 'consumer_price_indices', 'timePointMonths')[code == '7013', code]))
      )
      
      cpi0 <- GetData(cpiKey, flags = TRUE)
      cpi0[, timePointMonths := NULL]
      cpi0 <- cpi0[order(as.numeric(timePointYears))]
      cpi0[ , GR := c(NA, diff(Value))/shift(Value),
            by = c('geographicAreaM49', 'measuredElement')]
      cpi <- dcast(cpi0, geographicAreaM49 + timePointYears ~ measuredElement, value.var = 'GR')
      setnames(cpi, c("23012",  "23013"), c("GeneralCPI", "FoodCPI"))
      
      cpi[!is.na(FoodCPI) , GR2use := FoodCPI]
      cpi[is.na(FoodCPI) , GR2use := GeneralCPI]
      #cpi <- cpi[timePointYears != '2000']
      
      cpimethod <- copy(series_comm)
      
      cpimethod <- merge(cpimethod, cpi[,.(geographicAreaM49, 
                                           timePointYears,
                                           GR2use)],
                         by = c('geographicAreaM49', 'timePointYears'), all.x = T)
      cpimethod[order(timePointYears)]
      cpimethod[, ValueCPI := shift(Value, n = 2L)*(1+GR2use)]
      cpimethod[is.na(Value), Value := ValueCPI]      
      
      # -- End of imputation methods ----
      
      newimputation <- series_comm[, names(prep_price), with = F]
      newimputation[ , c('ARIMAX',
                         'Kalman', 
                         'Ensemble',
                         'LM',
                         'Comm_Group',
                         'CPI',
                         'PriceRatio') := list(c(Value[!is.na(Value)], exp(pred)),
                                             exp(predKalm),
                                             predEns,
                                             c(Value[!is.na(Value)], exp(predlm)),
                                             cgmethod$Value,
                                             cpimethod$Value,
                                             convprices[geographicAreaM49 == geo &
                                                          measuredItemCPC == selComm]$PriceRatio)] 

      # newimputation[ is.na(Value), 
      #                c('Value', 'flagObservationStatus', 'flagMethod') := 
      #                  list(Imputed, 'P', 'e')]
      
      fulldata <- rbind(fulldata, newimputation[flagMethod == 'u'])
        
      # ggplot()+
      #  geom_line(data = fulldata, aes(timePointYears, Predicted, group = 1, col = 'Predicted'))+
      #   geom_point(data = fulldata, aes(timePointYears, Predicted, col = 'Predicted'))+
      #   geom_line(data = fulldata, aes(timePointYears, Kalman, group = 1, col = 'Kalman'))+
      #   geom_point(data = fulldata, aes(timePointYears, Kalman, col = 'Kalman'))+
      #   geom_line(data = fulldata, aes(timePointYears, Ensemble, group = 1, col = 'Ensemble'))+
      #   geom_point(data = fulldata, aes(timePointYears, Ensemble, col = 'Ensemble'))+
      #   geom_line(data = fulldata, aes(timePointYears, CommGroup, group = 1, col = 'Comm group'))+
      #   geom_point(data = fulldata, aes(timePointYears, CommGroup, col = 'Comm group'))+
      #   geom_line(data = fulldata, aes(timePointYears, CPI, group = 1, col = 'CPI'))+
      #   geom_point(data = fulldata, aes(timePointYears, CPI, col = 'CPI'))+
      #   geom_line(data = fulldata, aes(timePointYears, Value, group = 1, col = 'Original series'))+
      #   geom_point(data = fulldata, aes(timePointYears, Value, col = 'Original series'))+
      #   ggtitle('Bulgaria, wheat series')
      #   
      
    
    }, error=function(e){})
    
    print(geo)
    
  }
  
}

imp2save <- fulldata
imp2save <- melt(imp2save,
                 id.vars = 1:7,
                 measure.vars = 8:ncol(imp2save),
                 variable.name = 'approach',
                 value.name = 'estimation')
names(imp2save) <- tolower(names(imp2save))

changeset <- Changeset('imputed_annual_prices')
AddInsertions(changeset, imp2save)
Finalize(changeset)

# SaveData(domainPP, 'annual_producer_prices_validation')
# Stima fatta ora è da salvare e da revisionare in shiny 
# come salvare più valori in SWS? Più anni? mettere in metadati modello
# ne andrebbe scelto uno di valore... da chiedere
