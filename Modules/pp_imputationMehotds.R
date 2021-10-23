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
                     token = SETTINGS[["token"]])#'4e9d9a2e-5258-48b6-974f-3656d1af8217')
}

# -- Expand Year ----
message('PP_imputationMethods: initiate plugin')

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
countryPar <- swsContext.computationParams$countries

message('PP_imputationMethods: get data')

domainPP <- 'prod_prices'
#datasetVal <- 'annual_producer_prices_validation'
datasetPrep <- 'annual_producer_prices_prep' 
SLCelement <- '5531'


if(!is.null(countryPar) & length(countryPar) > 0){
  countryPar <- swsContext.computationParams$countries
  sessionCountry <- strsplit(countryPar, ', ')[[1]]

  } else {
  sessionCountry <- swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys
  countries <- GetCodeList(domainPP, datasetPrep, "geographicAreaM49")[ type == 'country']$code
  # Make sure only countries not areas
  sessionCountry <- sessionCountry[sessionCountry %in% countries]
}

message(paste("Prod Prices: countries selected ", paste0(sessionCountry, collapse = ', '), '.', sep = ''))


lastyear <- as.character(as.numeric(format(Sys.Date(), '%Y')))

preppriceKey = DatasetKey(
  domain = domainPP,
  dataset = datasetPrep,
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = sessionCountry),
    Dimension(name = "measuredElement", 
              keys = SLCelement),
    Dimension(name = "measuredItemCPC",
              keys = GetCodeList('agriculture', 'aproduction', 'measuredItemCPC')[, code]),
    Dimension(name = "timePointYears", 
              keys = GetCodeList(domainPP, datasetPrep, 'timePointYears')[code > as.character(as.numeric(lastyear)-21), code]))
  
)

prep_price0 <- GetData(preppriceKey, flags = TRUE)
impYear <- max(as.numeric(unique(prep_price0$timePointYears)))

sessionCountry <- unique(prep_price0$geographicAreaM49)

# Impute last three years, i.e. delete previous imputations (timePointYears %in% as.character((impYear-2):impYear))
# or only last year (timePointYears == impYear)

# prep_price0[!flagObservationStatus %in% c('', 'X') & # NO DELETION OF PREVIOUS IMPUTATIONS!!!
#               timePointYears == impYear,
#             c('Value', 'flagObservationStatus', 'flagMethod') := 
#               list(NA, 'M', 'u')]

# Get space for imputations
prep_price <- expandYear(prep_price0)#, newYears = impYear)

# If one year not in the selected ones is missing the system ignore them
prep_price <- prep_price[!prep_price[timePointYears < impYear-3 & 
                                       flagMethod == 'u'], on = c("geographicAreaM49",
                                                                      "measuredItemCPC", "timePointYears")]

prep_price <- prep_price[!prep_price[timePointYears == impYear-3 & 
             flagMethod == 'u'], on = c("geographicAreaM49",
                                        "measuredItemCPC")]


#---- Interpolation ----

message('PP_imputationMethods: start interpolation')

interpolated <- data.table()

prep_price_int <- copy(prep_price)
#prepcov[timePointYears == impYear & flagObservationStatus == '']
#Check data with offcial last year figures
checkinterp <- unique(prep_price_int[timePointYears == impYear & flagObservationStatus %in% c('', 'X'), .(geographicAreaM49, measuredItemCPC)])

interDTcheck <- prep_price_int[prep_price_int[checkinterp, on = c('geographicAreaM49', 'measuredItemCPC'), which=TRUE]]

# Previous year must be imputed otherwise no interpolation
checkingDT <- interDTcheck[timePointYears == as.character(as.numeric(impYear)-1) &
                             !flagObservationStatus %in% c('', 'X')]
tochange <- interDTcheck[interDTcheck[checkingDT, on  = c('geographicAreaM49', 'measuredItemCPC'), which = TRUE]]

if(tochange[,.N] > 0){
  
  # Last official value as minimum start year for interpolation
  tochange[timePointYears != impYear & flagObservationStatus %in% c('', 'X'), 
           minyear := max(timePointYears), by =  c('geographicAreaM49', 'measuredItemCPC')]
  # Assign minyear value also to last year
  # tochange[timePointYears == impYear, minyear := max(timePointYears)]
  # Only consider series where maximum three years need to be interpolated
  tochange <- tochange[ minyear >= (impYear-4)]
  
  geocominterp <- unique(tochange[,.(geographicAreaM49, measuredItemCPC)])
  
  # Take data only for these combinations
  data2interp <- merge(prep_price_int, geocominterp, by = c('geographicAreaM49', 'measuredItemCPC'))
  data2interp[, valueinterp := Value]
  
  for(i in 1:nrow(geocominterp)){
    data2interp[geographicAreaM49 == geocominterp[i,]$geographicAreaM49 &
                  measuredItemCPC == geocominterp[i,]$measuredItemCPC, valueinterp := na.approx(valueinterp), ]
  }
  
  data2interp[flagObservationStatus == 'M', c('flagObservationStatus','flagMethod') := list('I', 'e')]
  
  #data2interp[, LogValueInterp := LogValue]
  #data2interp[!flagObservationStatus %in% c('', 'X') & timePointYears >= (impYear-4), LogValueInterp := NA]
  
  # for(geoint in unique(geocominterp$geographicAreaM49)){
  #   series_geoint <- data2interp[geographicAreaM49 == geoint]
  #   
  #   for(comint in unique(geocominterp[geographicAreaM49 == geoint]$measuredItemCPC)){
  #     
  #     series_comint <- series_geoint[measuredItemCPC == comint]
  #     series_comint <- series_comint[,colSums(is.na(series_comint))< nrow(series_comint), with = F]
  #     series_comint <- series_comint[,apply(series_comint,2,function(series_comint) !all(series_comint==0)), with = F]
  #     
  #     xreg_comint <- series_comint[,names(series_comint) %in% c("timePointYears", "geographicAreaM49", "measuredItemCPC",    
  #                                                               "LogGDP", "LogVA", "ValueYield", "toi_aff"), with = F] # , "TOI_AFF"
  #     
  #     xreg_comint <- xreg_comint[,colSums(is.na(xreg_comint))<nrow(xreg_comint), with = F]
  #     xreg_comintsig <- copy(xreg_comint)
  #     
  #     # -- Variable selection ----
  #     
  #     ppseries <- ts(series_comint$LogValueInterp, start = min(as.numeric(series_comint$timePointYears)))
  #     
  #     # Covariates available
  #     vec2comb <- names(xreg_comint)[4:ncol(xreg_comint)]
  #     
  #     # Create a list with possible covariate combinations
  #     dfcomb <- list()
  #     
  #     for(h in 1:length(vec2comb)){
  #       dfcomb[[h]] <- as.data.frame(t(combn(vec2comb, h)))
  #     }
  #     
  #     covcomb <- rbindlist(dfcomb, use.names = T, fill = T)
  #     
  #     # Models using the possible combinations in covcomb
  #     modlist <- list()
  #     
  #     for(j in 1:nrow(covcomb)){
  #       cov2use <- as.matrix(covcomb[j,])
  #       xreg2use <- xreg_comint[ , cov2use[!is.na(cov2use)], with = F]
  #       modj <- auto.arima(ppseries, seasonal = FALSE, xreg = xreg2use)
  #       
  #       modlist[[j]] <- modj
  #       
  #     }
  #     
  #     # Best ARIMAX with lower BIC
  #     bestmod <- modlist[[which(unlist(lapply(modlist, function(x){x$bic})) ==
  #                                 min(unlist(lapply(modlist, function(x){x$bic}))))]]
  #     
  #     nh <- impYear - max(as.numeric(series_comint[!is.na(LogValueInterp)]$timePointYears))
  #     
  #     # #-- ARIMA forescast ----
  #     
  #     varselected <- names(bestmod$coef)[names(bestmod$coef) %in% names(xreg_comint)]
  #     
  #     #-- Kalman ----
  #     
  #     predKalm <- try(na.kalman(x = ppseries, model = bestmod$model,
  #                               xreg = xreg_comintsig[, varselected, with = F]))
  # 
  #     # If auto.arima does not work then splines
  #     if(inherits(predKalm, 'try-error') | all(arimaorder(bestmod) == 0) | predKalm[which(is.na(ppseries))] == 0 | 
  #        predKalm[which(is.na(ppseries))] > (median(ppseries, na.rm = T) + 2*sd(ppseries, na.rm = T)) | 
  #        predKalm[which(is.na(ppseries))] < (median(ppseries, na.rm = T) - 2*sd(ppseries, na.rm = T)) ){
  #       
  #       predKalm <- try(na.kalman(x = ppseries, model = 'auto.arima',
  #                                 xreg = xreg_comintsig[, varselected, with = F]))
  #       
  #       if(inherits(predKalm, 'try-error') | predKalm[which(is.na(ppseries))] == 0 | 
  #          predKalm[which(is.na(ppseries))] > (median(ppseries, na.rm = T) + 2*sd(ppseries, na.rm = T)) | 
  #          predKalm[which(is.na(ppseries))] < (median(ppseries, na.rm = T) - 2*sd(ppseries, na.rm = T))){
  #         # predKalm <- na.interp(x = ppseries)
  #         splinemod <- smooth.spline(x = series_comint[!is.na(LogValueInterp)]$timePointYears, 
  #                                    y = series_comint[!is.na(LogValueInterp)]$LogValueInterp, all.knots = T)
  #         
  #         predKalm <- series_comint[is.na(LogValueInterp),
  #                                   LogValueInterp := predict(splinemod,
  #                                                             as.numeric(series_comint$timePointYears))$y[which(is.na(ppseries))]]$LogValueInterp
  #       }
  #     }
  #     
  #     series_comint[, ValueInterp := exp(predKalm)]
  #     series2bind <- series_comint[, c("geographicAreaM49",
  #                                      "measuredItemCPC",
  #                                      "timePointYears",
  #                                      "Value",
  #                                      "flagObservationStatus",
  #                                      "flagMethod", 
  #                                      "ValueInterp"), with = F]
  #     setcolorder(series2bind, c("geographicAreaM49",
  #                                "measuredItemCPC",
  #                                "timePointYears",
  #                                "Value",
  #                                "flagObservationStatus",
  #                                "flagMethod", 
  #                                "ValueInterp"))
  #     series2bind[, ValueInterp := as.numeric(ValueInterp)]
  #     interpolated <- rbind(interpolated, series2bind)
  #     
  #   }
  # }
  
  interpolated <- copy(data2interp)
  interpolated[, measuredElement := NULL]
  names(interpolated) <- tolower(names(interpolated))
  setnames(interpolated, 'valueinterp', 'interpolation')
  interpolated[, selected := as.logical(0)]
  
  chngsint <- Changeset('interpolation_annual_prices')
  currentInt <- ReadDatatable('interpolation_annual_prices', readOnly = FALSE)
  
  if(nrow(currentInt[geographicaream49 %in% sessionCountry])>0){
    AddDeletions(chngsint,currentInt[geographicaream49 %in% sessionCountry])
    Finalize(chngsint)
  }
  AddInsertions(chngsint, interpolated)
  Finalize(chngsint)

  prep_price <- prep_price[!geocominterp, on = c('geographicAreaM49', 'measuredItemCPC')]
    
} else {
  
  chngsint <- Changeset('interpolation_annual_prices')
  currentInt <- ReadDatatable('interpolation_annual_prices', readOnly = FALSE)
  
  if(nrow(currentInt[geographicaream49 %in% sessionCountry])>0){
    AddDeletions(chngsint,currentInt[geographicaream49 %in% sessionCountry])
    Finalize(chngsint)
  } 

}


# -- Auxiliary variable ----

message('PP_imputationMethods: add covariates')

if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws1.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = 'bc82f137-dc0e-429d-99d8-5c71016083eb')
}


# Macro Indicators
GDP_VAcode <- c('8005', '8028') # GDP and VA codes

MIKey = DatasetKey(
  domain = 'macro_stats',
  dataset = 'ess_eco_macroind_complete',
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = GetCodeList('macro_stats', 
                                 'ess_eco_macroind_complete', 
                                 'geographicAreaM49')[code %in% sessionCountry, code]),
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

GDPapproach <- copy(macro_ind)
GDPapproach[!is.na(ValueVA), GDP2use := ValueVA]
GDPapproach[is.na(ValueVA), GDP2use := ValueGDP]
GDPapproach[,c('ValueGDP', 'ValueVA')] <- NULL

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
                                 'geographicAreaM49')[code %in% sessionCountry, code]),
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

if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])#'4e9d9a2e-5258-48b6-974f-3656d1af8217')
}

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

class(convprices$Value_ref)
convprices[is.na(Value) & 
             !is.na(tcf) & 
             !is.na(Value_ref) & 
             flagObservationStatus == 'M' & flagMethod == 'u', 
           PriceRatio := tcf*Value_ref]

convprices[,c("cpc_reference", "tcf", "Value_ref")] <- NULL

prepcov <- convprices

# -- Start loop ----

message('PP_imputationMethods: start imputation')

cpchierarchy <- ReadDatatable('cpc_hierarchy_ebx5')

fulldata <- data.table()

for(geo in unique(geocommmiss$geographicAreaM49)){
  series_geo <- prepcov[geographicAreaM49 == geo]
  
  for(selComm in unique(geocommmiss[geographicAreaM49 == geo]$measuredItemCPC)){
    
    series_comm <- series_geo[measuredItemCPC == selComm]
    series_comm <- series_comm[,colSums(is.na(series_comm))<nrow(series_comm), with = F]
    
    # For LM
    series_comm_lm <- copy(series_comm)
    series_comm_lm$timePointYears <- as.numeric(series_comm_lm$timePointYears)
    xreg_comm <- series_comm[,names(series_comm) %in% c("timePointYears", "geographicAreaM49", "measuredItemCPC",    
                                                        "LogGDP", "LogVA", "ValueYield", "toi_aff"), with = F] # , "TOI_AFF"
    
    xreg_comm <- xreg_comm[,colSums(is.na(xreg_comm))<nrow(xreg_comm), with = F]
    xreg_commsig <- copy(xreg_comm)
    
    
      # -- Variable selection ----
      
      ppseries <- ts(series_comm$LogValue, start = min(as.numeric(series_comm$timePointYears)))
      
      # Covariates available
      vec2comb <- names(xreg_comm)[4:ncol(xreg_comm)]
      
      # Create a list with possible covariate combinations
      dfcomb <- list()
      
      for(h in 1:length(vec2comb)){
        dfcomb[[h]] <- as.data.frame(t(combn(vec2comb, h)))
      }
      
      covcomb <- rbindlist(dfcomb, use.names = T, fill = T)  
      
      # Models using the possible combinations in covcomb
      modlist <- list()
      lmlist <- list()
      
      for(j in 1:nrow(covcomb)){
        cov2use <- as.matrix(covcomb[j,])
        xreg2use <- xreg_comm[ , cov2use[!is.na(cov2use)], with = F]
        modj <- tryCatch({auto.arima(ppseries, seasonal = FALSE, xreg = xreg2use)}, 
                         error = function(error_condition) { auto.arima(ppseries, seasonal = FALSE)})
        
        modlist[[j]] <- modj
        
        formula <- as.formula(paste('LogValue ~ timePointYears + ',
                                    paste(names(xreg2use), collapse = ' + '), collapse = ''))
        lmlist[[j]] <-  tryCatch({lm(formula = formula, data = series_comm_lm)},
                                 error = function(error_condition) { lm(formula = 'LogValue ~ timePointYears',
                                                                        data = series_comm_lm)})
      }
      
      # Best ARIMAX with lower BIC
      bestmod <- modlist[which(!lapply(modlist, function(x){x$bic}) %in% c(Inf, -Inf))]
      
      # Best LM with lowest BIC
      BICnotInf <- unlist(lapply(lmlist, function(x){BIC(x)}))
      BICnotInf <- BICnotInf[!BICnotInf %in% c(-Inf, Inf)]
      
      nh <- impYear - max(as.numeric(series_comm[!is.na(LogValue)]$timePointYears))

      #-- ARIMA forescast ----
 
      # Covariates for predictive years
      if(length(bestmod) > 0){
        bestmod <- modlist[[which(unlist(lapply(modlist, function(x){x$bic})) ==
                                    min(unlist(lapply(modlist, function(x){x$bic}))))[1]]]
        varselected <- names(bestmod$coef)[names(bestmod$coef) %in% names(xreg_comm)]
        xreg_comm_pred <- xreg_commsig[(nrow(xreg_commsig)-nh+1):nrow(xreg_commsig),
                                       varselected, with = F]
        pred <- forecast(bestmod, h = nh, xreg = xreg_comm_pred)$mean
      } else {
        pred <- NA
      }
     
      
      
      #-- Linear model ----
      series_comm_lm_pred <- copy(series_comm_lm)
      series_comm_lm_pred <- series_comm_lm_pred[(nrow(series_comm)-nh+1):nrow(series_comm)]
      series_comm_lm_pred$timePointYears <- as.numeric(series_comm_lm_pred$timePointYears)
      
      if(length(BICnotInf) > 0){
        bestlm <- lmlist[[which(unlist(lapply(lmlist, function(x){BIC(x)})) == 
                                  min(BICnotInf))[1]]]
        predlm <- forecast(bestlm, h = nh, series_comm_lm_pred)$mean
      } else {
        predlm <- rep(NA, nh)
      }

   
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
                             as.character(impYear)]) == 0
         & names(hier1) == 'code_l4'){
        
        hier1 <- unique(hierarchyComm[ , "code_l3", with = F]) 
        codes2compare <- c(cpchierarchy[code_l3 == hier1$code_l3 & !is.na(code_l4), ]$code_l4,
                           cpchierarchy[code_l3 == hier1$code_l3 & !is.na(code_l5), ]$code_l5,
                           cpchierarchy[code_l3 == hier1$code_l3 & !is.na(code_l6), ]$code_l6)
        
        commoditygroup <- series_geo[!is.na(Value) &
                                       measuredItemCPC %in% codes2compare]
        
      }else if(nrow(commoditygroup[timePointYears %in% 
                                   as.character((impYear-2):impYear)]) == 0
               & names(hier1) == 'code_l3'){
        print('Commodity group approch not applicable') #Should be an NA in the final file just to have all results 
      }
      
      # calculate growth rate and the mean or median applied to product
      commoditygroup$timePointYears <- as.numeric(commoditygroup$timePointYears)
      commoditygroup <- commoditygroup[order(timePointYears)]
      
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
                    keys = GetCodeList(domainPP, 'consumer_price_indices', 'geographicAreaM49')[code %in% sessionCountry, code]),
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
      
      cpimethod <- copy(series_comm)
      
      cpimethod <- merge(cpimethod, cpi[,.(geographicAreaM49, 
                                           timePointYears,
                                           GR2use)],
                         by = c('geographicAreaM49', 'timePointYears'), all.x = T)
      cpimethod[order(timePointYears)]
      cpimethod[, ValueCPI := shift(Value)*(1+shift(GR2use))]
      cpimethod[is.na(Value), Value := ValueCPI]      
      
      # -- GDP or AgGDP ----
      gdpmethod <- copy(series_comm)
      GDPapproach[ , GRgdp := c(NA, diff(GDP2use))/shift(GDP2use)]
      
      gdpmethod <- merge(gdpmethod, GDPapproach, 
                         by = c('geographicAreaM49', 'timePointYears'), 
                         all.x = T)
      
      gdpmethod[order(timePointYears)]
      gdpmethod[, ValueGDPdefl := shift(Value)*(1+GRgdp)]
      gdpmethod[is.na(Value), Value := ValueGDPdefl]  
      
      # -- AgPPI ----
      
      # -- End of imputation methods ----
      
      newimputation <- series_comm[, names(prep_price), with = F]
      newimputation[ , c('ARIMAX',
                         # 'Kalman', 
                         'Ensemble',
                         'LM',
                         'Comm_Group',
                         'CPI',
                         'PriceRatio',
                         'GDP') := list(c(Value[!is.na(Value)], exp(pred)),
                                        #    exp(predKalm),
                                        predEns,
                                        c(Value[!is.na(Value)], exp(predlm)),
                                        cgmethod$Value,
                                        cpimethod$Value,
                                        convprices[geographicAreaM49 == geo &
                                                     measuredItemCPC == selComm]$PriceRatio,
                                        gdpmethod$Value)] 
      
      fulldata <- rbind(fulldata, newimputation[flagMethod == 'u'])
      
    
    
    #print(selComm)
  }
  
}

message('PP_imputationMethods: save imputation')

imp2save <- copy(fulldata[,!names(fulldata) %in% c("measuredElement",
                                                   "Value",
                                                   "flagObservationStatus",
                                                   "flagMethod"), with = F])
imp2save <- melt(imp2save,
                 id.vars = c("geographicAreaM49",
                             "measuredItemCPC",
                             "timePointYears"),
                 measure.vars = 4:ncol(imp2save),
                 variable.name = 'approach',
                 value.name = 'estimation')
names(imp2save) <- tolower(names(imp2save))

changeset <- Changeset('imputation_annual_prices')
currentImp <- ReadDatatable('imputation_annual_prices', readOnly = FALSE)

if(nrow(currentImp[geographicaream49 %in% sessionCountry])>0){
  AddDeletions(changeset,currentImp[geographicaream49 %in% sessionCountry])
  Finalize(changeset)
}
AddInsertions(changeset, imp2save)
Finalize(changeset)



message('Producer prices imputation and interpolation plugin completed')


from = "sws@fao.org"
to = swsContext.userEmail
subject = "PP imputation and interpolation plug-in has correctly run"
body = list('The plugin has correctly run. You can check the results in the SWS datatable.')
sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)
paste0("Email sent to ", swsContext.userEmail)

