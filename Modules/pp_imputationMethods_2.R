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
  SETTINGS = ReadSettings("sws1.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])#'9436e20f-8f96-46bb-a10d-a075189ad3cb')#'04fe6b26-39af-4ce8-a609-bb6bbf2bb045')#'4e9d9a2e-5258-48b6-974f-3656d1af8217')
}

# -- Functions ----
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


computeEnsemble <- function(fits, weights, errors){
  stopifnot(all(names(fits) %in% names(weights)))
  stopifnot(all(names(weights) %in% names(fits)))
  fits = fits[names(weights)]
  stopifnot(all(names(weights) == names(fits)))
  stopifnot(length(fits) == ncol(weights))
  if (!all(sapply(fits, length) == nrow(weights))) 
    stop("Length of fits do not match nrow(weights)!")
  fitsMatrix = matrix(unlist(fits), ncol = length(fits))
  weightedFit = fitsMatrix * weights
  errorFit = errors * weights
  ensemble = data.table(fit = apply(weightedFit, 1, function(x) sum(x, 
                                                                    na.rm = !all(is.na(x)))), variance = apply(errorFit, 
                                                                                                               1, sum, na.rm = TRUE))
  
  
  fitsMatrix <- fitsMatrix[,!apply(fitsMatrix, 2, function(x) all(is.na(x)))]
  modelMin = apply(fitsMatrix, 2, min, na.rm = TRUE)
  if (any(modelMin < 0)) {
    negMod = which(modelMin < 0)
    stop("Imputation gave negative result")
  }
  ensemble
}

ensembleImpute <- function (data, imputationParameters){
  if (!exists("ensuredImputationData") || !ensuredImputationData) 
    ensureImputationInputs(data = data, imputationParameters = imputationParameters)
  valueMissingIndex = is.na(data[[imputationParameters$imputationValueColumn]])
  flagMissingIndex = (data[[imputationParameters$imputationFlagColumn]] == 
                        imputationParameters$missingFlag)
  if (!all(valueMissingIndex == flagMissingIndex)) {
    cat("Values that are NA: ", sum(valueMissingIndex), 
        "\n")
    cat("Flags with missingFlag value: ", sum(flagMissingIndex), 
        "\n")
    stop("Different missing values from flags/values!  Maybe call remove0M?")
  }
  if (is.null(names(imputationParameters$ensembleModels))) 
    names(imputationParameters$ensembleModels) = paste("Model", 
                                                       1:length(imputationParameters$ensembleModels), sep = "_")
  if (!any(is.na(data[[imputationParameters$imputationValueColumn]]))) {
    warning("No missing values in data[[imputationValueColumn]].", 
            "Returning data[[imputationValueColumn]]")
    return(data[[imputationParameters$imputationValueColumn]])
  }
  setkeyv(x = data, cols = c(imputationParameters$byKey, imputationParameters$yearValue))
  ensemble = data[[imputationParameters$imputationValueColumn]]
  missIndex = is.na(ensemble)
  cvGroup = makeCvGroup(data = data, imputationParameters = imputationParameters)
  modelFits = computeEnsembleFit(data = data, imputationParameters = imputationParameters)
  modelStats = computeEnsembleWeight(data = data, cvGroup = cvGroup, 
                                     fits = modelFits, imputationParameters = imputationParameters)
  modelWeights = modelStats[[1]]
  modelErrors = modelStats[[2]]
  
  ensembleFit = computeEnsemble(fits = modelFits, weights = modelWeights, 
                                errors = modelErrors)
  ensemble[missIndex] = ensembleFit[missIndex, fit]
  if (imputationParameters$plotImputation != "") {
    pl <- plotEnsemble(data = data, modelFits = modelFits, 
                       modelWeights = modelWeights, ensemble = ensemble, 
                       imputationParameters = imputationParameters, returnFormat = imputationParameters$plotImputation)
    pdf("Rplot%03d.pdf")
    lapply(pl, print)
    dev.off()
  }
  data.table(ensemble = ensemble)
}

imputeVariable0 <- function (data, imputationParameters) {
  if (!exists("ensuredImputationData") || !ensuredImputationData) 
    ensureImputationInputs(data = data, imputationParameters = imputationParameters)
  if (imputationParameters$newImputationColumn == "") {
    newValueColumn = imputationParameters$imputationValueColumn
    newObsFlagColumn = imputationParameters$imputationFlagColumn
    newMethodFlagColumn = imputationParameters$imputationMethodColumn
  }
  else {
    newValueColumn = paste0("Value_", imputationParameters$newImputationColumn)
    newObsFlagColumn = paste0("flagObservationStatus_", imputationParameters$newImputationColumn)
    newMethodFlagColumn = paste0("flagMethod_", imputationParameters$newImputationColumn)
  }
  imputeSingleObservation(data, imputationParameters)
  missingIndex = data[[imputationParameters$imputationFlagColumn]] == 
    "M" & data[[imputationParameters$imputationMethodColumn]] == 
    "u"
  ensemble = ensembleImpute(data = data, imputationParameters = imputationParameters)
  if (!is.null(nrow(ensemble))) {
    data = cbind(data, ensemble)
    data[missingIndex & !is.na(ensemble), `:=`(c(newValueColumn), 
                                               ensemble)]
    data = data[, `:=`(ensemble, NULL)]
  }
  imputedIndex = missingIndex & !is.na(data[[newValueColumn]])
  invisible(data[imputedIndex, `:=`(c(newObsFlagColumn, newMethodFlagColumn), 
                                    list(imputationParameters$imputationFlag, imputationParameters$newMethodFlag))])
}

arimaxAndlm <- function(series_comm, series_comm_lm, impYear, nh, missing, missingyear){
  
  if(max(missing) == min(missing)){
    xreg_comm <- xreg_comm
  } else {
    xreg_comm <- xreg_comm[ , c("timePointYears", "geographicAreaM49", "measuredItemCPC",    
                                names(missing[missing == min(missing)])), with = F ]
  }
  
  xreg_comm <- xreg_comm[,colSums(is.na(xreg_comm))<nrow(xreg_comm), with = F]
  xreg_commsig <- copy(xreg_comm)
  
  
  # Exclude columns with NAs
  xreg_comm <- xreg_comm[ , colSums(is.na(xreg_comm)) < nrow(series_comm), with = F]
  
  # Check if there are data before the data to impute
  if(xreg_comm[timePointYears < impYear,.N]== 0){
    xreg_comm
  } else {
    xreg_comm <- xreg_comm[timePointYears < impYear,]
  }
  
  # -- Variable selection ----
  
  ppseries <- ts(series_comm[!is.na(Value)]$LogValue, start = min(as.numeric(series_comm$timePointYears)))
  
  
  # Check there is at least one covariates
  if(length(names(xreg_comm)) > 3){
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
      xreg2use <- xreg_comm[timePointYears %in% time(ppseries) , cov2use[!is.na(cov2use)], with = F]
      modj <- tryCatch({auto.arima(ppseries, xreg = xreg2use)}, 
                       error = function(error_condition) { auto.arima(ppseries)})
      
      modlist[[j]] <- modj
      
      formula <- as.formula(paste('LogValue ~ timePointYears + ',
                                  paste(names(xreg2use), collapse = ' + '), collapse = ''))
      lmlist[[j]] <-  tryCatch({lm(formula = formula, data = series_comm_lm)},
                               error = function(error_condition) { lm(formula = 'LogValue ~ timePointYears',
                                                                      data = series_comm_lm)})
    }
    
    # Best ARIMAX with lowest BIC
    if(length(which(!lapply(modlist, function(x){x$bic}) %in% c(Inf, -Inf))) == 0){
      bestmod <- NULL
    } else {
      bestmod <- modlist[which(!lapply(modlist, function(x){x$bic}) %in% c(Inf, -Inf))][[1]]
    }
    
    bics <- unlist(lapply(modlist, function(x){x$bic}))
    mod2sel <- which(bics == min(bics))
    
    # Best LM with lowest BIC
    BICnotInf <- unlist(lapply(lmlist, function(x){BIC(x)}))
    BICnotInf <- BICnotInf[!BICnotInf %in% c(-Inf, Inf)]
    
    # nh <- impYear - max(as.numeric(series_comm[!is.na(LogValue)]$timePointYears))
    
    #-- ARIMA forescast ----
    
    # Covariates for predictive years
    if(length(bestmod) > 0){
      bestmod <- modlist[[mod2sel[1]]]
      varselected <- names(bestmod$coef)[names(bestmod$coef) %in% names(xreg_comm)]
      
      if(length(varselected) == 0){
        pred <- ts(forecast(bestmod, h = nh)$mean, start = min(as.numeric(missingyear)))
      } else {
        
        xreg_comm_pred <- series_comm[flagMethod == 'u' & timePointYears %in% impYear:min(as.numeric(missingyear)), 
                                      varselected, with = F]
        if(all(is.na(xreg_comm_pred))){
          pred <- ts(rep(NA, series_comm[flagMethod == 'u' & timePointYears %in% impYear:min(as.numeric(missingyear)),.N]), 
                     start = min(as.numeric(series_comm[flagMethod == 'u' & 
                                                          timePointYears %in% impYear:min(as.numeric(missingyear)),
                                                        ]$timePointYears)))
        } else {
          
          pred <- tryCatch({forecast(bestmod, h = nh, xreg = xreg_comm_pred)$mean},
                           error = function(error_condition) {ts(forecast(bestmod, h = nh)$mean, 
                                                                 start = min(as.numeric(missingyear)))})
          
        }
        
      }
      
    } else {
      pred <- ts(rep(NA, series_comm[flagMethod == 'u' & timePointYears %in% impYear:min(as.numeric(missingyear)),.N]), 
                 start = min(as.numeric(series_comm[flagMethod == 'u' & 
                                                      timePointYears %in% impYear:min(as.numeric(missingyear)),
                                                    ]$timePointYears)))
    }
    
  } else{
    
    pred <- ts(rep(NA, series_comm[flagMethod == 'u' & timePointYears %in% impYear:min(as.numeric(missingyear)),.N]), 
               start = min(as.numeric(series_comm[flagMethod == 'u' & 
                                                    timePointYears %in% impYear:min(as.numeric(missingyear)),
                                                  ]$timePointYears)))
    
    
  }
  
  pred2complete <- copy(series_comm) 
  
  pred2complete[timePointYears %in% time(pred), LogValue := pred] 
  
  pred <- exp(pred2complete$LogValue)
  
  
  print('ARIMAX ok')
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
  print('LM ok')
  
  # Use the same data for LM
  pred2completelm <- copy(series_comm) 
  pred2completelm[timePointYears %in% time(pred), LogValue := predlm]
  predlm <- exp(pred2completelm$LogValue)
  
  return(list(pred = pred, predlm = predlm))
  
}

applyEnsemble <- function(series_comm){ 
  
  impPar <- defaultImputationParameters()
  
  impPar$ensembleModels <-  impPar$ensembleModels[-10] # no defaultMixedModel
  
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
  
  if(pp_ensemble_sub[timePointYears < min(pp_ensemble_sub[flagObservationStatus == 'M']$timePointYears),.N] < 2 ){
    
    predEns <- series_comm$Value
  } else {
    # If no missing data the commodityDB does not change
    
    pp_ensemble_imp <- imputeVariable0(data = pp_ensemble_sub,
                                       imputationParameters = impPar)
    
    predEns <- pp_ensemble_imp$Value
  }
  
  return(predEns)
  
}

applyCG <- function(cpchierarchy, selComm, series_geo, series_comm, missingyear){
  
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
                               as.character((impYear-nyear2impute):impYear)]) == 0
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
  cgmethod[timePointYears %in% missingyear, Value := ValueCG]
  i = 0
  while( (cgmethod[timePointYears %in% missingyear & is.na(ValueCG) & !is.na(medianbyyear),.N] > 0 & 
          cgmethod[timePointYears < min(missingyear),.N] > 0) | (i < 50) ){
    cgmethod[!is.na(medianbyyear) , ValueCG := shift(Value)*(1+medianbyyear)]
    cgmethod[timePointYears %in% missingyear, Value := ValueCG]
    i = i+1
  }
  cgmethod[is.na(Value), Value := ValueCG]
  print('Commodity group ok')
  
  return(cgmethod)
  
}

applyCPI <- function(cpi, selComm, series_comm, missingyear){
  cpimethod <- copy(series_comm)
  cpimethod <- merge(cpimethod, cpi[,.(geographicAreaM49,
                                       timePointYears,
                                       GR2use)],
                     by = c('geographicAreaM49', 'timePointYears'), all.x = T)
  cpimethod[order(timePointYears)]
  cpimethod[, ValueCPI := shift(Value)*(1+shift(GR2use))]
  cpimethod[timePointYears %in% missingyear, Value := ValueCPI]
  i = 0
  while( (cpimethod[timePointYears %in% missingyear & is.na(ValueCPI) & !is.na(GR2use),.N] > 0 &
          cpimethod[timePointYears < min(missingyear),.N] > 0) | (i < 50) ){
    cpimethod[!is.na(GR2use) , ValueCPI := shift(Value)*(1+GR2use)]
    cpimethod[timePointYears %in% missingyear, Value := ValueCPI]
    i = i + 1
  }
  
  cpimethod[is.na(Value), Value := ValueCPI]
  print('CPI ok')
  
  return(cpimethod)
}

applyGDP <- function(GDPapproach, selComm, series_comm, missingyear){
  gdpmethod <- copy(series_comm)
  GDPapproach[ , GRgdp := c(NA, diff(GDP2use))/shift(GDP2use)]
  
  gdpmethod <- merge(gdpmethod, GDPapproach,
                     by = c('geographicAreaM49', 'timePointYears'),
                     all.x = T)
  
  gdpmethod[order(timePointYears)]
  gdpmethod[, ValueGDPdefl := shift(Value)*(1+GRgdp)]
  gdpmethod[timePointYears %in% missingyear, Value := ValueGDPdefl]
  i = 0
  while( (gdpmethod[timePointYears %in% missingyear & is.na(ValueGDPdefl) & !is.na(GRgdp),.N] > 0 &
          gdpmethod[timePointYears < min(missingyear),.N] > 0) | (i < 50) ){
    gdpmethod[!is.na(GRgdp) , ValueGDPdefl := shift(Value)*(1+GRgdp)]
    gdpmethod[timePointYears %in% missingyear, Value := ValueGDPdefl]
    i = i + 1
  }
  gdpmethod[is.na(Value), Value := ValueGDPdefl]
  print('GDP ok')
  return(gdpmethod)
}


# -- Pull preparation data SLC ----
countryPar <- swsContext.computationParams$countries

message('PP_imputationMethods: get data')

domainPP <- 'prod_prices'
#datasetVal <- 'annual_producer_prices_validation'
datasetPrep <- 'annual_producer_prices_prep' 
SLCelement <- c('5531') #, '5530', '5532')


if(!is.null(countryPar) & length(countryPar) > 0){
  countryPar <- swsContext.computationParams$countries
  sessionCountry <- strsplit(countryPar, ', ')[[1]]
  
} else {
  # sessionCountry <- swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys
  countries <- GetCodeList(domainPP, datasetPrep, "geographicAreaM49")[ type == 'country']$code
  # Make sure only countries not areas
  sessionCountry <- countries #sessionCountry[sessionCountry %in% countries]
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
              keys = GetCodeList(domainPP, datasetPrep, 'timePointYears')[code > as.character(1991), code])) #as.numeric(lastyear)-21
  
)

prep_price0 <- GetData(preppriceKey, flags = TRUE)
impYear <- 2020# swsContext.computationParams$last_imp_y #as.numeric(lastyear)-1 #max(as.numeric(unique(prep_price0$timePointYears)))

sessionCountry <- unique(prep_price0$geographicAreaM49)

# Impute last four years maximum
# Get space for imputations
#nyear2impute <- 10

prep_price0[, minYear := min(timePointYears), .(geographicAreaM49, measuredItemCPC)]

setkey(prep_price0)

statSeries2consider <- unique(prep_price0[,.(geographicAreaM49, measuredItemCPC, minYear)])

prep_price_exp <- expandYear(prep_price0[geographicAreaM49 %in% sessionCountry], # %in% impYear:(impYear-nyear2impute)], 
                             newYears = impYear)

prep_price_exp <- merge(prep_price_exp[, minYear :=NULL], statSeries2consider, c('geographicAreaM49', 'measuredItemCPC'),
                        all.x = T)

prep_price <- prep_price_exp[timePointYears >= minYear,] #rbind(prep_price_exp, prep_price0[!timePointYears %in% impYear:(impYear-nyear2impute)])


#---- Interpolation ----

message('PP_imputationMethods: start interpolation')

interpolated <- data.table()

prep_price_int <- copy(prep_price)

#Check data with offcial last year figures
checkinterp <- unique(prep_price_int[timePointYears == impYear & flagObservationStatus %in% c('', 'X'),
                                     .(geographicAreaM49, measuredItemCPC)])

interDTcheck <- prep_price_int[checkinterp, on = c('geographicAreaM49', 'measuredItemCPC')]

# Previous 4 years must be imputed otherwise no interpolation
setkey(interDTcheck)
checkingDT <- unique(interDTcheck[timePointYears %in% as.character((as.numeric(impYear)-1):(as.numeric(impYear)-nyear2impute)) &
                                    flagObservationStatus == c('M')][,.(geographicAreaM49, measuredItemCPC)])
tochange <- interDTcheck[checkingDT, on  = c('geographicAreaM49', 'measuredItemCPC')]

if(tochange[,.N] > 0){
  print('PP: series to interpolate selected')
  setkey(tochange)
  minYear2consider <- copy(tochange)
  
  minYear2consider <- unique(minYear2consider[flagObservationStatus == 'M',
                                              minMissingYear := min(as.numeric(timePointYears)),
                                              by  = c('geographicAreaM49', 'measuredItemCPC')]
                             
                             [!is.na(minMissingYear), 
                             .(geographicAreaM49,
                               measuredItemCPC, 
                               minMissingYear)])
  
  tochange <- merge(minYear2consider, tochange, by = c('geographicAreaM49', 'measuredItemCPC'), all = T)
  
  # Last official value as minimum start year for interpolation
  excludeNewProduct <- tochange[timePointYears <= minMissingYear-1, .N,  by =  c('geographicAreaM49', 'measuredItemCPC')]
  tochange <- tochange[excludeNewProduct, on =  c('geographicAreaM49', 'measuredItemCPC')]
  tochange <- tochange[timePointYears == minMissingYear-1 & flagObservationStatus %in% c('', 'X') ]
  
  # List of country commodity combination to impute
  setkey(tochange)
  geocominterp <- unique(tochange[,.(geographicAreaM49, measuredItemCPC)])
  print(geocominterp)
  # Take data only for these combinations
  data2interp <- merge(prep_price_int, geocominterp, by = c('geographicAreaM49', 'measuredItemCPC'))
  data2interp <- data2interp[order(timePointYears)]
  data2interp[, valueinterp := Value]
  
  for(i in 1:nrow(geocominterp)){
    data2interp[geographicAreaM49 == geocominterp[i,]$geographicAreaM49 &
                  measuredItemCPC == geocominterp[i,]$measuredItemCPC, valueinterp := na.approx(valueinterp)]
  }
  
  data2interp[flagObservationStatus == 'M', c('flagObservationStatus','flagMethod') := list('I', 'interpolation')]
  print('Data interpolated')
  interpolated <- copy(data2interp)
  interpolated[, measuredElement := NULL]
  names(interpolated) <- tolower(names(interpolated))
  setnames(interpolated, 'valueinterp', 'interpolation')
  interpolated$selected <- NA
  
  chngsint <- Changeset('interpolation_annual_prices')
  currentInt <- ReadDatatable('interpolation_annual_prices', readOnly = FALSE)
  print('Delete previous data')
  if(nrow(currentInt[geographicaream49 %in% sessionCountry])>0){
    AddDeletions(chngsint,currentInt[geographicaream49 %in% sessionCountry])
    Finalize(chngsint)
  }
  print('Add new data')
  AddInsertions(chngsint, interpolated)
  Finalize(chngsint)
  print(interpolated)
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

# CPI

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
if(any(sessionCountry %in% '156')){
  sel_country_yield <- c(sessionCountry, '1248')
} else { sel_country_yield <- sessionCountry}

yieldKey = DatasetKey(
  domain = 'agriculture',
  dataset = 'aproduction',
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = GetCodeList('agriculture', 
                                 'aproduction', 
                                 'geographicAreaM49')[code %in% c(sel_country_yield), code]),
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
if(yield[geographicAreaM49 == '156',.N] == 0){
  yield[geographicAreaM49 == '1248', geographicAreaM49 := '156']
  
}

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


prepcov <- prepcov[order(timePointYears)]
prepcov[!is.na(Value), LogValue := log(Value)]

# series with missing data
setkey(prepcov)
notimputedseries <- unique(prepcov[timePointYears < min(impYear - 3) , all(is.na(Value)), 
                                   .(geographicAreaM49, measuredItemCPC)][V1 == TRUE,
                                                                          .(geographicAreaM49, 
                                                                            measuredItemCPC)])
# Ignore series not imputable
#prepcov <- prepcov[!notimputedseries, on = c('geographicAreaM49', 'measuredItemCPC')]

# setnames(notimputedseries, names(notimputedseries), tolower(names(notimputedseries)))


# series with missing data

geocommmiss <- unique(prepcov[flagObservationStatus == 'M' & flagMethod == 'u', .(geographicAreaM49, measuredItemCPC)])


# -- Start loop ----

message('PP_imputationMethods: start imputation')

cpchierarchy <- ReadDatatable('cpc_hierarchy_ebx5')

fulldata <- data.table()

for(geo in unique(geocommmiss$geographicAreaM49)){
  series_geo <- prepcov[geographicAreaM49 == geo]
  print(geo)
  for(selComm in unique(geocommmiss[geographicAreaM49 == geo]$measuredItemCPC)){
    print(selComm)
    
    series_comm <- series_geo[measuredItemCPC == selComm]
    series_comm <- series_comm[,colSums(is.na(series_comm))<nrow(series_comm), with = F]
    
    # For LM
    series_comm_lm <- copy(series_comm)
    series_comm_lm$timePointYears <- as.numeric(series_comm_lm$timePointYears)
    xreg_comm <- series_comm[,names(series_comm) %in% c("timePointYears", "geographicAreaM49", "measuredItemCPC",    
                                                        "LogGDP", "LogVA", "ValueYield", "toi_aff"), with = F] # , "TOI_AFF"
    
    missing <- apply(xreg_comm[ , 4:ncol(xreg_comm), with = F], 2, function(x){length(x[is.na(x)])})
    
    missingyear <- series_comm[flagMethod == 'u' & 
                                 timePointYears > max(series_comm[!is.na(Value)]$timePointYears),]$timePointYears
    
    nh <- series_comm[flagMethod == 'u' & timePointYears > max(series_comm[!is.na(Value)]$timePointYears),.N] #series_comm[flagMethod == 'u' & timePointYears %in% impYear:(impYear-nyear2impute),.N] #as.numeric(maxyear) - as.numeric(minyear) + 1
    
    if(nh == 0){
      next
    }
    
    
    
    arimaxAndlmResults <- tryCatch({ arimaxAndlm(series_comm, series_comm_lm, impYear, nh, missing, missingyear)},
                                   error = function(error_condition) {list(pred = ts(rep(NA, series_comm[flagMethod == 'u' & timePointYears %in% impYear:min(as.numeric(missingyear)),.N]), 
                                                                                     start = min(as.numeric(series_comm[flagMethod == 'u' & 
                                                                                                                          timePointYears %in% impYear:min(as.numeric(missingyear)),
                                                                                                                        ]$timePointYears))),
                                                                           predlm = rep(NA, nh))})
    
    pred <- arimaxAndlmResults$pred
    predlm <- arimaxAndlmResults$predlm
    # -- Ensemble approach ----
    
    
    predEns <- tryCatch({applyEnsemble(series_comm)},
                        error = function(error_condition) {series_comm$Value})
    
    print('Ensemble ok')
    # -- Commodity group information ----
    
    
    
    cgmethod <- tryCatch({applyCG(cpchierarchy, selComm, series_geo, series_comm, missingyear)}, 
                         error = function(error_condition) {series_comm})
    
    
    # -- CPI ----
    
    
    
    
    cpimethod <- tryCatch({applyCPI(cpi, selComm, series_comm, missingyear)}, 
                          error = function(error_condition) {series_comm})
    
    # -- GDP or AgGDP ----
    
    
    gdpmethod <- tryCatch({applyGDP(GDPapproach, selComm, series_comm, missingyear)}, 
                          error = function(error_condition) {series_comm})
    
    # -- End of imputation methods ----
    
    newimputation <- series_comm[, names(prep_price), with = F]
    newimputation[ , c('ARIMAX',
                       #   'Kalman', 
                       'Ensemble',
                       'LM',
                       'Comm_Group',
                       'CPI',
                       'PriceRatio',
                       'GDP')
                   :=  list(pred,
                            predEns,
                            predlm,
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

changeset <- Changeset('imputation_annual_prices')
currentImp <- ReadDatatable('imputation_annual_prices', readOnly = FALSE)

if(nrow(currentImp[geographicaream49 %in% sessionCountry])>0){
  AddDeletions(changeset,currentImp[geographicaream49 %in% sessionCountry])
  Finalize(changeset)
}

if(nrow(imp2save) >0){
  names(imp2save) <- tolower(names(imp2save))
  AddInsertions(changeset, imp2save)
  Finalize(changeset)
}


message('Producer prices imputation and interpolation plugin completed')


from = "sws@fao.org"
to = swsContext.userEmail
subject = "PP imputation and interpolation plug-in has correctly run"
body = list(paste('The plugin has correctly run. You can check the results in the SWS datatable.'))
sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)
paste0("Email sent to ", swsContext.userEmail)

