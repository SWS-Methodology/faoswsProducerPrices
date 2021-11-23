# -- Load Packages ----

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

# country parameter
countryPar <- swsContext.computationParams$countries

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

# commodities 

commPar <- swsContext.computationParams$commodities

cpchierarchy <- ReadDatatable('cpc_hierarchy_ebx5')

commPar = 'All'
if(commPar == 'All'){
  
  commList <-   GetCodeList('agriculture', 'aproduction', 'measuredItemCPC')[, code]
  
  
  
} else if(commPar == 'Fruits, Vegetables and Nuts'){
  
  commList <- rbind(  cpchierarchy[name_en_l3 %in% c('Vegetables', 'Fruits and nuts')]$code_l4,
                      cpchierarchy[name_en_l3 %in% c('Vegetables', 'Fruits and nuts')]$code_l5,
                      cpchierarchy[name_en_l3 %in% c('Vegetables', 'Fruits and nuts')]$code_l6)
  
  
  

}  else if(commPar == 'Cereals'){
  
  commList <- rbind(  cpchierarchy[name_en_l3 %in% c('Cereals')]$code_l4,
                      cpchierarchy[name_en_l3 %in% c('Cereals')]$code_l5,
                      cpchierarchy[name_en_l3 %in% c('Cereals')]$code_l6)
  
  
  

}  else if(commPar == 'Roots, Tubers and Oilseed'){
  
  commList <- rbind(  cpchierarchy[name_en_l3 %in% c('Oilseeds and oleaginous fruits',
                                                     'Edible roots and tubers with high starch or inulin content')]$code_l4,
                      cpchierarchy[name_en_l3 %in% c('Oilseeds and oleaginous fruits',
                                                     'Edible roots and tubers with high starch or inulin content')]$code_l5,
                      cpchierarchy[name_en_l3 %in% c('Oilseeds and oleaginous fruits',
                                                     'Edible roots and tubers with high starch or inulin content')]$code_l6)
  
  
  
  
  

}  else if(commPar == 'Pulses'){
  
  commList <- rbind(  cpchierarchy[name_en_l3 %in% c( 'Pulses (dried leguminous vegetables)')]$code_l4,
                      cpchierarchy[name_en_l3 %in% c( 'Pulses (dried leguminous vegetables)')]$code_l5,
                      cpchierarchy[name_en_l3 %in% c( 'Pulses (dried leguminous vegetables)')]$code_l6)
  
  
  
}  else if(commPar == 'Spices'){
  
  commList <- rbind(cpchierarchy[name_en_l3 %in% c( 'Stimulant, spice and aromatic crops')]$code_l4,
                    cpchierarchy[name_en_l3 %in% c( 'Stimulant, spice and aromatic crops')]$code_l5,
                    cpchierarchy[name_en_l3 %in% c( 'Stimulant, spice and aromatic crops')]$code_l6)
  
  
  
}  else if(commPar == 'Sugar and fibre crops; Forage products'){
  commList <- rbind(cpchierarchy[name_en_l3 %in% c( 'Sugar crops', 
                                                    'Forage products; fibre crops; plants used in perfumery, pharmacy, or for insecticidal, fungicidal or similar purposes; beet, forage plant and flower seeds; natural rubber; living plants, cut flowers and flower buds; unmanufactured tobacco; other raw vegetable materials')
                                 ]$code_l4,
                    
                    cpchierarchy[name_en_l3 %in% c( 'Sugar crops', 
                                                    'Forage products; fibre crops; plants used in perfumery, pharmacy, or for insecticidal, fungicidal or similar purposes; beet, forage plant and flower seeds; natural rubber; living plants, cut flowers and flower buds; unmanufactured tobacco; other raw vegetable materials')
                                 ]$code_l5,
                    
                    cpchierarchy[name_en_l3 %in% c( 'Sugar crops', 
                                                    'Forage products; fibre crops; plants used in perfumery, pharmacy, or for insecticidal, fungicidal or similar purposes; beet, forage plant and flower seeds; natural rubber; living plants, cut flowers and flower buds; unmanufactured tobacco; other raw vegetable materials')
                                 ]$code_l6)
  
  
  
} else if(commPar == 'Meat and animal products'){
  
  commList <- rbind(cpchierarchy[name_en_l2 %in% c( 'Live animals and animal products (excluding meat)',
                                                    'Meat, fish, fruits, vegetables, oils and fats')]$code_l3,
                    cpchierarchy[name_en_l2 %in% c( 'Live animals and animal products (excluding meat)',
                                                    'Meat, fish, fruits, vegetables, oils and fats')]$code_l4,
                    cpchierarchy[name_en_l2 %in% c( 'Live animals and animal products (excluding meat)',
                                                    'Meat, fish, fruits, vegetables, oils and fats')]$code_l5,
                    cpchierarchy[name_en_l2 %in% c( 'Live animals and animal products (excluding meat)',
                                                    'Meat, fish, fruits, vegetables, oils and fats')]$code_l6)
  
}

# years


maxyear <- 2019# swsContext.computationParams$maxyear
minyear <- 2018#swsContext.computationParams$minyear

sel_years <- as.character(minyear:maxyear)


# method

selMethod <- "ARIMAX" #swsContext.computationParams$method

message(paste("Prod Prices: countries selected ", paste0(sessionCountry, collapse = ', '), ". ",
              "Commodity groups:", paste0(commPar), '.', sep = ''))


# Load data
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
              keys = GetCodeList('agriculture', 'aproduction', 'measuredItemCPC')[code %in% commList, code]),
    Dimension(name = "timePointYears", 
              keys = GetCodeList(domainPP, datasetPrep, 'timePointYears')[code > '1991', code]))
  
)

prep_price0 <- GetData(preppriceKey, flags = TRUE)
impYear <- max(as.numeric(lastyear) - 1)

sessionCountry <- unique(prep_price0$geographicAreaM49)

# Impute last three years, i.e. delete previous imputations (timePointYears %in% as.character((impYear-2):impYear))
# or only last year (timePointYears == impYear)

# prep_price0[!flagObservationStatus %in% c('', 'X') & # NO DELETION OF PREVIOUS IMPUTATIONS!!!
#               timePointYears == impYear,
#             c('Value', 'flagObservationStatus', 'flagMethod') := 
#               list(NA, 'M', 'u')]

# Get space for imputations
prep_price_exp <- expandYear(prep_price0[timePointYears %in% sel_years], 
                             newYears = impYear)

prep_price_exp[timePointYears %in% (minyear-1):(minyear-2), ]

prep_price <- rbind(prep_price_exp, prep_price0[!timePointYears %in% sel_years])

# Re-impute all previous imputations
prep_price[timePointYears %in% sel_years & !flagObservationStatus %in% c('', 'X') , c('Value',
                                                                                        'flagObservationStatus',
                                                                                        'flagMethod') :=list(NA, 'M', 'u')]

# If no official or imputed data before the 'M;u' then the series is not imputed
# If one year not in the selected ones is missing the system ignore them
prep_price <- prep_price[!prep_price[!timePointYears %in% sel_years & 
                                       flagMethod == 'u'], on = c("geographicAreaM49",
                                                                  "measuredItemCPC")]

# 
# if(is.null(input$btn_country) |
#    is.null(input$btn_year) | is.null(input$btn_start_year) |
#    is.null(input$btn_commodity) ) return(NULL)
# 
# sel_country <- country_input[country_input$label == input$btn_country, code]
# 
# if(length(sel_country) == 0) return(NULL)
# 
# sel_country <- country_input[country_input$label == input$btn_country, code]
# sel_years <- input$btn_start_year:input$btn_year
# startyearevaluation <- as.character(min(series2est$country_data$timePointYears))#as.numeric(input$btn_start_year)-10)
# sel_commodity <- cpc_list[label == input$btn_commodity, code]
# 
# if(length(sel_country)>0){
#   if(sel_country != '' & sel_commodity != ''){
#     #prep_price_slc <- series2est$slc_series
#     prep_price_slc <- series2est$country_data[geographicAreaM49 == sel_country &
#                                                 measuredItemCPC == sel_commodity &
#                                                 measuredElement == '5531']
#     
#     
#     validate(need(prep_price_slc[timePointYears %in% sel_years & flagMethod != 'q',.N] > 0,
#                   "All data in this series are official."))

# --- Load auxiliary data ----
# load(
#   'plugintest.RData'
# )
options(warn=2)
#---- ARIMAX ----
if(selMethod == "ARIMAX"){

message('PP_imputationMethods: add covariates')

if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws1.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])
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
if(yield[geographicAreaM49 == '156',.N] == 0){
  yield[geographicAreaM49 == '1248', geographicAreaM49 := '156']
  
}

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
setkey(prepcov)
notimputedseries <- unique(prepcov[timePointYears < min(sel_years), is.na(Value), 
                                   .(geographicAreaM49, measuredItemCPC)][V1 == TRUE,
                                                                          .(geographicAreaM49, 
                                                                            measuredItemCPC)])
# Ignore series not imputable
prepcov <- prepcov[!notimputedseries, on = c('geographicAreaM49', 'measuredItemCPC')]
geocommmiss <- unique(prepcov[flagObservationStatus == 'M' & flagMethod == 'u',
                              .(geographicAreaM49, measuredItemCPC)])

setnames(notimputedseries, names(notimputedseries), tolower(names(notimputedseries)))

#notimpseries <- ReadDatatable('non_imputed_bulk_series', readOnly = F)

#chng <- Changeset('non_imputed_bulk_series')
#AddDeletions(chng, notimpseries[geographicaream49 %in% sessionCountry])
#Finalise(chng)
#AddInsertions(chng, notimputedseries)
#Finalise(chng)

# -- Start loop ----


message('PP_imputationMethods: start imputation')

fulldata <- data.table()
#prepcov <- prepcov[timePointYears != 2020]
options(warn = 2)
for(geo in unique(geocommmiss$geographicAreaM49)){
  series_geo <- prepcov[geographicAreaM49 == geo]
  print(geo)
  for(selComm in unique(geocommmiss[geographicAreaM49 == geo]$measuredItemCPC)){
    print(selComm)
#    if(selComm =='01929.90')browser()
    series_comm <- series_geo[measuredItemCPC == selComm]
    series_comm <- series_comm[,colSums(is.na(series_comm))<nrow(series_comm), with = F]

    # Series for arima model: time series until btn_start_year
    ppseries <- tryCatch({ts(series_comm[timePointYears < minyear]$LogValue,
                             start = as.numeric(min(series_comm$timePointYears)))},
                         error = function(error_condition){ts(series_comm$LogValue, start = as.numeric(min(series_comm$timePointYears)))})
    
    if(length(ppseries[!is.na(ppseries)]) < 2 ){
      next
    }
    
    # Covariate set Exclude Year, country and commodity (columns 1:3)
    xreg_comm <- series_comm[  , names(series_comm) %in% c("timePointYears", "geographicAreaM49", "measuredItemCPC",    
                                                           "LogGDP", "LogVA", "ValueYield", "toi_aff"), with = F]
    
    missing <- apply(xreg_comm[ , 4:ncol(xreg_comm), with = F], 2, function(x){length(x[is.na(x)])})
    if(max(missing) == min(missing)){
      xreg_comm <- xreg_comm
    } else {
      xreg_comm <- xreg_comm[ , c("timePointYears", "geographicAreaM49", "measuredItemCPC",    
                                  names(missing[missing == min(missing)])), with = F ]
    }
    
    # Exclude columns with NAs
    xreg_comm <- xreg_comm[ , colSums(is.na(xreg_comm)) < nrow(series_comm), with = F]
    
    # Check if there are data before the data to impute
    if(xreg_comm[timePointYears < minyear,.N]== 0){
      xreg_comm
    } else {
      xreg_comm <- xreg_comm[timePointYears < minyear,]
    }
    
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
      
      for(j in 1:nrow(covcomb)){
        cov2use <- as.matrix(covcomb[j,])
        xreg2use <- xreg_comm[ , cov2use[!is.na(cov2use)], with = F]
        
        modj <- tryCatch(auto.arima(ppseries, seasonal = FALSE, xreg = xreg2use),
                         error = function(error_condition) { auto.arima(ppseries, seasonal = FALSE)})
        modlist[[j]] <- modj
        
      }
      
      # Best ARIMAX with lowest BIC
      if(length(which(!lapply(modlist, function(x){x$bic}) %in% c(Inf, -Inf))) == 0){
        bestmod <- NULL
      } else {
      bestmod <- modlist[which(!lapply(modlist, function(x){x$bic}) %in% c(Inf, -Inf))][[1]]
      }
      #-- ARIMA forescast ----
      nh <- series_comm[flagMethod == 'u' & timePointYears %in% sel_years,.N] #as.numeric(maxyear) - as.numeric(minyear) + 1
      missingyear <- series_comm[flagMethod == 'u' & timePointYears %in% sel_years,]$timePointYears
      # Covariates for predictive years
      
      if(length(bestmod) > 0){
        bestmod <- modlist[[which(unlist(lapply(modlist, function(x){x$bic})) ==
                                    min(unlist(lapply(modlist, function(x){x$bic}))))[1]]]
        varselected <- names(bestmod$coef)[names(bestmod$coef) %in% names(xreg_comm)]
        
        if(length(varselected) == 0){
          pred <- forecast(bestmod, h = nh)$mean
        } else {
        
        xreg_comm_pred <- series_comm[flagMethod == 'u' & timePointYears %in% sel_years, 
                                      varselected, with = F]
        pred <- forecast(bestmod, h = nh, xreg = xreg_comm_pred)$mean
        
        }
        
      } else {
        pred <- NA
      }
      
      #-- Kalman ----
      
      if(any(series_comm[timePointYears %in% sel_years]$flagObservationStatus %in% c('', 'X')) & length(bestmod) > 0){
        
        prepcovInterp <- copy(series_comm)
        prepcovInterp[timePointYears %in% sel_years & !flagObservationStatus %in% c('', 'X'), LogValue := NA]
        tskalm <- ts(prepcovInterp$LogValue, start = as.numeric(as.numeric(min(series_comm$timePointYears))))
       
        predkalm <- tryCatch(na.kalman(x = tskalm, model = "auto.arima", #bestmod$model, 
                                       xreg = prepcovInterp[, varselected, with = F]),
                             error = function(error_condition) {  tskalm[is.na(tskalm)] <- pred[is.na(tskalm)] })
      
        
        if(all(predkalm[is.na(tskalm)] == 0)){
          pred1 <- series_comm[!timePointYears %in% sel_years,]$LogValue
          pred2 <- series_comm[timePointYears %in% sel_years,]$LogValue
          pred2[which(is.na(pred2))] <- pred[which(is.na(pred2))]
          pred <- ts(c(pred1, pred2), start = as.numeric(as.numeric(min(series_comm$timePointYears))))
          #pred
        } else {
          pred <- predkalm  
        }
      }
        
    } else{
      prepcovInterp <- copy(series_comm)
      pred <- ts(rep(NA, length(sel_years)), start = as.numeric(as.numeric(min(sel_years))))
    }
   
    # -- End of imputation methods ----
    
    newimputation <- series_comm[, names(prep_price), with = F]
    newimputation[timePointYears %in% missingyear, ARIMAX := exp(pred[time(pred) %in% missingyear])]
    newimputation[!timePointYears %in% missingyear, ARIMAX := Value]
    
    newimputation[ !flagObservationStatus %in% c('', 'X'),
                   c('Value', 'flagObservationStatus', 'flagMethod') :=
                     list(ARIMAX, 'I', 'e')]
    
    fulldata <- rbind(fulldata, newimputation)
    
}
#fulldata <- rbind(fulldata, fulldata_country)
}
 




#---- ENSEMBLE ----


} else if(selMethod == "Ensemble"){
  
  # -- Missing value ----
  prepcov <- prep_price[order(timePointYears)]
  prepcov[!is.na(Value), LogValue := log(Value)]
  
  # series with missing data
  setkey(prepcov)
  notimputedseries <- unique(prepcov[timePointYears < min(sel_years), is.na(Value), .(geographicAreaM49, measuredItemCPC)][V1 == TRUE,.(geographicAreaM49, measuredItemCPC)])
  # Ignore series not imputable
  prepcov <- prepcov[!notimputedseries, on = c('geographicAreaM49', 'measuredItemCPC')]
  geocommmiss <- unique(prepcov[flagObservationStatus == 'M' & flagMethod == 'u', .(geographicAreaM49, measuredItemCPC)])
  setnames(notimputedseries, names(notimputedseries), tolower(names(notimputedseries)))
  
  # notimpseries <- ReadDatatable('non_imputed_bulk_series', readOnly = F)
  # 
  # chng <- Changeset('non_imputed_bulk_series')
  # AddDeletions(chng, notimpseries[geographicaream49 %in% sessionCountry])
  # Finalise(chng)
  # AddInsertions(chng, notimputedseries)
  # Finalise(chng)
  # 
  # -- Start loop ----
  
  message('PP_imputationMethods: start imputation')
  
  fulldata <- data.table()
  
  for(geo in unique(geocommmiss$geographicAreaM49)){
    series_geo <- prepcov[geographicAreaM49 == geo]
   
    for(selComm in unique(geocommmiss[geographicAreaM49 == geo]$measuredItemCPC)){
      
      series_comm <- series_geo[measuredItemCPC == selComm]
      series_comm <- series_comm[,colSums(is.na(series_comm))<nrow(series_comm), with = F]
      # -- Ensemble approach ----
      
      impPar <- defaultImputationParameters()
      impPar$imputationValueColumn="Value"
      impPar$imputationFlagColumn="flagObservationStatus"
      impPar$imputationMethodColumn="flagMethod"
      impPar$byKey=c("geographicAreaM49", "measuredItemCPC")
      impPar$estimateNoData=FALSE
      
      # If the data series contains only zero and missing value then it is considered to contain no information for imputation.
      
      series_ensemble <- series_comm
      
      series_ensemble <- series_ensemble[timePointYears %in% sel_years &
                                           !flagObservationStatus %in% c('', 'X'), 
                                         c('Value',
                                           'flagObservationStatus',
                                           'flagMethod') := list(NA, 'M', 'u')]
      
      pp_ensemble_sub <- series_ensemble[,c("geographicAreaM49",
                                            "timePointYears",
                                            "measuredItemCPC",
                                            "Value",
                                            "flagObservationStatus",
                                            "flagMethod"), with = F]
      
      if(pp_ensemble_sub[flagObservationStatus != 'M',.N] > 2){
      pp_ensemble_sub <- pp_ensemble_sub[order(timePointYears)]
      # If no missing data the commodityDB does not change
      pp_ensemble_imp <- imputeVariable(data = pp_ensemble_sub,
                                        imputationParameters = impPar)
      
      predEns <- pp_ensemble_imp$Value
      } else {
        
        predEns <- rep(NA, length =  pp_ensemble_sub[,.N])
      }
  
    # -- End of imputation methods ----
      
      newimputation <- series_comm[, names(prep_price), with = F]
      newimputation[ ,Ensemble := predEns] 
      
      
      
      # newimputation[ is.na(Value), 
      #                c('Value', 'flagObservationStatus', 'flagMethod') := 
      #                  list(Imputed, 'P', 'e')]
      
      imp2save <- copy(newimputation)
     
      
      fulldata <- rbind(fulldata, newimputation)
      
    }
#    fulldata <- rbind(fulldata, fulldata_country)
  }
  
  
  
  
  
#---- LINEAR ----  
  
  
  } else if(selMethod == 'Linear'){
  
  
  
  message('PP_imputationMethods: add covariates')
  
  if(CheckDebug()){
    library(faoswsModules)
    SETTINGS = ReadSettings("sws1.yml")
    R_SWS_SHARE_PATH = SETTINGS[["share"]]
    SetClientFiles(SETTINGS[["certdir"]])
    GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                       token = SETTINGS[["token"]])
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
  setkey(prepcov)
  notimputedseries <- unique(prepcov[timePointYears < min(sel_years), is.na(Value), .(geographicAreaM49, measuredItemCPC)][V1 == TRUE,.(geographicAreaM49, measuredItemCPC)])
  # Ignore series not imputable
  prepcov <- prepcov[!notimputedseries, on = c('geographicAreaM49', 'measuredItemCPC')]
  geocommmiss <- unique(prepcov[flagObservationStatus == 'M' & flagMethod == 'u', .(geographicAreaM49, measuredItemCPC)])
  setnames(notimputedseries, names(notimputedseries), tolower(names(notimputedseries)))
  
  # notimpseries <- ReadDatatable('non_imputed_bulk_series', readOnly = F)
  # 
  # chng <- Changeset('non_imputed_bulk_series')
  # AddDeletions(chng, notimpseries[geographicaream49 %in% sessionCountry])
  # Finalise(chng)
  # AddInsertions(chng, notimputedseries)
  # Finalise(chng)
  # 
  # -- Start loop ----
  
  
  message('PP_imputationMethods: start imputation')
  
  cpchierarchy <- ReadDatatable('cpc_hierarchy_ebx5')
  
  fulldata <- data.table()

  for(geo in unique(geocommmiss$geographicAreaM49)){
    series_geo <- prepcov[geographicAreaM49 == geo]
    
    fulldata_country <- data.table()
    
    for(selComm in unique(geocommmiss[geographicAreaM49 == geo]$measuredItemCPC)){
      
      series_comm <- series_geo[measuredItemCPC == selComm]
      series_comm <- series_comm[,colSums(is.na(series_comm))<nrow(series_comm), with = F]
      # Series for lm model: numeric year and series until btn_start_year
      series_comm_lm <- copy(series_comm)
      series_comm_lm$timePointYears <- as.numeric(series_comm_lm$timePointYears)
      if(nrow(series_comm[!is.na(Value)]) < 2 ){
        next
      }
      # Covariate set Exclude Year, country and commodity (columns 1:3)
      xreg_comm <- series_comm_lm[  , names(series_comm_lm) %in% c("timePointYears", "geographicAreaM49", "measuredItemCPC",    
                                                                "LogGDP", "LogVA", "ValueYield", "toi_aff"), with = F]
      
      missing <- apply(xreg_comm[ , 4:ncol(xreg_comm), with = F], 2, function(x){length(x[is.na(x)])})
      if(max(missing) == min(missing)){
        xreg_comm <- xreg_comm
      } else {
        xreg_comm <- xreg_comm[ , c("timePointYears", "geographicAreaM49", "measuredItemCPC",    
                                    names(missing[missing == min(missing)])), with = F ]
      }
      # Exclude columns with NAs
      xreg_comm <- xreg_comm[ , colSums(is.na(xreg_comm)) < nrow(series_comm), with = F]
      if(xreg_comm[timePointYears < minyear,.N]== 0){
        xreg_comm
      } else {
        xreg_comm <- xreg_comm[timePointYears < minyear,]
      }
      
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
        lmlist <- list()
        
        for(j in 1:nrow(covcomb)){
          cov2use <- as.matrix(covcomb[j,])
          xreg2use <- xreg_comm[ , cov2use[!is.na(cov2use)], with = F]
          
          formula <- as.formula(paste('LogValue ~ timePointYears + ',
                                      paste(names(xreg2use), collapse = ' + '), collapse = ''))
          lmlist[[j]] <- tryCatch(lm(formula = formula, data = series_comm_lm),
                                  error = function(error_condition) { lm(formula = 'LogValue ~ timePointYears',
                                                                         data = series_comm_lm)})
          # print(j)
        }
        
        # Best LM with lowest BIC
        BICnotInf <- unlist(lapply(lmlist, function(x){BIC(x)}))
        BICnotInf <- BICnotInf[!BICnotInf %in% c(-Inf, Inf)]
        
        #-- ARIMA forescast ----
        nh <- maxyear - minyear +1
        
        #-- Linear model ----
        
        # Dataset for predictive years
        
        series_comm_lm_pred <- copy(series_comm_lm)
        series_comm_lm_pred <- series_comm_lm_pred[timePointYears %in% sel_years]
        series_comm_lm_pred$timePointYears <- as.numeric(series_comm_lm_pred$timePointYears)
        
        if(length(BICnotInf) > 0){
          bestlm <- lmlist[[which(unlist(lapply(lmlist, function(x){BIC(x)})) == 
                                    min(BICnotInf))[1]]]
          predlm <- forecast(bestlm, h = nh, series_comm_lm_pred)$mean
        } else {
          predlm <- rep(NA, nh)
        }
        
        #-- Kalman ----
        
        if(any(series_comm[timePointYears %in% sel_years]$flagMethod == 'q') & length(BICnotInf) > 0){
          
          prepcovInterp <- copy(series_comm)
          prepcovInterp[timePointYears %in% sel_years & flagMethod != 'q', LogValue := NA]
          
          prepcovInterp$timePointYears <- as.numeric(prepcovInterp$timePointYears)
          predlm <- na.approx(prepcovInterp$LogValue) #forecast(bestlm, h = nh, prepcovInterp)$mean
          #predlm <- ifelse(is.na(prepcovInterp$LogValue), predlm, prepcovInterp$LogValue)
          
        }
        
      } else{

        predlm <- rep(NA, length(sel_years))
      }
      
      # -- End of imputation methods ----
      
      newimputation <- series_comm[, names(prep_price), with = F]
      newimputation[ , Linear := c(series_comm[!timePointYears %in% sel_years][order(timePointYears)]$Value,
                               exp(predlm))] 
      
      # newimputation[ is.na(Value), 
      #                c('Value', 'flagObservationStatus', 'flagMethod') := 
      #                  list(Imputed, 'P', 'e')]
      
      imp2save <- copy(newimputation)
      #setnames(imp2save, 'Value', 'Original series')
      
      fulldata <- rbind(fulldata, imp2save)
    }
  }
  

  
  
#---- COMMODITY GROUP ----  
  
  
  } else if(selMethod == 'Comm_group'){

  # -- Missing value ----
  prepcov <- prep_price[order(timePointYears)]
  prepcov[!is.na(Value), LogValue := log(Value)]
  
  # series with missing data
  setkey(prepcov)
  #notimputedseries <- unique(prepcov[timePointYears < min(sel_years), is.na(Value), .(geographicAreaM49, measuredItemCPC)][V1 == TRUE,.(geographicAreaM49, measuredItemCPC)])
  # Ignore series not imputable
  #prepcov <- prepcov[!notimputedseries, on = c('geographicAreaM49', 'measuredItemCPC')]
  geocommmiss <- unique(prepcov[flagObservationStatus == 'M' & flagMethod == 'u', .(geographicAreaM49, measuredItemCPC)])

  # -- Start loop ----
  
  
  message('PP_imputationMethods: start imputation')
  
  cpchierarchy <- ReadDatatable('cpc_hierarchy_ebx5')
  
  fulldata <- data.table()

  for(geo in unique(geocommmiss$geographicAreaM49)){
    series_geo <- prepcov[geographicAreaM49 == geo]
    
    for(selComm in unique(geocommmiss[geographicAreaM49 == geo]$measuredItemCPC)){

      # -- Commodity group information ----
      series_comm <- series_geo[measuredItemCPC == selComm]
      series_comm <- series_comm[,colSums(is.na(series_comm))<nrow(series_comm), with = F]
      
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
      
      commoditygroup  <- prepcov[!is.na(Value) & geographicAreaM49 == geo &
                                   measuredItemCPC %in% codes2compare]
      
      # If there are data ok otherwise hierarchy up (if code_l4)
      if(nrow(commoditygroup[timePointYears %in% sel_years]) == 0
         & names(hier1) == 'code_l4'){
        
        hier1 <- unique(hierarchyComm[ , "code_l3", with = F]) 
        codes2compare <- c(cpchierarchy[code_l3 == hier1$code_l3 & !is.na(code_l4), ]$code_l4,
                           cpchierarchy[code_l3 == hier1$code_l3 & !is.na(code_l5), ]$code_l5,
                           cpchierarchy[code_l3 == hier1$code_l3 & !is.na(code_l6), ]$code_l6)
        
        commoditygroup <- prepcov[!is.na(Value) & geographicAreaM49 == geo & 
                                           measuredItemCPC %in% codes2compare]
        
      }else if(nrow(commoditygroup[timePointYears %in% 
                                   sel_years]) == 0
               & names(hier1) == 'code_l3'){
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
      # commoditygroup2merge[timePointYears == '2018', medianbyyear := 0.2]
      
      years <- sel_years
      cgmethod <- copy(series_comm)
      
      cgmethod <- merge(cgmethod, commoditygroup2merge,
                        by = 'timePointYears', all.x = T)
      cgmethod <- cgmethod[order(timePointYears)]
      
      cgmethod[, ValueCG := shift(Value)*(1+medianbyyear)]
      cgmethod[flagMethod == 'q', ValueCG := Value]
      cgmethod[timePointYears %in% sel_years, Value := ValueCG]               
      
      
      # -- End of imputation methods ----
      
      newimputation <- series_comm[, names(prep_price), with = F]
      newimputation[ , Comm_Group := cgmethod$Value] 
      
      # newimputation[ is.na(Value), 
      #                c('Value', 'flagObservationStatus', 'flagMethod') := 
      #                  list(Imputed, 'P', 'e')]
      
      imp2save <- copy(newimputation)
      #setnames(imp2save, 'Value', 'Original series')
      fulldata <- rbind(fulldata, imp2save)
      
    }
  }
  
  
  }else if(selMethod == 'CPI'){
  
  
  message('PP_imputationMethods: add covariates')
 
  # -- Missing value ----
  prepcov <- prep_price[order(timePointYears)]
  prepcov[!is.na(Value), LogValue := log(Value)]
  
  # series with missing data
  
  geocommmiss <- unique(prepcov[flagObservationStatus == 'M' & flagMethod == 'u', .(geographicAreaM49, measuredItemCPC)])
  
  # -- Start loop ----
  
  
  message('PP_imputationMethods: start imputation')
  
  cpchierarchy <- ReadDatatable('cpc_hierarchy_ebx5')
  
  fulldata <- data.table()

  for(geo in unique(geocommmiss$geographicAreaM49)){
    series_geo <- prepcov[geographicAreaM49 == geo]
    
    cpiKey = DatasetKey(
      domain = domainPP,
      dataset = 'consumer_price_indices',
      dimensions = list(
        Dimension(name = "geographicAreaM49",
                  keys = GetCodeList(domainPP, 'consumer_price_indices', 'geographicAreaM49')[code == geo, code]),
        Dimension(name = "measuredElement", 
                  keys = GetCodeList(domainPP, 'consumer_price_indices', 'measuredElement')[code %in% c('23012', '23013'), code]),
        Dimension(name = "timePointYears",
                  keys = GetCodeList(domainPP, 'consumer_price_indices', 'timePointYears')[, code]),
        Dimension(name = "timePointMonths", 
                  keys = GetCodeList(domainPP, 'consumer_price_indices', 'timePointMonths')[code == '7013', code]))
    )
    
    cpi0 <- GetData(cpiKey, flags = TRUE)
    
    for(selComm in unique(geocommmiss[geographicAreaM49 == geo]$measuredItemCPC)){
      
      series_comm <- series_geo[measuredItemCPC == selComm]
      series_comm <- series_comm[,colSums(is.na(series_comm))<nrow(series_comm), with = F]
      
      # -- CPI ----
     
      if(cpi0[,.N] > 0){
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
        
        cpimethod[, ValueCPI := shift(Value)*(1+GR2use)]
        cpimethod[flagMethod == 'q', ValueCPI := Value]
        
        cpimethod[timePointYears %in% sel_years, Value := ValueCPI]      
        
      } else {
        
        cpimethod <- data.table(Value = NA)
      }
      
      # -- End of imputation methods ----
      
      newimputation <- series_comm[, names(prep_price), with = F]
      newimputation[ , CPI := cpimethod$Value] 
      
      
      
      # newimputation[ is.na(Value), 
      #                c('Value', 'flagObservationStatus', 'flagMethod') := 
      #                  list(Imputed, 'P', 'e')]
      
      imp2save <- copy(newimputation)

      fulldata <- rbind(fulldata, imp2save)
    }
  }
  

  
  }else if(selMethod == 'GDP'){
  
  
  message('PP_imputationMethods: add covariates')
  
  if(CheckDebug()){
    library(faoswsModules)
    SETTINGS = ReadSettings("sws1.yml")
    R_SWS_SHARE_PATH = SETTINGS[["share"]]
    SetClientFiles(SETTINGS[["certdir"]])
    GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                       token = SETTINGS[["token"]])
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
 
  
  if(CheckDebug()){
    library(faoswsModules)
    SETTINGS = ReadSettings("sws.yml")
    R_SWS_SHARE_PATH = SETTINGS[["share"]]
    SetClientFiles(SETTINGS[["certdir"]])
    GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                       token = SETTINGS[["token"]])#'4e9d9a2e-5258-48b6-974f-3656d1af8217')
  }
  
  # -- Missing value ----
  prepcov <- prepcov[order(timePointYears)]
  prepcov[!is.na(Value), LogValue := log(Value)]
  
  # series with missing data
  
  geocommmiss <- unique(prepcov[flagObservationStatus == 'M' & flagMethod == 'u', .(geographicAreaM49, measuredItemCPC)])
  
  # -- Start loop ----
  
  
  message('PP_imputationMethods: start imputation')
  
  cpchierarchy <- ReadDatatable('cpc_hierarchy_ebx5')
  
  fulldata <- data.table()

  for(geo in unique(geocommmiss$geographicAreaM49)){
    series_geo <- prepcov[geographicAreaM49 == geo]
    
    for(selComm in unique(geocommmiss[geographicAreaM49 == geo]$measuredItemCPC)){
     
      series_comm <- series_geo[measuredItemCPC == selComm]
      series_comm <- series_comm[,colSums(is.na(series_comm))<nrow(series_comm), with = F]
      
      # -- GDP or AgGDP ----
      gdpmethod <- copy(series_comm)
      GDPapproach[ , GRgdp := c(NA, diff(GDP2use))/shift(GDP2use), by = 'geographicAreaM49']
      
      gdpmethod <- merge(gdpmethod, GDPapproach, 
                         by = c('geographicAreaM49', 'timePointYears'), 
                         all.x = T)
      
      gdpmethod[order(timePointYears)]
      gdpmethod[, ValueGDPdefl := shift(Value)*(1+GRgdp)]
      gdpmethod[flagMethod == 'q', ValueGDPdefl := Value]
      gdpmethod[timePointYears %in% sel_years, Value := ValueGDPdefl]  
      
      # -- End of imputation methods ----
      
      newimputation <- series_comm[, names(prep_price), with = F]
      newimputation[ ,GDP := gdpmethod$Value] 
      
      # newimputation[ is.na(Value), 
      #                c('Value', 'flagObservationStatus', 'flagMethod') := 
      #                  list(Imputed, 'P', 'e')]
      
      imp2save <- copy(newimputation)
      fulldata <- rbind(fulldata, imp2save)
      
    }
  }
  
  
  } 

fulldata <- fulldata[timePointYears %in% sel_years & !flagObservationStatus %in% c('', 'X')]

includemetadata <- copy(fulldata[timePointYears %in% sel_years & !flagObservationStatus %in% c('', 'X'),
                                 c("geographicAreaM49", "measuredItemCPC", "timePointYears"), with = F])
includemetadata[ ,  Metadata_Value := selMethod]
includemetadata[,Metadata:="GENERAL"]
includemetadata[,Metadata_Element:="COMMENT"]
includemetadata[,Metadata_Language:="en"]

includemetadata[, measuredElement := '5530']
includemetadata1 <- copy(includemetadata)[, measuredElement := '5531']
includemetadata2 <- copy(includemetadata)[, measuredElement := '5532']

includemetadata <- rbind(includemetadata, includemetadata1, includemetadata2)

setnames(fulldata, names(fulldata)[names(fulldata) == selMethod], 'ValueSLC')

fulldata[, Value := ValueSLC]

# get appropriate shape and flags (USD and SLC calculated, 'i')
pper <- melt(fulldata, measure.vars = c('Value', 'ValueSLC'),
             value.name = 'Value')

pper[variable == 'Value', c('measuredElement') := list('5530')]
pper[ , c('variable')] <- NULL

if(any(pper$flagObservationStatus == 'B')){
  
  geotimecomb <- unique(pper[flagObservationStatus == 'B', .(geographicAreaM49, timePointYears, from_currency)])
  
  # Get datatable with conversion rates 
  # If change of currency (the datatable has to be updated)
  conv_rates <- ReadDatatable('currency_changes')
  
  conv_rates_needed <- merge(conv_rates, geotimecomb, by.x  = 'new_currency_code',
                             by.y = 'from_currency')
  
  slcval <- merge(data2save, conv_rates_needed, by = 'geographicAreaM49', 
                  all.x = T, suffixes = c('', '_change'))
  
  slcval[measuredElement == '5530' & timePointYears < timePointYears_change, c('Value',
                                                                               'flagObservationStatus', 
                                                                               'flagMethod'):= list(Value*exchange_rate,
                                                                                                    flagObservationStatus,
                                                                                                    flagMethod)]
  names(slcval)
  slcval[ , c("new_currency_code",    
              "old_currency_code",
              "exchange_rate",
              "timePointYears_change")] <- NULL
  
  slcquest <- merge(pper, conv_rates_needed,  by = 'geographicAreaM49',
                    all.x = T, suffixes = c('', '_change'))
  
  slcquest[measuredElement == '5530' & timePointYears < timePointYears_change, c('Value',
                                                                                 'flagObservationStatus', 
                                                                                 'flagMethod'):= list(Value*exchange_rate,
                                                                                                      flagObservationStatus,
                                                                                                      flagMethod)]
  slcquest[ , c("new_currency_code",    
                "old_currency_code",
                "exchange_rate",
                "timePointYears_change")] <- NULL
  
  
  
} else {
  # slcval <- val_price
  datalcu <- pper
}

# USD conversion 
# Get country-currency datatatble ADD withdraw year/effective change in series
lcu_2_m49 <- ReadDatatable('lcu_2_m49')
lcu_2_m49[start_year_iso == '', start_year_iso := '1900']
lcu_2_m49[end_year_iso == '', end_year_iso := '9999']

# Pull exchange rates dataset

erKey = DatasetKey(
  domain = 'common',
  dataset = 'exchange_rates_annual',
  dimensions = list(
    Dimension(name = 'geographicAreaM49',
              keys = GetCodeList('common', 'exchange_rates_annual', 'geographicAreaM49')[code %in% unique(fulldata$geographicAreaM49), code]),
    Dimension(name = "from_currency",
              keys = GetCodeList('common', 'exchange_rates_annual', 'from_currency')[code != 'ECU' & endDate >= '1991-01-01' | 
                                                                                       is.na(endDate), code]),
    Dimension(name = "to_currency", 
              keys = GetCodeList('common', 'exchange_rates_annual', 'to_currency')[code == 'USD', code]),
    Dimension(name = 'measuredElement',
              keys = 'LCU'),
    Dimension(name = "timePointYears", 
              keys = GetCodeList('common', 'exchange_rates_annual', 'timePointYears')[code %in% unique(fulldata$timePointYears), code]))
  
)

erdt <- GetData(erKey, flags = F) 

erdt[,c('measuredElement', 'to_currency')] <- NULL

eco_curr0 <- ReadDatatable('currency_country_years')
xrcountry <-  ReadDatatable('currency_changes')

erdt <- fix_xr(erdt, lcu_2_m49, eco_curr0, xrcountry)

## FIX DUPLICATES!!!!!!

pper0 <- merge(datalcu, erdt, by = c('geographicAreaM49', 'timePointYears'), all.x = T,
               suffixes = c('', '_er'))

pper0[measuredElement == '5530', ValueUSD := Value/Value_er]
#### erdt[duplicated(erdt[,.( geographicAreaM49, timePointYears)])] !!!!!!!

pper0[, c("Value_er")] <- NULL

pper2 <- melt(pper0, measure.vars = c('Value', 'ValueUSD'),
              value.name = 'Value')

pper2[variable == 'ValueUSD', c('measuredElement') := list('5532')]
pper2[ , c('variable', 'from_currency')] <- NULL

pper3 <- pper2[ !is.na(Value)]
pper3[timePointYears %in% sel_years & !flagObservationStatus %in% c('', 'X'), 
      c('flagObservationStatus', 'flagMethod') := list('I', 'e')]

SaveData(domain = domainPP,
         dataset = datasetPrep,
         data = pper3,
         metadata = includemetadata,
         waitTimeout = Inf)


message('Producer prices series revision plugin completed')


from = "sws@fao.org"
to = swsContext.userEmail
subject = "Producer prices series revision plugin completed"
body = list('The plugin has correctly run. You can check the results in the SWS dataset')
sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)
paste0("Email sent to ", swsContext.userEmail)


