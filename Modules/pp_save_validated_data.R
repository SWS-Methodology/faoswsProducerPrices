# Plugin to transfer from imputed DT to dataset

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
                     token = SETTINGS[["token"]])#'4c304ada-522c-4110-bac6-34a3bc0703e8')  #'4e9d9a2e-5258-48b6-974f-3656d1af8217')
}

domainPP <- 'prod_prices'
datasetPrep <- 'annual_producer_prices_prep' 

countryPar <-  swsContext.computationParams$countries
print(countryPar)
if(!is.null(countryPar) & length(countryPar) > 0){
  countryPar <- swsContext.computationParams$countries
  sessionCountry <- strsplit(countryPar, ', ')[[1]]
} else {
  #sessionCountry <- swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys
  countries <- GetCodeList(domainPP, datasetPrep, "geographicAreaM49")[ type == 'country']$code
  # Make sure only countries not areas
  sessionCountry <- countries#sessionCountry[sessionCountry %in% countries]
}
message(paste("Prod Prices: countries selected ", paste0(sessionCountry, collapse = ', '), '.', sep = ''))

imputations <- ReadDatatable('imputation_annual_prices')

interpolations0 <- ReadDatatable('interpolation_annual_prices')
#interpolations0[geographicaream49 == '51' & measureditemcpc == '01242', selected := TRUE]
# Take only what has been interpolated
#interpolations <- interpolations0[!flagobservationstatus %in% c('', 'X')]
interpolations <- interpolations0[selected == TRUE]

if(interpolations[,.N] == 0){
  interpolations <- data.table()
  msg <- 'No interpolation validation has been found.'
} else {
  msg <- ''
  impYear <- max(interpolations$timepointyears)
  #interpolations <- interpolations[timepointyears >= (max(as.numeric(interpolations$timepointyears))-3)]
  interpolations <- interpolations[flagobservationstatus == 'I' & flagmethod == 'interpolation']
  # interpolations[is.na(value), c('value', 'flagobservationstatus', 'flagmethod') := list(0, 'M', 'u')]
  interpolations[, flagmethod := 'e']
  interpolations[,selected := NULL]
  interpolations[,value := interpolation ]
  interpolations[,interpolation := NULL]
  interpolations[,approach := 'Interpolation']
  setnames(interpolations, 'value', 'estimation')
}
# imputations[approach == 'ARIMAX', selected := TRUE]

approaches <- ReadDatatable('method_flag_link')
if(any(!unique(imputations$approach) %in% approaches$method)){
  message(paste("Please update the 'method_flag_link' datatable. These methods are not included: ",
                paste(imputations$approach[!unique(imputations$approach) %in% approaches$method], collapse = ', '), sep = '' ))
}  

validations0 <- imputations[selected == TRUE]

validations0 <- merge(validations0, approaches, by.x = 'approach', by.y = 'method', all.x = T)

validations0[ , selected := NULL]

validations <- rbind(validations0, interpolations)

validations[ , measuredElement := '5531']

setnames(validations, c("geographicaream49", "measureditemcpc", "timepointyears", "estimation", "approach", 
                        "flagobservationstatus", "flagmethod"),
         c("geographicAreaM49", "measuredItemCPC", "timePointYears", "Value", "Metadata_Value",
           "flagObservationStatus", "flagMethod"))

validations <- validations[geographicAreaM49 %in% sessionCountry ]

includemetadata <- copy(validations[,c("geographicAreaM49", "measuredItemCPC", "timePointYears", "Metadata_Value"), with = F])
includemetadata[,Metadata:="GENERAL"]
includemetadata[,Metadata_Element:="COMMENT"]
includemetadata[,Metadata_Language:="en"]

includemetadata[, measuredElement := '5530']
includemetadata1 <- copy(includemetadata)[, measuredElement := '5531']
includemetadata2 <- copy(includemetadata)[, measuredElement := '5532']

includemetadata <- rbind(includemetadata, includemetadata1, includemetadata2)


# validations[, Value := ValueSLC]
# validations[,Metadata_Value := NULL]
# # get appropriate shape and flags (USD and SLC calculated, 'i')
# pper <- melt(validations, measure.vars = c('Value', 'ValueSLC'),
#              value.name = 'Value')
# 
# pper[variable == 'Value', c('measuredElement') := list('5530')]
# pper[ , c('variable')] <- NULL

pper <- validations[!is.na(Value)]
pper[, Metadata_Value := NULL]
#-- USD conversion ----

# Pull exchange rates dataset

if(any(unique(validations$geographicAreaM49) == '275')){
  
  erKey = DatasetKey(
    domain = 'common',
    dataset = 'exchange_rates_annual',
    dimensions = list(
      Dimension(name = 'geographicAreaM49',
                keys = GetCodeList('common', 'exchange_rates_annual', 'geographicAreaM49')[code %in% c(unique(validations$geographicAreaM49), '376') , code]),
      Dimension(name = "from_currency",
                keys = GetCodeList('common', 'exchange_rates_annual', 'from_currency')[code != 'ECU' & endDate >= '1991-01-01' | 
                                                                                         is.na(endDate), code]),
      Dimension(name = "to_currency", 
                keys = GetCodeList('common', 'exchange_rates_annual', 'to_currency')[code == 'USD', code]),
      Dimension(name = 'measuredElement',
                keys = 'LCU'),
      Dimension(name = "timePointYears", 
                keys = GetCodeList('common', 'exchange_rates_annual', 'timePointYears')[code %in% unique(validations$timePointYears), code]))
    
  )
  
  erdt <- GetData(erKey, flags = F) 
  
  if(erdt[geographicAreaM49 == '275',.N] == 0){
    # Palestine copy Israel
    palestine <- erdt[geographicAreaM49 == '376']
    palestine[,geographicAreaM49 := '275']
    erdt <- rbind(erdt, palestine)
    
  } else {
    
    erdt <- erdt[geographicAreaM49 != '275']
    palestine <- erdt[geographicAreaM49 == '376']
    palestine[,geographicAreaM49 := '275']
    erdt <- rbind(erdt, palestine)
  }
  
} else {
  
  erKey = DatasetKey(
    domain = 'common',
    dataset = 'exchange_rates_annual',
    dimensions = list(
      Dimension(name = 'geographicAreaM49',
                keys = GetCodeList('common', 'exchange_rates_annual', 'geographicAreaM49')[code %in% unique(validations$geographicAreaM49), code]),
      Dimension(name = "from_currency",
                keys = GetCodeList('common', 'exchange_rates_annual', 'from_currency')[code != 'ECU' & endDate >= '1991-01-01' | 
                                                                                         is.na(endDate), code]),
      Dimension(name = "to_currency", 
                keys = GetCodeList('common', 'exchange_rates_annual', 'to_currency')[code == 'USD', code]),
      Dimension(name = 'measuredElement',
                keys = 'LCU'),
      Dimension(name = "timePointYears", 
                keys = GetCodeList('common', 'exchange_rates_annual', 'timePointYears')[code %in% unique(validations$timePointYears), code]))
    
  )
  
  erdt <- GetData(erKey, flags = F) 
  
}


erdt[,c('measuredElement', 'to_currency')] <- NULL

if(pper[,.N] >0){
pp_converted <- convert_currency(priceData = pper, erdt = erdt, sessionElement = 'SLC')

pp2save <- pp_converted[ !is.na(Value)]

includemetadata <- includemetadata[pp2save, on = c("geographicAreaM49", "measuredItemCPC", 
                                                   "timePointYears", "measuredElement")]
# setnames(pper3, c('flagobservationstatus',
#                   'flagmethod'), 
#          c('flagObservationStatus',
#            'flagMethod'))

pp2save[, Value := round(Value, 6)]


SaveData(domain = domainPP, dataset = datasetPrep, data = pp2save,
         metadata = includemetadata, waitTimeout = Inf)
}

from = "sws@fao.org"
to = swsContext.userEmail
subject = "PP Validation plug-in has correctly run"
body = list('The plugin has correctly run. The following messages have been returned:',msg)
sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)
paste0("Email sent to ", swsContext.userEmail)

