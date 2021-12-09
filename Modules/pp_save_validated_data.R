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
         c("geographicAreaM49", "measuredItemCPC", "timePointYears", "ValueSLC", "Metadata_Value",
           "flagObservationStatus", "flagMethod"))

includemetadata <- copy(validations[,c("geographicAreaM49", "measuredItemCPC", "timePointYears", "Metadata_Value"), with = F])
includemetadata[,Metadata:="GENERAL"]
includemetadata[,Metadata_Element:="COMMENT"]
includemetadata[,Metadata_Language:="en"]

includemetadata[, measuredElement := '5530']
includemetadata1 <- copy(includemetadata)[, measuredElement := '5531']
includemetadata2 <- copy(includemetadata)[, measuredElement := '5532']

includemetadata <- rbind(includemetadata, includemetadata1, includemetadata2)


validations[, Value := ValueSLC]
validations[,Metadata_Value := NULL]
# get appropriate shape and flags (USD and SLC calculated, 'i')
pper <- melt(validations, measure.vars = c('Value', 'ValueSLC'),
             value.name = 'Value')

pper[variable == 'Value', c('measuredElement') := list('5530')]
pper[ , c('variable')] <- NULL

pper <- pper[!is.na(Value)]

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

# Get country-currency datatatble ADD withdraw year/effective change in series
xr_corr <- ReadDatatable('exchange_rates_correspondences')
#lcu_2_m49 <- ReadDatatable('lcu_2_m49')
#eco_curr0 <- ReadDatatable('currency_country_years')
xrcountry <-  ReadDatatable('currency_changes')

erdt <- fix_xr(erdt, xrcountry, xr_corr)

# # check on currency
# if(!all(erdt$from_currency %in% lcu_2_m49$code_iso)){
#   stop(paste('Missing countey-currency correspondence: ', 
#              unique(erdt[!from_currency %in% lcu_2_m49$code_iso]$from_currency),
#              'not in the lcu_2_m49 datatble. Please update it.'))
# }

# Start conversion into USD and SLC merging with XR
selectedYears <- unique(pper$timePointYears)
if(xr_corr[start_year_iso %in% selectedYears,.N] > 0){ 
  #any(pper$flagObservationStatus == 'B')){
  
  geotimecomb <- xr_corr[start_year_iso %in% selectedYears,.(geographicaream49, start_year_iso, currency_code_iso)]
  #unique(pper[flagObservationStatus == 'B', .(geographicAreaM49, timePointYears)])
  #, from_currency)])
  setnames(geotimecomb, c('geographicaream49'), c('geographicAreaM49'))
  #  geotimecomb <- merge(erdt[,.(geographicAreaM49, timePointYears, from_currency)], geotimecomb, 
  #        by.x = c('geographicAreaM49', 'timePointYears'),
  #        by.y = c('geographicAreaM49', 'start_year_iso'))
  
  # Get datatable with conversion rates 
  # If change of currency (the datatable has to be updated)
  conv_rates <- ReadDatatable('currency_changes')
  
  conv_rates_needed <- merge(conv_rates, geotimecomb, by.x  = 'new_currency_code',
                             by.y = 'currency_code_iso')# 'from_currency')
  #curr2check <- unique(geotimecomb$from_currency)
  if(any(conv_rates_needed[,.N, new_currency_code]$N > 1)){
    
    message('Two conversion rate for the same currency')
    stop()
  }
  
  slcval <- merge(validations, conv_rates_needed, by = 'geographicAreaM49', 
                  all.x = T, suffixes = c('', '_change'))
  
  slcval[measuredElement == '5530' & timePointYears < start_year_iso, #timePointYears_change, 
         c('Value',
           'flagObservationStatus', 
           'flagMethod'):= list(Value*exchange_rate,
                                flagObservationStatus,
                                'i')]
  # names(slcval)
  slcval[ , c("new_currency_code",    
              "old_currency_code",
              "exchange_rate",
              #"timePointYears_change"
              "start_year_iso"
  )] <- NULL
  
  slcquest <- merge(pper, conv_rates_needed,  by = 'geographicAreaM49',
                    all.x = T, suffixes = c('', '_change'))
  
  slcquest[measuredElement == '5530' & timePointYears < start_year_iso, #timePointYears_change,
           c('Value',
             'flagObservationStatus', 
             'flagMethod'):= list(Value*exchange_rate,
                                  flagObservationStatus,
                                  'e')]
  slcquest[ , c("new_currency_code",    
                "old_currency_code",
                "exchange_rate",
                #"timePointYears_change"
                "start_year_iso"
                )] <- NULL
  
  pper <- copy(slcquest)
  
}


## FIX DUPLICATES!!!!!!

pper0 <- merge(pper, erdt, by = c('geographicAreaM49', 'timePointYears'), all.x = T, #  %in% c('233','428','440')
               suffixes = c('', '_er'))

pper0[measuredElement == '5530', ValueUSD := Value/Value_er]
#### erdt[duplicated(erdt[,.( geographicAreaM49, timePointYears)])] !!!!!!!

pper0[, c("Value_er")] <- NULL

pper2 <- melt(pper0, measure.vars = c('Value', 'ValueUSD'),
              value.name = 'Value')

pper2[variable == 'ValueUSD', c('measuredElement') := list('5532')]
pper2[ , c('variable', 'from_currency')] <- NULL

pper3 <- pper2[ !is.na(Value)]

# setnames(pper3, c('flagobservationstatus',
#                   'flagmethod'), 
#          c('flagObservationStatus',
#            'flagMethod'))

pper3[, Value := round(Value, 6)]

SaveData(domain = domainPP, dataset = datasetPrep, data = pper3,
         metadata = includemetadata, waitTimeout = Inf)


from = "sws@fao.org"
to = swsContext.userEmail
subject = "PP Validation plug-in has correctly run"
body = list('The plugin has correctly run. The following messages have been returned:',msg)
sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)
paste0("Email sent to ", swsContext.userEmail)

