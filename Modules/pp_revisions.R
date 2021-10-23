
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
                     token = '4c304ada-522c-4110-bac6-34a3bc0703e8')  #SETTINGS[["token"]])#'4e9d9a2e-5258-48b6-974f-3656d1af8217')
}

domainPP <- 'prod_prices'
datasetPrep <- 'annual_producer_prices_prep' 

revision0 <- ReadDatatable('revisions2control')
revision0[ , `:=` (value = as.numeric(value), value_old = as.numeric(value_old))]
# Take only what has not to be overwritten
revision <- revision0[refuse_update == TRUE]

if(revision[,.N] > 0){
 
revision[,c("value", "flagobservationstatus", "flagmethod") := NULL]
setnames(revision, c("geographicaream49", "measuredelement", "measureditemcpc", "timepointyears", 
                     "value_old", "flagobservationstatus_old", "flagmethod_old"),
         c("geographicAreaM49", "measuredElement", "measuredItemCPC", "timePointYears", 
           "Value", "flagObservationStatus", "flagMethod"))

  #-- USD conversion ----
  
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
                keys = GetCodeList('common', 'exchange_rates_annual', 'geographicAreaM49')[code %in% sessionCountry, code]),
      Dimension(name = "from_currency",
                keys = GetCodeList('common', 'exchange_rates_annual', 'from_currency')[code != 'ECU' & endDate >= '1991-01-01' | 
                                                                                         is.na(endDate), code]),
      Dimension(name = "to_currency", 
                keys = GetCodeList('common', 'exchange_rates_annual', 'to_currency')[code == 'USD', code]),
      Dimension(name = 'measuredElement',
                keys = 'LCU'),
      Dimension(name = "timePointYears", 
                keys = GetCodeList('common', 'exchange_rates_annual', 'timePointYears')[code %in% selectedYears, code]))
    
  )
  
  erdt <- GetData(erKey, flags = F) 
  
  erdt[,c('measuredElement', 'to_currency')] <- NULL
  
  # check on currency
  if(!all(erdt$from_currency %in% lcu_2_m49$code_iso)){
    stop(paste('Missing countey-currency correspondence: ', 
               unique(erdt[!from_currency %in% lcu_2_m49$code_iso]$from_currency),
               'not in the lcu_2_m49 datatble. Please update it.'))
  }
  
  # Start conversion into USD and SLC merging with XR
  pper0 <- merge(revision, erdt, by = c('geographicAreaM49', 'timePointYears'), all.x = T,
                 suffixes = c('', '_er'))
  
  if(nrow(pper0[is.na(Value_er)]) >0){
    misscountry <- unique(pper0[is.na(Value_er)]$geographicAreaM49)
    message(paste('Missing exchange rate for: ', misscountry, sep = ''))
  }
  
  pper0[, ValueUSD := Value / Value_er]
  pper0[, ValueSLC := Value]
  
  pper0[, c("Value_er")] <- NULL
  
  # get appropriate shape and flags (USD and SLC calculated, 'i')
  pper <- melt(pper0, measure.vars = c('Value', 'ValueUSD', 'ValueSLC'),
               value.name = 'Value')
  pper[variable == 'ValueUSD', c('measuredElement', 
                                 'flagMethod') := list('5532', 'i')]
  pper[variable == 'ValueSLC', c('measuredElement', 
                                 'flagMethod') := list('5531', 'i')]
  pper[ , c('variable')] <- NULL
  
  #-- Get Validated dataset ----
  
  valpriceKey = DatasetKey(
    domain = domainPP,
    dataset = datasetPrep,
    dimensions = list(
      Dimension(name = "geographicAreaM49",
                keys = GetCodeList(domainPP, datasetPrep, 'geographicAreaM49')[code %in% unique(revision$geographicAreaM49), code]),
      Dimension(name = "measuredElement", 
                keys = GetCodeList(domainPP, datasetPrep, 'measuredElement')[code %in% c('5530', '5531', '5532'), code]),
      Dimension(name = "measuredItemCPC",
                keys = GetCodeList(domainPP, datasetPrep, 'measuredItemCPC')[code %in% unique(revision$measuredItemCPC), code]),
      Dimension(name = "timePointYears", 
                keys = GetCodeList(domainPP, datasetPrep, 'timePointYears')[code >= min(pper$timePointYears), code]))
    
  )
  
  val_price <- GetData(valpriceKey, flags = TRUE)
  
  if(any(val_price[timePointYears == maxyear]$flagObservationStatus == 'B')){
    
    geotimecomb <- unique(val_price[flagObservationStatus == 'B', .(geographicAreaM49, timePointYears)])
    geotimecomb <- merge(geotimecomb, lcu_2_m49[,.(code_m49, code_iso)], 
                         by.x = 'geographicAreaM49', by.y = 'code_m49')
    # Get datatable with conversion rates 
    # If change of currency (the datatable has to be updated)
    conv_rates <- ReadDatatable('currency_changes')
    
    conv_rates_needed <- merge(conv_rates, geotimecomb, by.x  = 'new_currency_code',
                               by.y = 'code_iso')
    
    slcquest <- merge(pper, conv_rates_needed,  by = 'geographicAreaM49',
                      all.x = T, suffixes = c('', '_change'))
    
    slcquest[measuredElement == '5531' & timePointYears < timePointYears_change, c('Value',
                                                                                   'flagObservationStatus', 
                                                                                   'flagMethod'):= list(Value/exchange_rate,
                                                                                                        flagObservationStatus,
                                                                                                        'i')]
    slcquest <- slcquest[ , .(geographicAreaM49,
                              timePointYears,
                              measuredElement,
                              measuredItemCPC,
                              Value, 
                              flagObservationStatus,
                              flagMethod)]
    
  } else {
    slcquest <- pper
  }
  


SaveData(domain = 'prod_prices', dataset = 'annual_producer_prices_validation', data = slcquest,
         metadata = includemetadata, waitTimeout = Inf)
}

from = "sws@fao.org"
to = swsContext.userEmail
subject = "PP revision plug-in has correctly run"
body = list('The plugin has correctly run. Please save the data to the Producer prices preparation dataset.')
sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)
paste0("Email sent to ", swsContext.userEmail)


