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
  SETTINGS = ReadSettings("sws.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = '4c304ada-522c-4110-bac6-34a3bc0703e8')  #SETTINGS[["token"]])#'4e9d9a2e-5258-48b6-974f-3656d1af8217')
}


imputations <- ReadDatatable('imputation_annual_prices')

# imputations[approach == 'ARIMAX', selected := TRUE]

approaches <- ReadDatatable('method_flag_link')
if(any(!unique(imputations$approach) %in% approaches$method)){
  message(paste("Please update the 'method_flag_link' datatable. These methods are not included: ",
                paste(imputations$approach[!unique(imputations$approach) %in% approaches$method], collapse = ', '), sep = '' ))
}  

validations <- imputations[selected == TRUE]

validations[approach %in% c("ARIMAX", "Ensemble", "LM"), c('flagObservationStatus', 'flagMethod') := list('I','e')]
validations[approach %in% c("Comm_Group", "CPI", "PriceRatio", "GDP"), c('flagObservationStatus', 'flagMethod') := list('I','i')]

validations[approach %in% c("Manual input"), c('flagObservationStatus', 'flagMethod') := list('E','f')]

validations[ , selected := NULL]
validations[ , measuredElement := '5531']

setnames(validations, c("geographicaream49", "measureditemcpc", "timepointyears", "estimation", "approach"),
         c("geographicAreaM49", "measuredItemCPC", "timePointYears", "ValueSLC", "Metadata_Value"))

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


if(any(pper$flagObservationStatus == 'B')){

  geotimecomb <- unique(pper[flagObservationStatus == 'B', .(geographicAreaM49, timePointYears, from_currency)])
  
  # Get datatable with conversion rates 
  # If change of currency (the datatable has to be updated)
  conv_rates <- ReadDatatable('currency_changes')
  
  conv_rates_needed <- merge(conv_rates, geotimecomb, by.x  = 'new_currency_code',
                             by.y = 'from_currency')
  
  slcval <- merge(validations, conv_rates_needed, by = 'geographicAreaM49', 
                  all.x = T, suffixes = c('', '_change'))
  
  slcval[measuredElement == '5530' & timePointYears < timePointYears_change, c('Value',
                                                                               'flagObservationStatus', 
                                                                               'flagMethod'):= list(Value*exchange_rate,
                                                                                                    flagObservationStatus,
                                                                                                    'i')]
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
                                                                                                      'i')]
  slcquest[ , c("new_currency_code",    
                "old_currency_code",
                "exchange_rate",
                "timePointYears_change")] <- NULL
  
  
  
} else {
  # slcval <- val_price
  datalcu <- pper
}


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

erdt[,c('measuredElement', 'to_currency')] <- NULL

# # check on currency
# if(!all(erdt$from_currency %in% lcu_2_m49$code_iso)){
#   stop(paste('Missing countey-currency correspondence: ', 
#              unique(erdt[!from_currency %in% lcu_2_m49$code_iso]$from_currency),
#              'not in the lcu_2_m49 datatble. Please update it.'))
# }

# Start conversion into USD and SLC merging with XR

## FIX DUPLICATES!!!!!!

pper0 <- merge(datalcu, erdt[!geographicAreaM49 %in% c('233','428','440')], by = c('geographicAreaM49', 'timePointYears'), all.x = T,
               suffixes = c('', '_er'))

pper0[measuredElement == '5530', ValueUSD := Value/Value_er]
#### erdt[duplicated(erdt[,.( geographicAreaM49, timePointYears)])] !!!!!!!

pper0[, c("Value_er")] <- NULL

pper2 <- melt(pper0, measure.vars = c('Value', 'ValueUSD'),
              value.name = 'Value')

pper2[variable == 'ValueUSD', c('measuredElement') := list('5532')]
pper2[ , c('variable', 'from_currency')] <- NULL

pper3 <- pper2[ !is.na(Value)]


SaveData(domain = 'prod_prices', dataset = 'annual_producer_prices_validation', data = pper3,
         metadata = includemetadata, waitTimeout = Inf)

