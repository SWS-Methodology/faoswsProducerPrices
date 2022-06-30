
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
  library(sendmailR)
})

#-- Token QA ----

if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws1.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = 'd0084d47-e95d-42aa-8435-654590f3be55')
                     #SETTINGS[["token"]])#'4e9d9a2e-5258-48b6-974f-3656d1af8217')
}

#-- Parameters ----

message('pp_Conversion: Getting parameters')

datasetPP <- swsContext.datasets[[1]]@dataset 
domainPP <-  swsContext.datasets[[1]]@domain 


countryPar <-  swsContext.computationParams$countries
print(countryPar)
if(!is.null(countryPar) & length(countryPar) > 0){
  countryPar <- swsContext.computationParams$countries
  sessionCountry <- strsplit(countryPar, ', ')[[1]]
} else {
  #sessionCountry <- swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys
  countries <- GetCodeList(domainPP, datasetPP, "geographicAreaM49")[ type == 'country']$code
  # Make sure only countries not areas
  sessionCountry <- countries #sessionCountry[sessionCountry %in% countries]
}


productPar <-  swsContext.computationParams$products
print(productPar)
if(!is.null(productPar) & length(productPar) > 0){
  productPar <- swsContext.computationParams$products
  sessionProduct <- strsplit(productPar, ', ')[[1]]
} else {
  #sessionCountry <- swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys
  products <- GetCodeList(domainPP, datasetPP, "measuredItemCPC")$code
  # Make sure only countries not areas
  sessionProduct <- products#sessionCountry[sessionCountry %in% countries]
}

message(paste("Prod Prices: countries selected ", paste0(sessionCountry, collapse = ', '), 
              '. Products selected ', paste0(sessionProduct, collapse = ', '), sep = ''))

# Mandatory year values.
maxyear <- as.numeric(swsContext.computationParams$maxyear)
minyear <- as.numeric(swsContext.computationParams$minyear)
selectedYears <- as.character(minyear:maxyear)

sessionElement <- swsContext.computationParams$element

elementCode <- ifelse(sessionElement == 'LCU', '5530', ifelse(sessionElement == 'SLC', '5531', '5532'))


#-- Pull questionnaire data ----

message('pp_Conversion: Pulling data')

priceKey = DatasetKey(
  domain = domainPP,
  dataset = datasetPP,
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = GetCodeList(domainPP, datasetPP, 'geographicAreaM49')[code %in% sessionCountry, code]),
    Dimension(name = "measuredElement", 
              keys = GetCodeList(domainPP, datasetPP, 'measuredElement')[ code == elementCode, code]),
    Dimension(name = "measuredItemCPC",
              keys = GetCodeList(domainPP, datasetPP, 'measuredItemCPC')[code %in% sessionProduct, code]),
    Dimension(name = "timePointYears", 
              keys = GetCodeList(domainPP, datasetPP, 'timePointYears')[code %in% selectedYears, code]))
  
)


priceData <- GetData(priceKey, flags = TRUE)

lcuByYear <- ReadDatatable('country_year_lcu')


priceWithCurrency <- merge(priceData, lcuByYear, by.x = c('geographicAreaM49', 'timePointYears'),
                           by.y = c('geographicaream49', 'timepointyears'), all.x = T)

# Get Exchange rates
# Fix for Palestine use Israel (376) exchange rate
if(any(sessionCountry == '275')){
  
  erKey = DatasetKey(
    domain = 'common',
    dataset = 'exchange_rates_annual',
    dimensions = list(
      Dimension(name = 'geographicAreaM49',
                keys = GetCodeList('common', 'exchange_rates_annual', 'geographicAreaM49')[code %in% c(sessionCountry, '376') , code]),
      Dimension(name = "from_currency",
                keys = GetCodeList('common', 'exchange_rates_annual', 'from_currency')[code != 'ECU' & endDate >= '1991-01-01' | 
                                                                                         is.na(endDate), code]),
      Dimension(name = "to_currency", 
                keys = GetCodeList('common', 'exchange_rates_annual', 'to_currency')[code == 'USD', code]),
      Dimension(name = 'measuredElement',
                keys = c('LCU')),
      Dimension(name = "timePointYears", 
                keys = GetCodeList('common', 'exchange_rates_annual', 'timePointYears')[code %in% selectedYears, code]))
    
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
                keys = GetCodeList('common', 'exchange_rates_annual', 'geographicAreaM49')[code %in% sessionCountry, code]),
      Dimension(name = "from_currency",
                keys = GetCodeList('common', 'exchange_rates_annual', 'from_currency')[code != 'ECU' & endDate >= '1991-01-01' | 
                                                                                         is.na(endDate), code]),
      Dimension(name = "to_currency", 
                keys = GetCodeList('common', 'exchange_rates_annual', 'to_currency')[code == 'USD', code]),
      Dimension(name = 'measuredElement',
                keys = c('LCU')),
      Dimension(name = "timePointYears", 
                keys = GetCodeList('common', 'exchange_rates_annual', 'timePointYears')[code %in% selectedYears, code]))
    
  )
  
  erdt <- GetData(erKey, flags = F) 
  
}

erdt[,c('to_currency', 'measuredElement')] <- NULL

message('pp_Conversion: Starting computations')

if(sessionElement == 'LCU'){

# Add LCU code
pricesWithBothCurrency <- merge(priceWithCurrency, erdt, 
                                by = c('geographicAreaM49', 'timePointYears'), 
                                suffixes = c('Prices', 'XR'), all.x = T)  

xrcountry <-  ReadDatatable('currency_changes')

# Correct Ghana
xrcountry[new_currency_code == 'GHS' & old_currency_code == 'GHC', exchange_rate := 1e+04]
setkey(xrcountry)
xrcountry <- unique(xrcountry)

pricesCorrection <- merge(pricesWithBothCurrency, xrcountry, 
                          by.x = c('currency_code', 'from_currency'),
                          by.y = c('old_currency_code', 'new_currency_code'), 
                          all.x = T)

pricesCorrection[from_currency != currency_code, XR2apply := ValueXR*exchange_rate]
pricesCorrection[from_currency == currency_code, XR2apply := ValueXR]
pricesCorrection[, ValueUSD := ValuePrices/XR2apply]

# Delete useless columns
pricesCorrection[,c('from_currency', 'ValueXR', 'XR2apply', 'exchange_rate')] <- NULL

# Calculation for SLC

lcuM49 <- ReadDatatable('country_year_lcu')

# Add pre-2006 series for Serbia assigning Serbia-Montenegro currency
#Montenegro pre-2006
serbia05 <- lcuM49[geographicaream49 == '891' & timepointyears < 2006]
serbia05 <- serbia05[, geographicaream49 := '688']
lcuM49 <- rbind(lcuM49, serbia05)
# Change Cuba code
lcuM49[geographicaream49 == '192', currency_code := 'CUX']

xr_corr <- ReadDatatable('exchange_rates_correspondences')

xr_corr[is.na(end_year_iso), end_year_iso := '9999']

last_year_curr <- xr_corr[ , max(end_year_iso), geographicaream49]
setnames(last_year_curr, 'V1', 'end_year_iso') 
last_curr <- xr_corr[last_year_curr, on = c('geographicaream49', 'end_year_iso')][,.(geographicaream49, currency_code_iso)]


# Add SLC code
pricesWithSLC <- merge(pricesCorrection, last_curr, by.x = c('geographicAreaM49'),
                       by.y = c('geographicaream49'), all.x = T)  

setnames(pricesWithSLC, c("currency_code", "currency_code_iso"),
         c("currency_code_lcu", "currency_code_slc"))


pricesWithCurrencyChange <- merge(pricesWithSLC, xrcountry, 
                                  by.x = c("currency_code_lcu", "currency_code_slc"),
                                  by.y = c('old_currency_code', 'new_currency_code'), all.x = T)

pricesWithCurrencyChange[is.na(exchange_rate) & 
                           currency_code_lcu == currency_code_slc, 
                         exchange_rate := 1]

# Calculate SLC
pricesWithCurrencyChange[, ValueSLC := ValuePrices/exchange_rate]

if(pricesWithCurrencyChange[is.na(ValueSLC),.N] > 0){
  msg1 <- paste('No SLC value calculated for country: ', 
                paste(unique(pricesWithCurrencyChange[is.na(ValueSLC)]$geographicAreaM49), collapse = ', '),
                sep = '')
}  else {
  msg1 <- ''
}

pricesWithCurrencyChange[, c("currency_code_lcu",
                             "currency_code_slc",
                             "exchange_rate")] <- NULL

pper <- melt(pricesWithCurrencyChange, measure.vars = c('ValuePrices', 'ValueUSD', 'ValueSLC'),
             value.name = 'Value')
pper[variable == 'ValueUSD', c('measuredElement') := list('5532')]
pper[variable == 'ValueSLC', c('measuredElement') := list('5531')]
pper[ , c('variable')] <- NULL


} else if(sessionElement == 'SLC'){
  
  xrcountry <-  ReadDatatable('currency_changes')
  # Correct Ghana
  xrcountry[new_currency_code == 'GHS' & old_currency_code == 'GHC', exchange_rate := 1e+04]
  setkey(xrcountry)
  xrcountry <- unique(xrcountry)
  
  # Calculation for SLC
  
  lcuM49 <- ReadDatatable('country_year_lcu')
  
  # Add pre-2006 series for Serbia assigning Serbia-Montenegro currency
  #Montenegro pre-2006
  serbia05 <- lcuM49[geographicaream49 == '891' & timepointyears < 2006]
  serbia05 <- serbia05[, geographicaream49 := '688']
  lcuM49 <- rbind(lcuM49, serbia05)
  # Change Cuba code
  lcuM49[geographicaream49 == '192', currency_code := 'CUX']
  
  xr_corr <- ReadDatatable('exchange_rates_correspondences')
  
  xr_corr[is.na(end_year_iso), end_year_iso := '9999']
  
  last_year_curr <- xr_corr[ , max(end_year_iso), geographicaream49]
  setnames(last_year_curr, 'V1', 'end_year_iso') 
  last_curr <- xr_corr[last_year_curr, on = c('geographicaream49', 'end_year_iso')][,.(geographicaream49, currency_code_iso)]
  
  
  # Add SLC code
  pricesWithSLC <- merge(priceWithCurrency, last_curr, by.x = c('geographicAreaM49'),
                         by.y = c('geographicaream49'), all.x = T)  
  
  setnames(pricesWithSLC, c("currency_code", "currency_code_iso"),
           c("currency_code_lcu", "currency_code_slc"))
  
  
  pricesWithCurrencyChange <- merge(pricesWithSLC, xrcountry, 
                                    by.x = c("currency_code_lcu", "currency_code_slc"),
                                    by.y = c('old_currency_code', 'new_currency_code'), all.x = T)
  
  pricesWithCurrencyChange[is.na(exchange_rate) & 
                             currency_code_lcu == currency_code_slc, 
                           exchange_rate := 1]
  
  # Calculate SLC
  pricesWithCurrencyChange[, ValueLCU := Value*exchange_rate]
 
  setnames(pricesWithCurrencyChange, 'Value', 'ValueSLC')
  
  # Add LCU code
  pricesWithBothCurrency <- merge(pricesWithCurrencyChange, erdt, 
                                  by = c('geographicAreaM49', 'timePointYears'), 
                                  suffixes = c('Prices', 'XR'), all.x = T)  
  setnames(pricesWithBothCurrency, 'Value', 'ValueXR')

  
  pricesWithBothCurrency[from_currency != currency_code_lcu, XR2apply := ValueXR*exchange_rate]
  pricesWithBothCurrency[from_currency == currency_code_lcu, XR2apply := ValueXR]
  pricesWithBothCurrency[, ValueUSD := ValueLCU/XR2apply]
  
  # Delete useless columns
  pricesWithBothCurrency[,c('from_currency', 'ValueXR', 'XR2apply', 'exchange_rate',
                            'currency_code_lcu', 'currency_code_slc')] <- NULL
  
   
  if(pricesWithBothCurrency[is.na(ValueSLC),.N] > 0){
    msg1 <- paste('No SLC value calculated for country: ', 
                  paste(unique(pricesWithCurrencyChange[is.na(ValueSLC)]$geographicAreaM49), collapse = ', '),
                  sep = '')
  }  else {
    msg1 <- ''
  }
  
  pper <- melt(pricesWithBothCurrency, measure.vars = c('ValueLCU', 'ValueUSD', 'ValueSLC'),
               value.name = 'Value')
  pper[variable == 'ValueUSD', c('measuredElement') := list('5532')]
  pper[variable == 'ValueSLC', c('measuredElement') := list('5531')]
  pper[variable == 'ValueLCU', c('measuredElement') := list('5530')]
  
  pper[ , c('variable')] <- NULL
  
  

} else if(sessionElement == 'USD'){
  
  # Add LCU code
  pricesWithBothCurrency <- merge(priceWithCurrency, erdt, 
                                  by = c('geographicAreaM49', 'timePointYears'), 
                                  suffixes = c('Prices', 'XR'), all.x = T)  
  
  xrcountry <-  ReadDatatable('currency_changes')
  
  # Correct Ghana
  xrcountry[new_currency_code == 'GHS' & old_currency_code == 'GHC', exchange_rate := 1e+04]
  setkey(xrcountry)
  xrcountry <- unique(xrcountry)
  
  pricesCorrection <- merge(pricesWithBothCurrency, xrcountry, 
                            by.x = c('currency_code', 'from_currency'),
                            by.y = c('old_currency_code', 'new_currency_code'), 
                            all.x = T)
  
  pricesCorrection[from_currency != currency_code, XR2apply := ValueXR*exchange_rate]
  pricesCorrection[from_currency == currency_code, XR2apply := ValueXR]
  pricesCorrection[, ValueLCU := ValuePrices*XR2apply]
  
  # Delete useless columns
  pricesCorrection[,c('from_currency', 'ValueXR', 'XR2apply', 'exchange_rate')] <- NULL
  
  # Calculation for SLC
  
  lcuM49 <- ReadDatatable('country_year_lcu')
  
  # Add pre-2006 series for Serbia assigning Serbia-Montenegro currency
  #Montenegro pre-2006
  serbia05 <- lcuM49[geographicaream49 == '891' & timepointyears < 2006]
  serbia05 <- serbia05[, geographicaream49 := '688']
  lcuM49 <- rbind(lcuM49, serbia05)
  # Change Cuba code
  lcuM49[geographicaream49 == '192', currency_code := 'CUX']
  
  xr_corr <- ReadDatatable('exchange_rates_correspondences')
  
  xr_corr[is.na(end_year_iso), end_year_iso := '9999']
  
  last_year_curr <- xr_corr[ , max(end_year_iso), geographicaream49]
  setnames(last_year_curr, 'V1', 'end_year_iso') 
  last_curr <- xr_corr[last_year_curr, on = c('geographicaream49', 'end_year_iso')][,.(geographicaream49, currency_code_iso)]
  
  
  # Add SLC code
  pricesWithSLC <- merge(pricesCorrection, last_curr, by.x = c('geographicAreaM49'),
                         by.y = c('geographicaream49'), all.x = T)  
  
  setnames(pricesWithSLC, c("currency_code", "currency_code_iso"),
           c("currency_code_lcu", "currency_code_slc"))
  
  
  pricesWithCurrencyChange <- merge(pricesWithSLC, xrcountry, 
                                    by.x = c("currency_code_lcu", "currency_code_slc"),
                                    by.y = c('old_currency_code', 'new_currency_code'), all.x = T)
  
  pricesWithCurrencyChange[is.na(exchange_rate) & 
                             currency_code_lcu == currency_code_slc, 
                           exchange_rate := 1]
  
  # Calculate SLC
  pricesWithCurrencyChange[, ValueSLC := ValueLCU/exchange_rate]
  
  if(pricesWithCurrencyChange[is.na(ValueSLC),.N] > 0){
    msg1 <- paste('No SLC value calculated for country: ', 
                  paste(unique(pricesWithCurrencyChange[is.na(ValueSLC)]$geographicAreaM49), collapse = ', '),
                  sep = '')
  } else {
    msg1 <- ''
  }
  
  pricesWithCurrencyChange[, c("currency_code_lcu",
                               "currency_code_slc",
                               "exchange_rate")] <- NULL
  
  pper <- melt(pricesWithCurrencyChange, measure.vars = c('ValuePrices', 'ValueLCU', 'ValueSLC'),
               value.name = 'Value')
  pper[variable == 'ValueLCU', c('measuredElement') := list('5530')]
  pper[variable == 'ValueSLC', c('measuredElement') := list('5531')]
  pper[ , c('variable')] <- NULL
  
}

message('pp_Conversion: Saving data')

# Prices not converted
missingPrices <- pper[is.na(Value)]

pper1 <- pper[!is.na(Value)]

save <- SaveData(domainPP, datasetPP, pper1)


paste0("Conversion completed successfully!!! ",
       save$inserted, " observations written, ",
       save$ignored, " weren't updated, ",
       save$discarded, " had problems.")

#-- send Email with notification of correct execution ----

mailbody <- paste('Number of inserted values: ', save$inserted,
                  '. Number of ignored values: ', save$ignored, 
                  '. Number of discarded values: ', save$discarded,  
                         '. Problem! ', msg1, sep = '')

from = "sws@fao.org"
to = swsContext.userEmail
subject = "The plug-in has correctly run"
body = list(mailbody, mime_part(missingPrices))
sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)
paste0("Email sent to ", swsContext.userEmail)
