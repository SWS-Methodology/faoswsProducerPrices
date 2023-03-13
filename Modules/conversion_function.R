

convert_currency <- function(priceData, erdt, sessionElement){
  
  
  
  lcuByYear <- ReadDatatable('country_year_lcu')

  
  priceWithCurrency <- merge(priceData, lcuByYear, by.x = c('geographicAreaM49', 'timePointYears'),
                             by.y = c('geographicaream49', 'timepointyears'), all.x = T)
  
  
  
  
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
      msg1 <- paste('No SLC value caluclated for country: ', 
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
      msg1 <- paste('No SLC value caluclated for country: ', 
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
      msg1 <- paste('No SLC value caluclated for country: ', 
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

  return(pper)

}

