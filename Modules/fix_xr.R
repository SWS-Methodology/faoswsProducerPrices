fix_xr <- function(erdt, lcu_2_m49, eco_curr0, xrcountry){
  
  # Get country-currency datatatble from IMF

  lcu_2_m49[start_year_iso == '', start_year_iso := '1900']
  lcu_2_m49[end_year_iso == '', end_year_iso := '9999']
  
  # Get country-currency datatatble from ECO team, 
  # i.e. year of effective change of currency in questionnaires
  
  eco_curr <- eco_curr0[,.(country_code_m49, currency_code, start_year, end_year, active_flag)]
  eco_curr <- eco_curr[!is.na(start_year)]
  eco_curr[is.na(end_year) & active_flag == TRUE, end_year := '9999']
  eco_curr[is.na(end_year) & active_flag == FALSE & country_code_m49 == '58', end_year := '1999']
  eco_curr[is.na(end_year) & active_flag == FALSE & country_code_m49 == '58', end_year := '2005']
  
  # Check when end dates do not match
  check <- merge(lcu_2_m49[end_year_iso >= '1990',.(code_m49, code_iso, start_year_iso, end_year_iso)], eco_curr[end_year >= '1990'], 
                 by.x = c('code_m49', 'code_iso'),
                 by.y = c('country_code_m49', 'currency_code'), all = TRUE)
  
  check[ end_year == end_year_iso, check_ed := 'ok']
  check[ start_year == start_year_iso, check_sd := 'ok']
  
  check[ end_year != end_year_iso, check_ed := 'no']
  check[ start_year != start_year_iso, check_sd := 'no']
  
  # Merge XR dataset with table from ECO team
  dtcheck <- merge(erdt, eco_curr, by.x = c('geographicAreaM49', 'from_currency'),
                   by.y = c('country_code_m49', 'currency_code'), all.x = T)
  
  # If start years of the two sources do not match check which currency to use
  problems <- dtcheck[timePointYears < start_year]
  prova <-  merge(problems, eco_curr, by.x = c('geographicAreaM49'),
                  by.y = c('country_code_m49'), all.x = T)
  tochange <- prova[timePointYears <= end_year.y & timePointYears >= start_year.y]
  
  tochange[from_currency != currency_code]
  setkey(tochange)
  changes <- unique(tochange[, .(from_currency, currency_code)])
  
  # Get fixed exchange rates from one currency to the other
  
  neededxr <- merge(xrcountry, changes, 
                    by.x = c('new_currency_code', 'old_currency_code'),
                    by.y = c('from_currency', 'currency_code'))
  
  correct <- merge(tochange, neededxr, by.x = c('from_currency', 'currency_code'),
                   by.y = c('new_currency_code', 'old_currency_code'), all.x = T)
  
  # calculate the appropriate XR and insert it in the dataset
  correct[, newXR := Value*exchange_rate]
  
  erdt <- merge(erdt,correct[,.(geographicAreaM49, timePointYears, newXR)], 
                by = c("geographicAreaM49", "timePointYears"), all.x = T)
  
  erdt[!is.na(newXR), Value := newXR]
  erdt[,newXR := NULL]
  
  # check on currency
  if(!all(erdt$from_currency %in% lcu_2_m49$code_iso)){
    stop(paste('Missing countey-currency correspondence: ', 
               unique(erdt[!from_currency %in% lcu_2_m49$code_iso]$from_currency),
               'not in the lcu_2_m49 datatble. Please update it.'))
  }
  
  return(erdt)
  
}