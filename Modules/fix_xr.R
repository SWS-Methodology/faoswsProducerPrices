fix_xr <- function(erdt, xrcountry, xr_corr){
  
  # Get country-currency datatatble from IMF
  xr_corr[is.na(start_year_iso), start_year_iso := '1900']
  xr_corr[is.na(end_year_iso), end_year_iso := '9999']
  
  xr_corr[is.na(start_year_eco_team), start_year_eco_team := '1900']
  xr_corr[is.na(end_year_eco_team), end_year_eco_team := '9999']
  #xr_corr <- xr_corr[end_year_iso >= '1990' & end_year_m49 >= '1990' & end_year_eco_team > '1990']
  
  xr_corr[,c("country", "geographicareaiso3",
              "geographicareaiso2", "start_year_m49",
              "end_year_m49", "currency_name_iso",
              "currency_name_eco_team", "start_date",
              "end_date"  )] <- NULL
  xr_corr[start_year_iso == start_year_eco_team, check_sd := 'ok']
  xr_corr[end_year_iso == end_year_eco_team, check_ed := 'ok']
  
  xr_corr[start_year_iso != start_year_eco_team, check_sd := 'no']
  xr_corr[end_year_iso != end_year_eco_team, check_ed := 'no']

  dtcheck <- merge(erdt, xr_corr, by.x = c('geographicAreaM49', 'from_currency'),
                   by.y = c('geographicaream49', 'currency_code_iso'), all.x = T)
  
  
  # If start years of the two sources do not match check which currency to use

  problems <- dtcheck[timePointYears < start_year_eco_team]
  
  problemcheck <- merge(problems, xr_corr[,.(geographicaream49, currency_code_iso)], by.x = c('geographicAreaM49'),
                    by.y = c('geographicaream49'), all.x = T)
  
  tochange <- problemcheck[timePointYears <= end_year_eco_team & timePointYears >= start_year_eco_team]
  
  setkey(tochange)
  changes <- unique(tochange[, .(from_currency, currency_code_iso)])
  
  # Get fixed exchange rates from one currency to the other
  neededxr <- merge(xrcountry, changes, 
                    by.x = c('new_currency_code', 'old_currency_code'),
                    by.y = c('from_currency', 'currency_code_iso'))
  
  correct <- merge(tochange, neededxr, by.x = c('from_currency', 'currency_code_iso'),
                   by.y = c('new_currency_code', 'old_currency_code'), all.x = T)
  

  # calculate the appropriate XR and insert it in the dataset
  correct[, newXR := Value*exchange_rate]
  
  erdt <- merge(erdt,correct[,.(geographicAreaM49, timePointYears, newXR)], 
                by = c("geographicAreaM49", "timePointYears"), all.x = T)
  
  erdt[!is.na(newXR), Value := newXR]
  erdt[,newXR := NULL]
  
  # check on currency
  if(!all(erdt$from_currency %in% xr_corr$currency_code_iso)){
    stop(paste('Missing country-currency correspondence: ', 
               paste(unique(erdt[!from_currency %in% xr_corr$currency_code_iso]$from_currency), collapse = ' '),
               "not in the 'Exchange Rates Correspondences' datatble. Please update it."))
  }
  
  return(erdt)
  
}
