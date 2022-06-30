
classifySeries <- function(prep_price0, minyear, maxyear){
  
  lastyear <- as.character(as.numeric(format(Sys.Date(), '%Y')))
  
  sel_years <- as.character(minyear:maxyear)
  impYear <- max(sel_years)#c(as.numeric(lastyear) - 1, max(prep_price0$timePointYears)))
  
# Get space for imputations
prep_price_exp <- expandYear(prep_price0, #[timePointYears %in% sel_years], 
                             newYears = as.numeric(impYear))

all <- data.table(expand.grid(measuredElement =unique(prep_price_exp$measuredElement),
                              geographicAreaM49 = unique(prep_price_exp$geographicAreaM49),
                              measuredItemCPC =unique(prep_price_exp$measuredItemCPC),
                              timePointYears=as.numeric(sel_years)))
all$timePointYears <- as.character(all$timePointYears)
prep_price_exp <- merge(prep_price_exp, all,
               by =c('measuredElement', 'geographicAreaM49', 'timePointYears','measuredItemCPC'), all = T)

#prep_price_exp[timePointYears %in% (minyear-1):(minyear-2), ]
m49 <- GetCodeList(domainPP, datasetPrep, 'geographicAreaM49')

prep_price_exp <- merge(prep_price_exp, m49[,.(code, startDate,  endDate)], by.x = 'geographicAreaM49', by.y = 'code', all.x = T)

prep_price_exp[is.na(startDate) & !is.na(endDate)][timePointYears <= substr(endDate, 1,4)]
prep_price_exp[!is.na(startDate) & is.na(endDate)][timePointYears >= substr(startDate, 1,4)]


prep_price_exp2 <- rbind(prep_price_exp[is.na(startDate) & is.na(endDate)], 
                         prep_price_exp[is.na(startDate) & !is.na(endDate)][timePointYears <= substr(endDate, 1,4)],
                         prep_price_exp[!is.na(startDate) & is.na(endDate)][timePointYears >= substr(startDate, 1,4)])

prep_price_exp2[, c('startDate', 'endDate')] <- NULL

prep_price <- prep_price_exp2 #rbind(prep_price_exp, prep_price0[!timePointYears %in% sel_years])

# Re-impute all previous imputations
prep_price[timePointYears %in% sel_years & !flagObservationStatus %in% c('', 'X') , c('Value',
                                                                                      'flagObservationStatus',
                                                                                      'flagMethod') :=list(NA, 'M', 'u')]

# If no official or imputed data before the 'M;u' then the series is not imputed
# If one year not in the selected ones is missing the system ignore them
setkey(prep_price)

firstReportedYear <- unique(prep_price[!is.na(Value), minRepY := min(as.numeric(timePointYears)), .(geographicAreaM49, measuredItemCPC)][,.(geographicAreaM49, measuredItemCPC, minRepY)])
firstReportedYear <- firstReportedYear[!is.na(minRepY)]
prep_price$minRepY <- NULL

lastReportedYear <- unique(prep_price[!is.na(Value), maxRepY := max(as.numeric(timePointYears)), .(geographicAreaM49, measuredItemCPC)][,.(geographicAreaM49, measuredItemCPC, maxRepY)])
lastReportedYear <- lastReportedYear[!is.na(maxRepY)]
prep_price$maxRepY <- NULL

prep_price <- merge(prep_price, firstReportedYear, 
                    by =c("geographicAreaM49", "measuredItemCPC"), all.x = T,
                    allow.cartesian = T)

prep_price <- merge(prep_price, lastReportedYear, 
                    by = c("geographicAreaM49", "measuredItemCPC"), all.x = T,
                    allow.cartesian = T)

# If the first available data and the latest are equal then impossible to impute

prep_price <- prep_price[minRepY != maxRepY]

# Function to distinguish series to forecast and retropolate
more_nas_first <- function(x){sum(is.na(x)[(1:length(x)) < (length(x) / 2)]) > sum(is.na(x)[(1:length(x)) >= (length(x) / 2)])}

# If the first official figure is in the years to revise then 
# A check whether retropolation or forecast must be done
sel_years_prep_price <- prep_price[ minRepY %in% sel_years]

if(sel_years_prep_price[,.N] > 0){
  
  sel_years_prep_price[timePointYears %in% sel_years , retro := more_nas_first(Value), by = c("geographicAreaM49", "measuredItemCPC")]
  setkey(sel_years_prep_price)
  # Year of reporting from which retropolation can be done
  year2retrop <- as.numeric(format(Sys.Date(),'%Y'))-8
  series2forecast <- rbind(unique(sel_years_prep_price[retro == FALSE,
                                                       .(geographicAreaM49, measuredItemCPC)]),
                           unique(sel_years_prep_price[retro == TRUE & minRepY <= year2retrop,
                                                       .(geographicAreaM49, measuredItemCPC)]))
  series2ertropolate <- unique(sel_years_prep_price[retro == TRUE & minRepY > year2retrop,
                                                    .(geographicAreaM49, measuredItemCPC)])
  
  # Forecast series with first reporting year before starting date of the revision
  # or series identified by the function
  prices2forecast <- rbind(prep_price[ minRepY < min(sel_years)],
                           prep_price[series2forecast, 
                                      on = c('geographicAreaM49', 'measuredItemCPC')])
  
  # Retropolate series with first reporting year after end date of the revision
  # or series identified by the function
  prices2retropolate <- rbind(prep_price[ minRepY > max(sel_years)],
                              prep_price[series2ertropolate, 
                                         on = c('geographicAreaM49', 'measuredItemCPC')])
  
  series2forecastComplete <- unique(prices2forecast[,.(geographicAreaM49, measuredItemCPC)])
  series2retropolateComplete <- unique(prices2retropolate[,.(geographicAreaM49, measuredItemCPC)])
  
  # Remove in series to forecast previous NAs values, i.e. values before the first reported year
  
  todelete <- prices2forecast[is.na(Value) & timePointYears < minRepY]
  
  prep_price <- rbind(prices2forecast[!todelete, on = c("geographicAreaM49", "measuredItemCPC", "timePointYears")],
                      prices2retropolate)
} else {
  
  series2forecastComplete <- unique(prep_price[,.(geographicAreaM49, measuredItemCPC)])
  
}
prep_price[, minRepY := NULL]
prep_price[, maxRepY := NULL]

return(list(completedSeries = prep_price,  series2forecast = series2forecastComplete))

}


# -- arimaxRevision ----

arimaxRevision <- function(series_comm, series2forecastComplete, minyear, maxyear){

  message('PP_imputationMethods: start imputation')

# IF row in series to forecast
if(nrow(match_df(unique(series_comm[,.(geographicAreaM49, measuredItemCPC)]), series2forecastComplete)) == 1){
  series_comm <- series_comm
  sel_years <- as.character(minyear:maxyear)
  
} else {
  # next 
  series_comm <- series_comm[, c('timePointYears') := list(rev(timePointYears))]
  series_comm <- series_comm[order(timePointYears),]
  sel_years <- min(series_comm[is.na(Value)]$timePointYears):max(series_comm$timePointYears)
}

# Check no column is completely missing and ignore it incase
series_comm <- series_comm[,colSums(is.na(series_comm))<nrow(series_comm), with = F]

# Possible interpolation needed if official values inside the series
# missingyear <- series_comm[timePointYears %in% sel_years,]$timePointYears

# Case where there are official data in the period to review
if(series_comm[timePointYears %in% sel_years & flagObservationStatus %in%  c('', 'X'),.N] > 0){
  
  #### Official data available in the period to review ####  
  
  # Official years in the series
  yearsOff <- series_comm[timePointYears %in% sel_years & flagObservationStatus %in%  c('', 'X'),]$timePointYears
  
  # Series for arima model: time series until revision start
  if(series_comm[timePointYears < min(sel_years),.N] > 0){
    ppseries <- ts(series_comm[timePointYears < min(sel_years)]$LogValue,
                   start = as.numeric(min(series_comm$timePointYears)))
  } else {
    ppseries <- ts(series_comm[!is.na(Value)]$LogValue,
                   start = as.numeric(min(series_comm[!is.na(Value)]$timePointYears)))
  }
  
  # Covariate set complete all years
  xreg_comm <- series_comm[  , names(series_comm) %in% c("timePointYears", "geographicAreaM49", "measuredItemCPC",    
                                                         "LogGDP", "LogVA", "ValueYield", "toi_aff"), with = F]
  
  # Number of missing points per covariate for needed years
  missing <- apply(xreg_comm[ , names(xreg_comm) %in% c("LogGDP", "LogVA", "ValueYield", "toi_aff"), with = F], 
                   2, function(x){length(x[is.na(x)])})
  
  # If the number of missing data is the same for all variables
  # select all variables
  if(max(missing) == min(missing)){
    xreg_comm <- xreg_comm
  } else { # select only the most complete ones
    xreg_comm <- xreg_comm[ , c("timePointYears", "geographicAreaM49", "measuredItemCPC",    
                                names(missing[missing == min(missing)])), with = F ]
  }
  
  if(length(names(xreg_comm)) > 3){
    #### No off data in the period and covariates available ####
    # Exclude columns with NAs
    xreg_comm <- xreg_comm[, colSums(is.na(xreg_comm[timePointYears %in% 
                                                       c(as.vector(min(time(ppseries))):max(sel_years))])) < 1,
                           with = F]
    
    # missingyear <- series_comm[timePointYears %in% sel_years,]$timePointYears
    startYearForecast <- max(time(ppseries))+1
    # Covariates available
    bestModelInfo <- bestModel(dt = series_comm, covariates = xreg_comm, tseries = ppseries,
                               startYearForecast = startYearForecast)
    mod2sel <- bestModelInfo$mod2sel
    modlist <- bestModelInfo$modlist
    
    if(length(modlist) > 0){ # If the best model exist
      
      bestmod <- modlist[[mod2sel[1]]]
      
      if(all(arimaorder(bestmod) == 0)){
        # If ARIMA order (0, 0, 0)
        # use function with auto.arima
        pred <- na.kalman(series_comm$LogValue, model = "auto.arima", allowdrift = F)
        
        # If again zeros predicted
        if(any(pred == 0)){
          pred <- na.approx(series_comm$LogValue) 
          
          if(length(pred) != length(na.kalman(series_comm$LogValue, model = "auto.arima", allowdrift = F))){
            
            nt <- length(na.kalman(series_comm$LogValue, model = "auto.arima", allowdrift = F)) - length(pred)
            
            ppseries2 <- ts(pred, start = as.numeric(min(series_comm$timePointYears)))
            startYearForecast2 <- max(time(ppseries2))+1
            bestModelInfo2 <- bestModel(dt = series_comm, covariates = xreg_comm, tseries = ppseries2,
                                        startYearForecast = startYearForecast2)
            mod2sel2 <- bestModelInfo2$mod2sel
            modlist2 <- bestModelInfo2$modlist
            bestmod2 <- modlist2[[mod2sel2[1]]]
            
            varselected2 <- names(bestmod2$coef)[names(bestmod2$coef) %in% names(xreg_comm)]
            
            if(length(varselected2) > 0){ # If there are covariates in the best model
              xreg_comm_pred2 <- series_comm[timePointYears > max(time(ppseries2)) , varselected2, with = F]
              remainingForecast <- forecast(bestmod2, h = nt, xreg = xreg_comm_pred2)$mean
            } else { # If there is no covariate in the best model
              remainingForecast <- forecast(bestmod2, h = nt)$mean
            }
            
            
            pred <- c(pred, remainingForecast)
            
          }
        }
      } else {
        # If ARIMA order different from (0, 0, 0)
        
        varselected <- names(bestmod$coef)[names(bestmod$coef) %in% names(xreg_comm)]
        
        if(length(varselected) > 0){ # If there are covariates in the best model
          xreg_comm_pred <- series_comm[(flagMethod == 'u' & timePointYears < max(as.numeric(sel_years))) | (timePointYears %in% sel_years), 
                                        varselected, with = F]
          pred <- na.kalman(series_comm$LogValue, model = bestmod$model, xreg =  xreg_comm_pred) #forecast(bestmod, h = nh, xreg = xreg_comm_pred)$mean
        } else { # If there is no covariate in the best model
          pred <- na.kalman(series_comm$LogValue, model = bestmod$model) #forecast(bestmod, h = nh)$mean
          
        }
      }
    } else { # If best model does not exist as if order is (0, 0, 0)
      
      pred <- na.kalman(series_comm$LogValue, model = "auto.arima", allowdrift = F)
      
      # If again zeros predicted
      if(any(pred == 0)){
        pred <- na.approx(series_comm$LogValue) 
        if(length(pred) != length(na.kalman(series_comm$LogValue, model = "auto.arima", allowdrift = F))){
          
          nt <- length(na.kalman(series_comm$LogValue, model = "auto.arima", allowdrift = F)) - length(pred)
          
          ppseries2 <- ts(pred, start = as.numeric(min(series_comm$timePointYears)))
          startYearForecast2 <- max(time(ppseries2))+1
          bestModelInfo2 <- bestModel(dt = series_comm, covariates = xreg_comm, tseries = ppseries2,
                                      startYearForecast = startYearForecast2)
          mod2sel2 <- bestModelInfo2$mod2sel
          modlist2 <- bestModelInfo2$modlist
          bestmod2 <- modlist2[[mod2sel2[1]]]
          
          if(length(modlist2) > 0){
            varselected2 <- names(bestmod2$coef)[names(bestmod2$coef) %in% names(xreg_comm)]
            
            if(length(varselected2) > 0){ # If there are covariates in the best model
              xreg_comm_pred2 <- series_comm[ , varselected2, with = F]
              remainingForecast <- forecast(bestmod2, h = nt, xreg = xreg_comm_pred2[timePointYears > max(time(ppseries2)), ])$mean
            } else { # If there is no covariate in the best model
              remainingForecast <- forecast(bestmod2, h = nt)$mean
            }
            
          } else { # If no model has been found
            # No result will be available
            remainingForecast <-  rep(NA, nt)
          }
          
          pred <- c(pred, remainingForecast)
          
        }
      }
      
    }
    
  } else {
    pred <- na.kalman(series_comm$LogValue, model = "auto.arima", allowdrift = F)
    
    # If again zeros predicted
    if(any(pred == 0)){
      pred <- na.approx(series_comm$LogValue)
    }
    
  }
  
  arimaxEst <- exp(pred)
  
  
} else {
  
  # Series for arima model: time series until revision start
  if(series_comm[timePointYears < min(sel_years),.N] > 0){
    ppseries <- ts(series_comm[timePointYears < min(sel_years) & !is.na(Value)]$LogValue,
                   start = as.numeric(min(series_comm$timePointYears)))
  } else {
    ppseries <- ts(series_comm[!is.na(Value)]$LogValue,
                   start = as.numeric(min(series_comm[!is.na(Value)]$timePointYears)))
  }
  
  # Covariate set complete all years
  xreg_comm <- series_comm[  , names(series_comm) %in% c("timePointYears", "geographicAreaM49", "measuredItemCPC",    
                                                         "LogGDP", "LogVA", "ValueYield", "toi_aff"), with = F]
  
  # Number of missing points per covariate for needed years
  missing <- apply(xreg_comm[timePointYears %in% c(as.vector(min(time(ppseries))):max(sel_years))  , 
                             names(xreg_comm) %in%c("LogGDP", "LogVA", "ValueYield", "toi_aff"), with = F], 2, function(x){length(x[is.na(x)])})
  
  # If the number of missing data is the same for all variables
  # select all variables
  if(max(missing) == min(missing)){
    xreg_comm <- xreg_comm
  } else { # select only the most complete ones
    xreg_comm <- xreg_comm[ , c("timePointYears", "geographicAreaM49", "measuredItemCPC",    
                                names(missing[missing == min(missing)])), with = F ]
  }
  
  if(length(names(xreg_comm)) > 3){
    #### No off data in the period and covariates available ####
    # Exclude columns with NAs
    xreg_comm <- xreg_comm[timePointYears %in% c(as.vector(min(time(ppseries))):max(sel_years)), 
                           colSums(is.na(xreg_comm[timePointYears %in% c(as.vector(min(time(ppseries))):max(sel_years))])) < 1, with = F]
    
    # missingyear <- series_comm[timePointYears %in% sel_years,]$timePointYears
    startYearForecast <- max(time(ppseries))+1
    # Covariates available
    bestModelInfo <- bestModel(dt = series_comm, covariates = xreg_comm, tseries = ppseries,
                               startYearForecast = startYearForecast)
    mod2sel <- bestModelInfo$mod2sel
    modlist <- bestModelInfo$modlist
    
    #-- ARIMA forescast ----
    nh <-  as.numeric(max(sel_years)) - max(time(ppseries))
    
    # Covariates for predictive years
    if(length(modlist) > 0){ # If the best model exist
      bestmod <- modlist[[mod2sel[1]]]
      varselected <- names(bestmod$coef)[names(bestmod$coef) %in% names(xreg_comm)]
      
      if(length(varselected) > 0){ # If there are covariates in the best model
        xreg_comm_pred <- series_comm[(flagMethod == 'u' & timePointYears < max(as.numeric(sel_years))) | (timePointYears %in% sel_years), 
                                      varselected, with = F]
        pred <- forecast(bestmod, h = nh, xreg = xreg_comm_pred)$mean
      } else { # If there is no covariate in the best model
        pred <- forecast(bestmod, h = nh)$mean
      }
      
      
    } else { # If no model has been found
      # No result will be available
      pred <-  ts(rep(NA, nh), 
                  start = as.numeric(as.numeric(min(series_comm[flagMethod == 'u']$timePointYears ))))
    }
    
    
  }else{ 
    
    #### No off data in the period and covariates NOT available ####
    
    nh <- series_comm[(flagMethod == 'u' & timePointYears <= max(as.numeric(sel_years))) | (timePointYears %in% sel_years),.N]
    bestmod <- auto.arima(ppseries, allowdrift = F)
    pred <- forecast(bestmod, h = nh)$mean
  }
  
  arimaxEst <- exp(c(series_comm[!is.na(Value) & timePointYears < min(sel_years)]$LogValue, 
                     pred,
                     series_comm[timePointYears > max(sel_years)]$LogValue))
  
}

data2add <- series_comm[ , Value := arimaxEst]
data2add <- data2add[, .(geographicAreaM49, measuredItemCPC, measuredElement, timePointYears, Value,
                         flagObservationStatus, flagMethod)]

if(nrow(match_df(unique(series_comm[,.(geographicAreaM49, measuredItemCPC)]), series2forecastComplete)) == 1){
  data2add <- data2add
  
} else {
  # next 
  data2add <- data2add[, c('timePointYears') := list(rev(timePointYears))]
  data2add <- data2add[order(timePointYears),]

  }

return(data2add)

}


# -- linearRevision ----


linearRevision <- function(series_comm_lm, minyear, maxyear, series2forecastComplete){
  
  # IF row in series to forecast
  if(nrow(match_df(unique(series_comm_lm[,.(geographicAreaM49, measuredItemCPC)]), series2forecastComplete)) == 1){
    series_comm_lm <- series_comm_lm
    sel_years <- as.character(minyear:maxyear)
    
  } else {
    # next 
    series_comm_lm <- series_comm_lm[, c('timePointYears') := list(rev(timePointYears))]
    series_comm_lm <- series_comm_lm[order(timePointYears),]
    sel_years <- min(series_comm_lm[is.na(Value)]$timePointYears):max(series_comm_lm$timePointYears)
  }
  
  prepcov <- copy(series_comm_lm)
  
  series_comm_lm$timePointYears <- as.numeric(series_comm_lm$timePointYears)
  sel_years <- as.character(minyear:maxyear)
  yearsOff <- prepcov[timePointYears %in% sel_years & flagObservationStatus %in% protectedFlags,]$timePointYears
  # Possible interpolation needed if official values inside the series
  missingyear <- prepcov[timePointYears %in% sel_years,]$timePointYears
  
  if(prepcov[timePointYears %in% sel_years & flagObservationStatus %in% protectedFlags,.N] > 0){
    
    yearsOff <- prepcov[timePointYears %in% sel_years & flagObservationStatus %in% protectedFlags,]$timePointYears
    
    # If the last value is official then all the series has to be interpolated ----
    
    if(max(sel_years) == max(yearsOff)){
      
      ppseries <- ts(prepcov$LogValue, start = min(as.numeric(prepcov$timePointYears)))
      
      predInterplm <- na.approx(ppseries)
      
      lmEst <- exp(predInterplm)
      
      predlm <- predInterplm[time(predInterplm) %in% sel_years]
      # If the last value is not official then series has to be forecast from the last official year excluded onwards ----  
    } else if (max(sel_years) > max(yearsOff)){
      
      # Years to forecast
      startYearForecast <- min(prepcov[timePointYears %in% sel_years & 
                                         timePointYears > max(yearsOff)]$timePointYears, 
                               na.rm = T)
      
      # Years to inteprolate
      years2interpolate <- prepcov[timePointYears %in% sel_years & timePointYears < max(yearsOff)]$timePointYears
      
      # Interpolation
      ppseries <- ts(prepcov[timePointYears <= max(yearsOff)]$LogValue, start = min(as.numeric(prepcov$timePointYears)))
      
      series_comm_lm[timePointYears <= max(yearsOff), LogValue := na.approx(LogValue)]
      
      # Start ARIMAX estimation and forecast
      # Covariate set Exclude Year, country and commodity (columns 1:3)
      xreg_comm <- prepcov[  , names(prepcov) %in% c("timePointYears", "geographicAreaM49", "measuredItemCPC",    
                                                     "LogGDP", "LogVA", "ValueYield", "toi_aff"), with = F]
      
      missing <- apply(xreg_comm[ , 4:ncol(xreg_comm), with = F], 2, function(x){length(x[is.na(x)])})
      if(max(missing) == min(missing)){
        xreg_comm <- xreg_comm
      } else {
        xreg_comm <- xreg_comm[ , c("timePointYears", "geographicAreaM49", "measuredItemCPC",    
                                    names(missing[missing == min(missing)])), with = F ]
      }
      
      
      if(length(names(xreg_comm)) > 3){
        # Exclude columns with NAs
        xreg_comm <- xreg_comm[ , colSums(is.na(xreg_comm)) < nrow(prepcov), with = F]
        
        missingyear <- prepcov[timePointYears >= startYearForecast]$timePointYears #%in% sel_years,]$timePointYears
        
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
          xreg2use <- xreg_comm[timePointYears %in% time(ppseries), cov2use[!is.na(cov2use)], with = F]
          
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
        
        # Dataset for predictive years
        
        series_comm_lm_pred <- copy(series_comm_lm)
        series_comm_lm_pred <- series_comm_lm_pred[timePointYears %in% sel_years & timePointYears >= startYearForecast]
        series_comm_lm_pred$timePointYears <- as.numeric(series_comm_lm_pred$timePointYears)
        nh <- series_comm_lm[(flagMethod == 'u' & timePointYears <= max(as.numeric(sel_years))),.N]
        
        if(length(BICnotInf) > 0){
          bestlm <- lmlist[[which(unlist(lapply(lmlist, function(x){BIC(x)})) == 
                                    min(BICnotInf))[1]]]
          predlm <- forecast(bestlm, h = nh, series_comm_lm_pred)$mean
        } else {
          predlm <- rep(NA, nh)
        }
        
        
      } else{ # If no available covariate
        
        predlm <- rep(NA, length(sel_years))
      }
      
      lmEst <- exp(c(series_comm_lm[timePointYears <= max(yearsOff)]$LogValue, predlm))
      
    }
    
    
  } else {
    
    
    # Series for arima model: time series until btn_start_year
    ppseries <- ts(prepcov[timePointYears < minyear]$LogValue,
                   start = as.numeric(min(prepcov$timePointYears)))
    
    # validate(need(length(ppseries[!is.na(ppseries)]) > 2 ), 
    #           "Not enough data to calculate the series")
    #
    
    # Covariate set Exclude Year, country and commodity (columns 1:3)
    xreg_comm <- prepcov[  , names(prepcov) %in% c("timePointYears", "geographicAreaM49", "measuredItemCPC",    
                                                   "LogGDP", "LogVA", "ValueYield", "toi_aff"), with = F]
    
    missing <- apply(xreg_comm[ , 4:ncol(xreg_comm), with = F], 2, function(x){length(x[is.na(x)])})
    if(max(missing) == min(missing)){
      xreg_comm <- xreg_comm
    } else {
      xreg_comm <- xreg_comm[ , c("timePointYears", "geographicAreaM49", "measuredItemCPC",    
                                  names(missing[missing == min(missing)])), with = F ]
    }
    
    if(length(names(xreg_comm)) > 3){
      # Exclude columns with NAs
      xreg_comm <- xreg_comm[ , colSums(is.na(xreg_comm)) < nrow(prepcov), with = F]
      
      missingyear <- prepcov[timePointYears %in% sel_years,]$timePointYears
      
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
        xreg2use <- xreg_comm[timePointYears %in% time(ppseries), cov2use[!is.na(cov2use)], with = F]
        
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
      
      # Dataset for predictive years
      
      series_comm_lm_pred <- copy(prepcov)
      series_comm_lm_pred <- series_comm_lm_pred[timePointYears %in% sel_years]
      series_comm_lm_pred$timePointYears <- as.numeric(series_comm_lm_pred$timePointYears)
      nh <- series_comm_lm[(flagMethod == 'u' & timePointYears <= max(as.numeric(sel_years))),.N]
      
      if(length(BICnotInf) > 0){
        bestlm <- lmlist[[which(unlist(lapply(lmlist, function(x){BIC(x)})) == 
                                  min(BICnotInf))[1]]]
        predlm <- forecast(bestlm, h = nh, series_comm_lm_pred)$mean
      } else {
        predlm <- rep(NA, nh)
      }
      
      
    } else{
      predlm <- rep(NA, length(sel_years))
    }
    
    lmEst <- c(series_comm_lm[timePointYears < min(sel_years)]$Value, exp(predlm))
    
  }
  
  
  if(nrow(match_df(unique(series_comm_lm[,.(geographicAreaM49, measuredItemCPC)]), series2forecastComplete)) == 1){
    lmEst <- lmEst
   
  } else {
    # next 
    lmEst <- rev(lmEst)
    }
  
  
  return(as.vector(lmEst)) 
}

