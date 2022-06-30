# Function to return the best model

bestModel <- function(dt, covariates,tseries, startYearForecast){
  
    # Covariates available
    vec2comb <- names(covariates)[4:ncol(covariates)]
    
    # Create a list with possible covariate combinations
    dfcomb <- list()
    
    for(h in 1:length(vec2comb)){
      dfcomb[[h]] <- as.data.frame(t(combn(vec2comb, h)))
    }
    
    covcomb <- rbindlist(dfcomb, use.names = T, fill = T)
    
    # Models using the possible combinations in covcomb
    modlist <- list()
    #lmlist <- list()
    
    for(j in 1:nrow(covcomb)){
      cov2use <- as.matrix(covcomb[j,])
      xreg2use <- covariates[timePointYears %in% time(tseries), cov2use[!is.na(cov2use)], with = F]
      
      modj <- tryCatch(auto.arima(tseries, seasonal = FALSE, xreg = xreg2use, allowdrift = F),
                       error = function(error_condition) { auto.arima(tseries, seasonal = FALSE)})
      modlist[[j]] <- modj
      
    }
    
    # Best ARIMAX with lowest BIC
    if(length(!lapply(modlist, function(x){x$bic}) %in% c(Inf, -Inf)) == 0){
      modlist <- NULL
    } else {
      modlist <- modlist[which(!lapply(modlist, function(x){x$bic}) %in% c(Inf, -Inf))]
    }
    
    bics <- unlist(lapply(modlist, function(x){x$bic}))
    
    mod2sel <- which(bics == min(bics))
    
    return(list(modlist = modlist, mod2sel = mod2sel))
  }
  