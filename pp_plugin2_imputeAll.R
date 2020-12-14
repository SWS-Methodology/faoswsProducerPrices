#-- Load Packages ----

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
})

#-- Token QA ----

if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = 'c9cd8ef0-5dee-49ed-8b40-93c96c917fdf')
}

# Function for consecutive NAs
consecna <- function(x) {
  # function to identify elements with n or more consecutive NA values
  y <- rle(is.na(x))
  yy <- data.table(lengths = y$lengths, values = y$values)
  yy <- yy[values == TRUE]
  a <- as.integer(max(yy$length))
  return(a)
}



#-- Pull preparation data ----

domainPP <- 'prod_prices'
datasetVal <- 'annual_producer_prices_validated'
datasetPrep <- 'annual_producer_prices_prep' #'annual_producer_prices_quest'

preppriceKey = DatasetKey(
  domain = domainPP,
  dataset = datasetPrep,
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = GetCodeList(domainPP, datasetPrep, 'geographicAreaM49')[, code]),
    Dimension(name = "measuredElement", 
              keys = GetCodeList(domainPP, datasetPrep, 'measuredElement')[, code]),
    Dimension(name = "measuredItemCPC",
              keys = GetCodeList(domainPP, datasetPrep, 'measuredItemCPC')[, code]),
    Dimension(name = "timePointYears", 
              keys = GetCodeList(domainPP, datasetPrep, 'timePointYears')[, code]))
  
)

prep_price <- GetData(preppriceKey, flags = TRUE)


#############

prep_price <- readRDS('ppannualcovclean.rds')
prep_price <- as.data.table(prep_price)
prep_price[,c('flagObservationStatus', 'flagMethod') := list('','-')]
prep_price[is.na(Value),c('flagObservationStatus', 'flagMethod') := list('M','u')]
setnames(prep_price, 'Year', 'timePointYears')
prep_price[,c('ValueGDP', 'ValueVA', 'ValueYield')] <- NULL

# temp <- readRDS('annualtemperature.rds')
# temp <- temp[, year := as.character(year)]
# rain <- readRDS('annualrainfall.rds')
# rain <- rain[, year := as.character(year)]

#-- Auxiliary variable ----

# Macro Indicators
GDP_VAcode <- c('8005', '8028')

MIKey = DatasetKey(
  domain = 'macro_stats',
  dataset = 'ess_eco_macroind_complete',
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = GetCodeList('macro_stats', 
                                 'ess_eco_macroind_complete', 
                                 'geographicAreaM49')[, code]),
    Dimension(name = "measuredElementGdp", 
              keys = GetCodeList('macro_stats', 
                                 'ess_eco_macroind_complete', 
                                 'measuredElementGdp')[code %in% GDP_VAcode, code]),
    Dimension(name = "timePointYears", 
              keys = GetCodeList('macro_stats', 
                                 'ess_eco_macroind_complete', 
                                 'timePointYears')[code %in% unique(prep_price$timePointYears), code]))
  )

macro_ind <- GetData(MIKey, flags = TRUE)
macro_ind <- dcast(macro_ind, geographicAreaM49 + timePointYears ~ measuredElementGdp, 
                   value.var = 'Value' )

setnames(macro_ind, c('8005', '8028'), c('ValueGDP', 'ValueVA'))

prep12 <- merge(prep_price, macro_ind, by = c("geographicAreaM49","timePointYears"),
                all.x = T)

prep12[!is.na(ValueGDP) , LogGDP := log(ValueGDP)]
prep12[!is.na(ValueVA) , LogVA := log(ValueVA)]

# Yield
yieldcode <- '5421'

yieldKey = DatasetKey(
  domain = 'agriculture',
  dataset = 'aproduction',
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = GetCodeList('agriculture', 
                                 'aproduction', 
                                 'geographicAreaM49')[, code]),
    Dimension(name = "measuredElement", 
              keys = GetCodeList('agriculture', 
                                 'aproduction', 
                                 'measuredElement')[code == yieldcode, code]),
    Dimension(name = "measuredItemCPC",
              keys = GetCodeList('agriculture', 'aproduction', 'measuredItemCPC')[, code]),
    Dimension(name = "timePointYears", 
              keys = GetCodeList('agriculture', 
                                 'aproduction', 
                                 'timePointYears')[code %in% unique(prep_price$timePointYears), code]))
)

yield <- GetData(yieldKey, flags = TRUE)
setnames(yield, 'Value', 'ValueYield')
yield[, c("measuredElement", "flagObservationStatus", "flagMethod")] <- NULL

prep123 <- merge(prep12, yield, 
                 by = c("geographicAreaM49", 
                        "timePointYears",
                        "measuredItemCPC"), all.x = T)

# TOI

toi <- ReadDatatable('toi_data')

prepcov <- merge(prep123, toi, by.x = c("geographicAreaM49", 
                                        "timePointYears"),
                 by.y = c("geographicaream49", 
                          "timepointyears"), all.x = T)

# nlppgeoarea <- merge(prep123, temp, by.x = c('geographicAreaM49', 'timePointYears'),
#                      by.y = c('m49_code', 'year'), all.x = T)
# 
# nlppgeoarea <- merge(nlppgeoarea, rain, by.x = c('geographicAreaM49', 'Year'),
#                      by.y = c('m49_code', 'year'), all.x = T)
# nlppgeoarea <- nlppgeoarea[Year <= '2016']

#-- NAs identification ----
prepcov[, nmiss := length(Value[is.na(Value)]), by = c("geographicAreaM49", "measuredItemCPC")]

prepcov[ , cmiss := consecna(Value),
             by = c("geographicAreaM49", "measuredItemCPC")]

prepshort <- copy(prepcov[cmiss <= 5])

preplong <- copy(prepcov[cmiss > 5])

#-- Short missing series ----

xregdata <- prepshort[,c("timePointYears", "geographicAreaM49", "measuredItemCPC",    
                             "LogGDP", "LogVA", "ValueYield"), with = F] # , "TOI_AFF"

prepshort <- prepshort[order(timePointYears)]
prepshort[!is.na(Value), LogValue := log(Value)]

xregdata <- xregdata[order(timePointYears)]

#xregdata[!is.na(ValueYield) , LogYield := log(ValueYield)]
# xregdata[,c('ValueGDP', 'ValueVA', 'ValueYield')] <- NULL

#-- Start loop ----

fulldata <- data.table()


geo = '76'
selComm = '21111.01b'

for(geo in unique(xregdata$geographicAreaM49)){
  series_geo <- prepshort[geographicAreaM49 == geo]
  
  xreg_geo <- unique(xregdata[geographicAreaM49 == geo])
  
  
  for(selComm in unique(xreg_geo$measuredItemCPC)){
    
    series_comm <- series_geo[measuredItemCPC == selComm]
    series_comm <- series_comm[,colSums(is.na(series_comm))<nrow(series_comm), with = F]
    #series_comm <- series_comm[complete.cases(series_comm)]
    xreg_comm <- xreg_geo[measuredItemCPC == selComm & 
                            timePointYears %in% unique(series_comm$timePointYears)]
    xreg_comm <- xreg_comm[,colSums(is.na(xreg_comm))<nrow(xreg_comm), with = F]
    xreg_commsig <- copy(xreg_comm)
    
    tryCatch({
      #-- Variable selection ----
      
      ppseries <- ts(series_comm$LogValue, start = min(as.numeric(series_comm$timePointYears)))
      
      dfcomb <- list()
      
      # Exclude Year, country and commodity (columns 1:3)
      xreg_comm <- xreg_comm[ , colSums(is.na(xreg_comm)) == 0, with = F]
      xreg_commsig <- copy(xreg_comm)
      
      vec2comb <- 4:ncol(xreg_comm)
      
      
      for(h in 1:length(vec2comb)){
        dfcomb[[h]] <- combn(vec2comb, h)
        
      }
      
      
      modlist <- list()
      ncolj <- 0
      for( j in 1:length(dfcomb)){
        
        combj <- dfcomb[[j]]
        ncolj <- ncolj + ncol(combj)
        for(k in 1:ncol(combj)){
          modvar1 <- unique(c(1,2,3, vec2comb[!vec2comb %in% combj[,k]]))
          xreg2use <- xreg_comm[ , -modvar1, with = F]
          modlist[[((ncolj - ncol(combj)) + k)]] <- auto.arima(ppseries, seasonal = FALSE, xreg = xreg2use)
          print(((ncolj - ncol(combj)) + k))
        }
        
        
      }
      rm(ncolj)
      
      modcomp <- data.table()
      
      for(mods in 1:length(modlist)){
        modcomp <- rbind(modcomp, 
                         data.table(Vars = paste(colnames(modlist[[mods]]$xreg), 
                                                 collapse = ' '),
                                    AIC = modlist[[mods]]$aic))
      }
      
      
      varsel <- unlist(strsplit(modcomp[AIC == min(AIC),]$Vars, ' '))
      varsel <- varsel[varsel != 'drift']
      #xreg_commsig <- xreg_commsig[,colSums(is.na(xreg_commsig))<nrow(xreg_commsig), with = F]
      
      xreg_commsig <- xreg_commsig[,varsel, with = F]
      
      mod1 <- auto.arima(ppseries, seasonal = FALSE, xreg = xreg_commsig)
      mod2 <- auto.arima(ppseries, seasonal = FALSE, xreg = xreg_comm[ ,-(1:3), with = F])
      if(mod1$aic < mod2$aic){
        mod <- mod1 
        newxreg1 <- xreg_commsig
      } else{ 
        mod <- mod2
        newxreg1 <- xreg_comm[ ,-(1:3), with = F] }
      
      # First option
      pred1a <- predict(mod1, newxreg = newxreg1)$pred
      if(ncol(pred1a) > 1){
        pred1a <- as.data.table(pred1a)
        pred1a[, mean := rowMeans(pred1a, na.rm = T)]
        pred1a <- pred1a[,.(mean)]
      }
      
      
      pred1aa <- predict(mod2, newxreg = xreg_comm[ ,-(1:3), with = F])
      # Second option
      pred1b <- na.kalman(x = ppseries, model = 'auto.arima', xreg = newxreg1)
      pred1bb <- na.kalman(x = ppseries, model = 'auto.arima', xreg = 
                            xreg_comm[ ,-c(1:3), with = F])
      #Third option
      
      # Ensemble approach
      impPar <- defaultImputationParameters()
      impPar$imputationValueColumn="Value"
      impPar$imputationFlagColumn="flagObservationStatus"
      impPar$imputationMethodColumn="flagMethod"
      impPar$byKey=c("geographicAreaM49", "measuredItemCPC")
      impPar$estimateNoData=FALSE
      
      # If the data series contains only zero and missing value then it is considered to contain no information for imputation.
      
      pp_ensemble <- removeNoInfo(series_comm,
                                  value="Value",
                                  observationFlag = "flagObservationStatus",
                                  byKey = c(impPar$byKey))
      names(pp_ensemble)
      pp_ensemble_sub <- pp_ensemble[,c("geographicAreaM49",
                                        "timePointYears",
                                        "measuredItemCPC",
                                        "Value",
                                        "flagObservationStatus",
                                        "flagMethod"), with = F]
      
      # If no missing data the commodityDB does not change
      pp_ensemble_imp <- imputeVariable(data = pp_ensemble_sub,
                                        imputationParameters = impPar)
      
      pred1c <- pp_ensemble_imp$Value
      #############
      
   
      newimputation <- series_comm[, names(prep_price), with = F]
      newimputation[, Predicted := exp(pred1a)] 
      newimputation[, Kalman := exp(pred1b)]
      newimputation[, Ensemble := pred1c]
      # newimputation[ is.na(Value), 
      #                c('Value', 'flagObservationStatus', 'flagMethod') := 
      #                  list(Imputed, 'P', 'e')]
      
      fulldata <- rbind(fulldata, newimputation)
         
      ggplot()+
       geom_line(data = fulldata, aes(timePointYears, Predicted, group = 1, col = 'Predicted'))+
        geom_point(data = fulldata, aes(timePointYears, Predicted, col = 'Predicted'))+
        geom_line(data = fulldata, aes(timePointYears, Kalman, group = 1, col = 'Kalman'))+
        geom_point(data = fulldata, aes(timePointYears, Kalman, col = 'Kalman'))+
        geom_line(data = fulldata, aes(timePointYears, Ensemble, group = 1, col = 'Ensemble'))+
        geom_point(data = fulldata, aes(timePointYears, Ensemble, col = 'Ensemble'))+
        geom_line(data = fulldata, aes(timePointYears, Value, group = 1, col = 'Original series'))+
        geom_point(data = fulldata, aes(timePointYears, Value, col = 'Original series'))+
        ggtitle('Myanmar, Soya beans series')
        
      
    
    }, error=function(e){})
    
    print(geo)
    
  }
  
}

SaveData(domainPP, 'annual_producer_prices_validation')
# Stima fatta ora è da salvare e da revisionare in shiny 
# come salvare più valori in SWS? Più anni? mettere in metadati modello
# ne andrebbe scelto uno di valore... da chiedere
