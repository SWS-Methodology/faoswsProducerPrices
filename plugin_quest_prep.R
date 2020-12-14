suppressMessages({
  library(data.table)
  library(DT)
  library(faosws)
  library(faoswsFlag)
  library(faoswsProcessing)
  library(faoswsUtil)
  library(faoswsImputation)
  library(ggplot2)
  library(rhandsontable)
  library(shiny)
  library(shinyWidgets)
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


lcu_2_m49 <- ReadDatatable('lcu_2_m49')
lcu_2_m49[start_year_iso == '', start_year_iso := '1900']
lcu_2_m49[end_year_iso == '', end_year_iso := '9999']


if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws1.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])
}  

erKey = DatasetKey(
    domain = 'Fisheries',
    dataset = 'exchange_rates_fi',
    dimensions = list(
      Dimension(name = "from_currency",
                keys = GetCodeList('Fisheries', 'exchange_rates_fi', 'from_currency')[code != 'ECU' & endDate >= '1991-01-01' | 
                                                                                        is.na(endDate), code]),
      Dimension(name = "to_currency", 
                keys = GetCodeList('Fisheries', 'exchange_rates_fi', 'to_currency')[, code]),
      Dimension(name = "split_year",
                keys = '0'),
      Dimension(name = "timePointYears", 
                keys = GetCodeList('Fisheries', 'exchange_rates_fi', 'timePointYears')[, code]))
    
  )
  
erdt <- GetData(erKey, flags = F) 


if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = 'c9cd8ef0-5dee-49ed-8b40-93c96c917fdf')
}


#erdt <- fread('ExchangeRates/Data/data.csv', colClasses = c(rep('character', 4), 'numeric'))
# erdt <- erdt[timePointYears >= '1991']

all(erdt$from_currency %in% lcu_2_m49$code_iso)
unique(erdt[!from_currency %in% lcu_2_m49$code_iso]$from_currency)

swsiso <- GetCodeList('Fisheries', 'exchange_rates_fi', 'from_currency')
swsiso2 <- swsiso[as.Date(endDate) > "1991-01-1" | is.na(as.Date(endDate))]


domainPP <- 'prod_prices'
datasetVal <- 'annual_producer_prices_validated'
datasetQuest <- 'annual_producer_prices_validated' #'annual_producer_prices_quest'
LCUcode <- '5530'

priceKey = DatasetKey(
  domain = domainPP,
  dataset = datasetQuest,
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = GetCodeList(domainPP, datasetQuest, 'geographicAreaM49')[code %in% c('250', '380'), code]),
    Dimension(name = "measuredElement", 
              keys = GetCodeList(domainPP, datasetQuest, 'measuredElement')[, code]),
    Dimension(name = "measuredItemCPC",
              keys = GetCodeList(domainPP, datasetQuest, 'measuredItemCPC')[, code]),
    Dimension(name = "timePointYears", 
              keys = GetCodeList(domainPP, datasetQuest, 'timePointYears')[code %in% as.character(1997:2002), code]))
  
)


priceData <- GetData(priceKey, flags = TRUE)

ppwithcurr <- merge(priceData, lcu_2_m49[code_m49 %in% c('250', '380')], #& 
                    #                                            end_year_iso >=  min(priceData$timePointYears)
                    #                                           end_year_iso == ''], 
                    by.x = 'geographicAreaM49',
                    by.y = 'code_m49', all.x = T, allow.cartesian = T)


ppwithcurr <- ppwithcurr[start_year_iso <= timePointYears & timePointYears <= end_year_iso]

ppwithcurr[, c("start_year_iso",       
               "end_year_iso", "name_en_iso", "start_year_m49",     
               "end_year_m49", "name_en_m49")] <- NULL
pper <- merge(ppwithcurr, erdt, by.x = c('code_iso', 'timePointYears'),
              by.y = c('from_currency', 'timePointYears'), all.x = T,
              suffixes = c('_price', '_er'))

pper[, ValueUSD := Value_price * Value_er]
pper[, c("to_currency",         
         "split_year", "Value_er")] <- NULL

pper[, ValueSLC := Value_price]

#---
crKey = DatasetKey(
  domain = 'producerprices',
  dataset = 'exchange_rates_test',
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = GetCodeList(domainPP, datasetQuest, 'geographicAreaM49')[, code]),
    Dimension(name = "from_currency",
              keys = GetCodeList('producerprices', 'exchange_rates_test', 'from_currency')[code != 'ECU' & endDate >= '1991-01-01' | 
                                                                                      is.na(endDate), code]),
    Dimension(name = "to_currency", 
              keys = GetCodeList('producerprices', 'exchange_rates_test', 'to_currency')[, code]),
    Dimension(name = "timePointYears", 
              keys = GetCodeList('producerprices', 'exchange_rates_test', 'timePointYears')[, code]))
  
)

crdt <- GetData(crKey, flags = T) 
crdt <- crdt[flagObservationStatus == 'B']

#----

ppslc <- merge(pper, crdt, by.x = c('geographicAreaM49', 'timePointYears', 'code_iso'),
      by.y = c('geographicAreaM49', 'timePointYears', 'to_currency'), all.x = T)

pper[geographicAreaM49 == '703' & timePointYears == '2018', flagObservationStatus := 'B']
pper[geographicAreaM49 == '703' & timePointYears < '2018', code_iso := 'OLD']

if(any(pper$flagObservationStatus == 'B')){
  
  breakcoord <- unique(pper[flagObservationStatus == 'B', 
                            .(geographicAreaM49, timePointYears, code_iso)])
  
  # conversionrates <- ReadDatatable('') to be done
  conversionrates <- data.table(geographicaream49 = c('703', '364'),
                                timepointyears = c('2018', '1964'), 
                                from_code_iso = c('OLD', 'PER'),
                                to_code_iso = c('EUR', 'IRR'),
                                convrate = c(1.89, 0.26))
  
  conversionrates <- conversionrates[geographicaream49 %in% breakcoord$geographicAreaM49 & 
                                       timepointyears %in% breakcoord$timePointYears & 
                                       to_code_iso %in% breakcoord$code_iso]
  
  ppslc <- merge(pper, conversionrates,
                 by.x = c('code_iso', 'geographicAreaM49'), by.y = c('from_code_iso', 'geographicaream49'), all.x = T)
  
  ppslc[!is.na(convrate), ValueSLC := ValueSLC*convrate]
  
} else {
  ppscl <- pper
}

pp <- melt(ppslc,
           measure.vars = c('Value_price', 'ValueUSD', 'ValueSLC'))

pp[variable == 'ValueUSD' , c('measuredElement',
                              'flagObservationStatus',
                              'flagMethod'):= list('5532', '', 'i')]

pp[variable == 'ValueSLC' , c('measuredElement',
                              'flagObservationStatus',
                              'flagMethod'):= list('5531', '', 'i')]

pp[ , c('variable', 'code_iso', "timepointyears",
        "to_code_iso", "convrate") := NULL]
setnames(pp, 'value', 'Value')

# SaveData(domainPP, 'annual_producer_prices_prep')

# now we should have the whole series and compare with previous ones.

pricevalKey = DatasetKey(
  domain = domainPP,
  dataset = datasetVal,
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = GetCodeList(domainPP, datasetQuest, 'geographicAreaM49')[, code]),
    Dimension(name = "measuredElement", 
              keys = GetCodeList(domainPP, datasetQuest, 'measuredElement')[, code]),
    Dimension(name = "measuredItemCPC",
              keys = GetCodeList(domainPP, datasetQuest, 'measuredItemCPC')[, code]),
    Dimension(name = "timePointYears", 
              keys = GetCodeList(domainPP, datasetQuest, 'timePointYears')[, code]))
  
)

ppval <- GetData(pricevalKey)


ppval <- ppval[order(timePointYears)]
setnames(ppval, 'flag_obs_status_v2', 'flagObservationStatus')

names(ppval)
ppcomp <- merge(ppval[geographicAreaM49 %in% unique(pp$geographicAreaM49)], pp, by = c("geographicAreaM49",
                        "measuredElement",
                        "measuredItemCPC",      
                        "timePointYears"), suffixes = c('_val', '_quest'),
                all = T)

diff <- ppcomp[measuredElement == '5530' & Value_val != Value_quest]
diff[ , var :=(Value_quest/Value_val)-1, 
      by = c("geographicAreaM49",
             "measuredElement",
             "measuredItemCPC")]
diff[, quart1 := quantile(var, 0.25), 
     by = c("geographicAreaM49",
            "measuredElement",
            "measuredItemCPC")]
diff[, quart3 := quantile(var, 0.75), 
     by = c("geographicAreaM49",
            "measuredElement",
            "measuredItemCPC")]

diff[, lower := quart1 - (1.5*(quart3-quart1))]
diff[, upper := quart3 + (1.5*(quart3-quart1))]
diff[var > upper | var < lower ,outlier := 1]
diff[var <= upper & var >= lower ,outlier := 0]


highrevisions <- diff[outlier == 1]

if(nrow(highrevisions) > 0){
  message('Questionnaire revisions to check!')
} else {
  
  ppnew <- ppcomp[!is.na(Value_quest), c('Value', 'flagObservationStatus', 
                                         'flagMethod') := list(Value_quest,
                                                             flagObservationStatus_quest,
                                                             flagMethod_quest)]
  ppnew[is.na(Value), c('Value', 'flagObservationStatus', 
                        'flagMethod') := list(Value_val, flagObservationStatus_val,
                                            flagMethod_val)]
  ppnew[,c("Value_val",
           "flagObservationStatus_val",  
           "flagMethod_val",
           "Value_quest",
           "flagObservationStatus_quest",
           "flagMethod_quest")] <- NULL
  }


ppnew[ , var := c(NA,exp(diff(log(Value)))-1), 
       by = c("geographicAreaM49",
              "measuredElement",
              "measuredItemCPC")]
ppnew[, quart1 := quantile(var, 0.25, na.rm = T), 
      by = c("geographicAreaM49",
             "measuredElement",
             "measuredItemCPC")]
ppnew[, quart3 := quantile(var, 0.75, na.rm = T), 
     by = c("geographicAreaM49",
            "measuredElement",
            "measuredItemCPC")]

ppnew[, lower := quart1 - (1.5*(quart3-quart1))]
ppnew[, upper := quart3 + (1.5*(quart3-quart1))]
ppnew[var > upper | var < lower , outlier := 1]
ppnew[var <= upper & var >= lower | timePointYears < min(pp$timePointYears),outlier := 0]



library(forecast)

ppnew[!is.na(Value) & timePointYears >= '2010' , Value_clean := tsclean(ts(Value), replace.missing = T),
       by = c("geographicAreaM49",
              "measuredElement", 'measuredItemCPC')] # use tsclean for missing values and outliers. 

#ppnew<- as.data.table(ppnew)
ppnew$Value <- as.vector(ppnew$Value)
ppnew$Value_clean <- as.vector(ppnew$Value_clean)
ppnew[timePointYears>= min(pp$timePointYears) & Value != Value_clean, flagObservationStatus_clean := 'E']
ppnew[timePointYears>= min(pp$timePointYears) & Value != Value_clean, flagMethod_clean := 'e']


View(ppnew[flagObservationStatus_clean == 'E' & outlier == 1 & measuredElement == '5530'])
View(ppnew[flagObservationStatus_clean == 'E' & measuredElement == '5530'])
View(ppnew[outlier == 1 & measuredElement == '5530'])

# comparison with mean monthly data!!!!!!!!!


