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
  library(sendmailR)
})

#-- Token QA ----

if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = '4e9d9a2e-5258-48b6-974f-3656d1af8217')
}

#-- Parameters ----

domainPP <- 'prod_prices'
datasetVal <- 'annual_producer_prices_validation'
datasetPrep <- 'annual_producer_prices_prep'
datasetQuest <- 'annual_producer_prices_quest'
LCUcode <- '5530'

countryPar <-  swsContext.computationParams$countries
print(countryPar)
if(!is.null(countryPar) & length(countryPar) > 0){
  countryPar <- swsContext.computationParams$countries
  sessionCountry <- strsplit(countryPar, ', ')[[1]]
} else {
  sessionCountry <- swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys
  countries <- GetCodeList(domainPP, datasetPrep, "geographicAreaM49")[ type == 'country']$code
  # Make sure only countries not areas
  sessionCountry <- sessionCountry[sessionCountry %in% countries]
}
message(paste("Prod Prices: countries selected ", paste0(sessionCountry, collapse = ', '), '.', sep = ''))

# Mandatory year values.
maxyear <- as.numeric(swsContext.computationParams$maxyear)
minyear <-  as.numeric(swsContext.computationParams$minyear)
selectedYears <- as.character(minyear:maxyear)

#-- Pull questionnaire data ----

priceKey = DatasetKey(
  domain = domainPP,
  dataset = datasetQuest,
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = GetCodeList(domainPP, datasetQuest, 'geographicAreaM49')[code %in% sessionCountry, code]),
    Dimension(name = "measuredElement", 
              keys = GetCodeList(domainPP, datasetQuest, 'measuredElement')[ code == LCUcode, code]),
    Dimension(name = "measuredItemCPC",
              keys = GetCodeList(domainPP, datasetQuest, 'measuredItemCPC')[, code]),
    Dimension(name = "timePointYears", 
              keys = GetCodeList(domainPP, datasetQuest, 'timePointYears')[code %in% selectedYears, code]))
  
)


priceData <- GetData(priceKey, flags = TRUE)


#-- USD conversion ----

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
              keys = GetCodeList('common', 'exchange_rates_annual', 'timePointYears')[, code]))
  
)

erdt <- GetData(erKey, flags = F) 

erdt[,c('measuredElement', 'to_currency')] <- NULL

lcu_2_m49 <- ReadDatatable('lcu_2_m49')
eco_curr0 <- ReadDatatable('currency_country_years')
xrcountry <-  ReadDatatable('currency_changes')

erdt <- fix_xr(erdt, lcu_2_m49, eco_curr0, xrcountry)

# Start conversion into USD and SLC merging with XR
pper0 <- merge(priceData, erdt, by = c('geographicAreaM49', 'timePointYears'), all.x = T,
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
  dataset = datasetVal,
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = GetCodeList(domainPP, datasetVal, 'geographicAreaM49')[code %in% unique(priceData$geographicAreaM49), code]),
    Dimension(name = "measuredElement", 
              keys = GetCodeList(domainPP, datasetVal, 'measuredElement')[code %in% c('5530', '5531', '5532'), code]),
    Dimension(name = "measuredItemCPC",
              keys = GetCodeList(domainPP, datasetVal, 'measuredItemCPC')[, code]),
    Dimension(name = "timePointYears", 
              keys = GetCodeList(domainPP, datasetVal, 'timePointYears')[code %in% as.character((maxyear-10):maxyear), code]))
  
)

val_price <- GetData(valpriceKey, flags = TRUE)
#setnames(val_price, 'flag_obs_status_v2', 'flagObservationStatus')

if(any(pper[timePointYears == maxyear]$flagObservationStatus == 'B')){
  
  geotimecomb <- unique(pper[flagObservationStatus == 'B', .(geographicAreaM49, timePointYears, from_currency)])
  
  # Get datatable with conversion rates 
  # If change of currency (the datatable has to be updated)
  conv_rates <- ReadDatatable('currency_changes')
  
  conv_rates_needed <- merge(conv_rates, geotimecomb, by.x  = 'new_currency_code',
                             by.y = 'from_currency')
  
  slcval <- merge(val_price, conv_rates_needed, by = 'geographicAreaM49', 
                  all.x = T, suffixes = c('', '_change'))
  
  slcval[measuredElement == '5531' & timePointYears < timePointYears_change, c('Value',
                                                                               'flagObservationStatus', 
                                                                               'flagMethod'):= list(Value/exchange_rate,
                                                                                                    flagObservationStatus,
                                                                                                    'i')]
  names(slcval)
  slcval[ , c("new_currency_code",    
              "old_currency_code",
              "exchange_rate",
              "timePointYears_change")] <- NULL
  
  slcquest <- merge(pper, conv_rates_needed,  by = 'geographicAreaM49',
                    all.x = T, suffixes = c('', '_change'))
  
  slcquest[measuredElement == '5531' & timePointYears < timePointYears_change, c('Value',
                                                                                 'flagObservationStatus', 
                                                                                 'flagMethod'):= list(Value/exchange_rate,
                                                                                                      flagObservationStatus,
                                                                                                      'i')]
  slcquest[ , c("new_currency_code",    
                "old_currency_code",
                "exchange_rate",
                "timePointYears_change")] <- NULL
  
  
  
} else {
  slcval <- val_price
  slcquest <- pper
}

#-- Merge QUEST and VAL ----

pptot <- merge(slcquest, slcval, by = c('geographicAreaM49', 
                                        'timePointYears',
                                        'measuredElement',
                                        'measuredItemCPC'),
               suffixes = c('', '_old'), all = T)

pptot[!is.na(Value_old) & is.na(Value), c('Value',
                                          'flagObservationStatus', 
                                          'flagMethod') := list(Value_old, 
                                                                flagObservationStatus_old,
                                                                flagMethod_old)]

pptot[!is.na(Value_old) & !is.na(Value), diff := (Value_old - Value)/Value]
pptot[ , from_currency := NULL]  

difference_tolerance <- 0.1
revisions2control <- pptot[abs(diff) > difference_tolerance & !is.na(diff)]
revisions2control[, refuse_update := FALSE]

names(revisions2control) <- tolower(names(revisions2control))

if(nrow(revisions2control) > 0){
  r2c <- ReadDatatable('revisions2control', readOnly = F)
  changeset <- Changeset('revisions2control')
  AddDeletions(changeset, r2c[geographicaream49 %in% unique(pptot$geographicAreaM49)])
  Finalise(changeset)
  changeset <- Changeset('revisions2control')
  AddInsertions(changeset, revisions2control)
  Finalize(changeset)
}

pptot[, c(names(pptot)[grepl('old', names(pptot))], 'diff')] <- NULL


#-- Outlier detection ----

ppout <- copy(pptot)
ppout[ , LogValue := log(Value)]
ppout[measuredElement == '5531' , 
      var := diff(c(NA, LogValue)), 
      by = c("geographicAreaM49",
             "measuredElement",
             "measuredItemCPC")]
#-- Manual outlier ----
ppout[measuredElement == '5531', 
      quart1 := quantile(var, 0.25, na.rm = T),
      by = c("geographicAreaM49",
             "measuredElement",
             "measuredItemCPC")]
ppout[measuredElement == '5531', 
      quart3 := quantile(var, 0.75, na.rm = T),
      by = c("geographicAreaM49",
             "measuredElement",
             "measuredItemCPC")]

ppout[measuredElement == '5531', lower := quart1 - (1.5*(quart3-quart1))]
ppout[measuredElement == '5531', upper := quart3 + (1.5*(quart3-quart1))]

ppout[, outlier2 := FALSE]
ppout[var > upper | var < lower  , outlier2 := TRUE,
      by = c("geographicAreaM49",
             "measuredElement",
             "measuredItemCPC")]
ppout[!timePointYears %in% selectedYears, outlier2 := FALSE]
ppout[outlier2 == TRUE, method := 'IQR']

#-- tsclean ----
ppnew <- copy(pptot)
ppnew <- ppnew[order(timePointYears)]
ppnew[,LogValue := log(Value)]
ppnew[!is.na(LogValue) & measuredElement == '5531', 
      c('LogValue_clean',
        'flagObservationStatus_clean',
        'flagMethod_clean'):= list(tsclean(ts(LogValue), replace.missing = FALSE),
                                   '', ''),
      by = c("geographicAreaM49",
             "measuredElement", 'measuredItemCPC')] # use tsclean for missing values and outliers. 

ppnew$LogValue_clean <- as.vector(ppnew$LogValue_clean)
ppnew$LogValue <- as.vector(ppnew$LogValue)
ppnew[ , Value_clean := exp(LogValue_clean)]

ppnew[, diff := (LogValue - LogValue_clean)]

ppnew[ , tresh1 := 1.25*Value_clean]
ppnew[ , tresh2 := 0.75*Value_clean]


ppnew[timePointYears >= min(slcquest$timePointYears) & Value > 1.25*Value_clean, 
      c('flagObservationStatus_clean',
        'flagMethod_clean'):= list('E', 'e')]

ppnew[timePointYears >= min(slcquest$timePointYears) & Value < 0.75*Value_clean, 
     c('flagObservationStatus_clean',
       'flagMethod_clean'):= list('E', 'e')]


ppnew[, method := 'tsclean']

#-- Manual outlier ----

ppout2 <- copy(pptot)

ppout2[measuredElement == '5531', 
      quart1 := quantile(Value, 0.25, na.rm = T),
      by = c("geographicAreaM49",
             "measuredElement",
             "measuredItemCPC")]
ppout2[measuredElement == '5531', 
      quart3 := quantile(Value, 0.75, na.rm = T),
      by = c("geographicAreaM49",
             "measuredElement",
             "measuredItemCPC")]

ppout2[measuredElement == '5531', lower := quart1 - (1.5*(quart3-quart1))]
ppout2[measuredElement == '5531', upper := quart3 + (1.5*(quart3-quart1))]

ppout2[, outlier2 := FALSE]
ppout2[Value > upper | Value < lower  , outlier2 := TRUE,
      by = c("geographicAreaM49",
             "measuredElement",
             "measuredItemCPC")]
ppout2[!timePointYears %in% selectedYears, outlier2 := FALSE]
ppout2[outlier2 == TRUE, method := 'IQR_level']


outliers <- rbind(ppnew[flagObservationStatus_clean == 'E' & flagMethod_clean == 'e'], 
                  ppout[timePointYears %in% selectedYears & outlier2 == TRUE],
                  ppout2[timePointYears %in% selectedYears & outlier2 == TRUE],
                  fill = T)

outliers2over3 <- outliers[,.N, c("geographicAreaM49",
                "measuredElement",
                "measuredItemCPC",
                'timePointYears')]
outliers2over3 <- outliers2over3[N >= 2]

setkey(outliers)
outliers <- outliers[outliers2over3, on = c("geographicAreaM49",
                                            "measuredElement",
                                            "measuredItemCPC",
                                            'timePointYears')]
setkey(outliers)
outliers <- unique(outliers[ ,c("geographicAreaM49",
                         "measuredElement",
                         "measuredItemCPC",
                         'timePointYears'), with = F])

# NOW (out;e) as there are other (E;e)
pptot[ outliers, c('flagObservationStatus', 'flagMethod') := list('E','e'),
       on = c('geographicAreaM49', 'timePointYears', 'measuredItemCPC')]



#-- Data Saving ----
SaveData(domainPP, datasetPrep, pptot)

# then the shiny gives outliers (E,e) to revise manually 
# data are then saved into the preparation dataset along with 
# elements 5530 and 5532 (LCU and USD)

#-- send Email with notification of correct execution ----

from = "sws@fao.org"
to = swsContext.userEmail
subject = "The plug-in has correctly run"
body = paste('Number fo revised values: ', nrow(revisions2control), '. Number of outlier detected: ', nrow(outliers), sep = '')
sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)
paste0("Email sent to ", swsContext.userEmail)
