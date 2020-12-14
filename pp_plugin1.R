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
datasetVal <- 'annual_producer_prices_validated'
datasetPrep <- 'annual_producer_prices_prep' #'annual_producer_prices_quest'
datasetQuest <- 'annual_producer_prices_quest'
LCUcode <- '5530'

countryPar <- '250, 36, 170, 840, 116' #swsContext.computationParams$countries
print(countryPar)
if(!is.null(countryPar) & length(countryPar) > 0){
  countryPar <- swsContext.computationParams$countries
  sessionCountry <- strsplit(countryPar, ', ')[[1]]
} else {
  sessionCountry <- swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys
  countries <- GetCodeList(domainPP, datasetPrep, "geographicAreaM49")[ type == 'country']$code
  # Make sure only countries not areas
  sessionCountry <- countries #sessionCountry[sessionCountry %in% countries]
}
message(paste("Prod Prices: countries selected ", paste0(sessionCountry, collapse = ', '), '.', sep = ''))

# Mandatory year values.
maxyear <- 2019#as.numeric(swsContext.computationParams$maxyear)
minyear <- 2017#as.numeric(swsContext.computationParams$minyear)
selectedYears <- as.character(minyear:maxyear)

#-- Pull questionnaire data ----

priceKey = DatasetKey(
  domain = domainPP,
  dataset = datasetQuest,
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = sessionCountry),
    Dimension(name = "measuredElement", 
              keys = GetCodeList(domainPP, datasetQuest, 'measuredElement')[ code == LCUcode, code]),
    Dimension(name = "measuredItemCPC",
              keys = GetCodeList(domainPP, datasetQuest, 'measuredItemCPC')[, code]),
    Dimension(name = "timePointYears", 
              keys = GetCodeList(domainPP, datasetQuest, 'timePointYears')[code %in% selectedYears, code]))
  
)


priceData <- GetData(priceKey, flags = TRUE)


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
              keys = GetCodeList('common', 'exchange_rates_annual', 'geographicAreaM49')[code %in% sessionCountry, code]),
    Dimension(name = "from_currency",
              keys = GetCodeList('common', 'exchange_rates_annual', 'from_currency')[code != 'ECU' & endDate >= '1991-01-01' | 
                                                                                      is.na(endDate), code]),
    Dimension(name = "to_currency", 
              keys = GetCodeList('common', 'exchange_rates_annual', 'to_currency')[code == 'USD', code]),
    Dimension(name = 'measuredElement',
              keys = 'LCU'),
    Dimension(name = "timePointYears", 
              keys = GetCodeList('common', 'exchange_rates_annual', 'timePointYears')[code %in% selectedYears, code]))
  
)

erdt <- GetData(erKey, flags = F) 

erdt[,c('measuredElement', 'to_currency')] <- NULL

# check on currency
if(!all(erdt$from_currency %in% lcu_2_m49$code_iso)){
  stop(paste('Missing countey-currency correspondence: ', 
             unique(erdt[!from_currency %in% lcu_2_m49$code_iso]$from_currency),
             'not in the lcu_2_m49 datatble. Please update it.'))
}

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
              keys = GetCodeList(domainPP, datasetVal, 'timePointYears')[, code]))
  
)

val_price <- GetData(valpriceKey, flags = TRUE)
save.image('saved1.RData')

setnames(val_price, 'flag_obs_status_v2', 'flagObservationStatus')
# If change of currency (the datatable has to be updated)
# pper[geographicAreaM49 == '364' & timePointYears == 2019, from_currency := 'IRA']
# pper[geographicAreaM49 == '480' & timePointYears >= 2018, from_currency := 'MAU']


if(any(pper$flagObservationStatus == 'B')){
  
  geotimecomb <- unique(pper[flagObservationStatus == 'B', .(geographicAreaM49, timePointYears, from_currency)])
  
  # Get datatable with conversion rates 
  conv_rates <- ReadDatatable('currency_changes')
  # conv_rates <- rbind(conv_rates, data.table(
  #   old_currency_code = c('IRR', 'MUR'), 
  #   new_currency_code = c('IRA', 'MAU'), 
  #   exchange_rate = c(1,1)))
  
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

pptot[ , names(pptot)[grepl('_old', names(pptot))]] <- NULL  

difference_tolerance <- 0.1
revision2control <- pptot[abs(diff) > difference_tolerance & !is.na(diff)]
revision2control[, substitute := FALSE]
names(revision2control) <- tolower(names(revision2control))

if(nrow(revision2control) > 0){
  r2c <- ReadDatatable('revision2control', readOnly = F)
  changeset <- Changeset('revision2control')
  AddDeletions(changeset, r2c[geographicaream49 %in% unique(pptot$geographicAreaM49)])
  Finalise(changeset)
  changeset <- Changeset('revision2control')
  AddInsertions(changeset, revision2control)
  Finalize(changeset)
}

pptot[,diff:= NULL]
#-- Outlier detection ----

ppout <- copy(pptot)
ppout[ , LogValue := log(Value)]
ppout[measuredElement == '5531' , 
      var := diff(c(NA, LogValue))/shift(LogValue), 
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

#-- tsclean ----
ppnew <- copy(pptot)
ppnew <- ppnew[order(timePointYears)]
ppnew[,LogValue := log(Value)]
ppnew[!is.na(LogValue) & measuredElement == '5531', 
      c('LogValue_clean',
        'flagObservationStatus_clean',
        'flagMethod_clean'):= list(tsclean(ts(LogValue), replace.missing = FALSE),
                                   flagObservationStatus, flagMethod),
      by = c("geographicAreaM49",
             "measuredElement", 'measuredItemCPC')] # use tsclean for missing values and outliers. 

ppnew$LogValue_clean <- as.vector(ppnew$LogValue_clean)
ppnew$LogValue <- as.vector(ppnew$LogValue)
ppnew[ , Value_clean := exp(LogValue_clean)]

ppnew[, diff := (LogValue_clean - LogValue)/LogValue]
ppnew[timePointYears >= min(slcquest$timePointYears) & LogValue != LogValue_clean, 
      c('flagObservationStatus_clean',
        'flagMethod_clean'):= list('E', 'e')]


outliers <- rbind(ppnew[flagObservationStatus_clean == 'E' & flagMethod == 'e'], 
                  ppout[timePointYears %in% selectedYears & outlier2 == TRUE], fill = T)
outliers <- unique(outliers[ , names(pptot), with = F])

# NOW (out;e) as there are other (E;e)
pptot[ outliers, c('flagObservationStatus', 'flagMethod') := list('out','e'),
       on = c('geographicAreaM49', 'timePointYears', 'measuredItemCPC')]

# comparison with mean monthly data!!!!!!!!!

# SaveData(domainPP, datasetPrep, pptot)

# then the shiny gives outliers (E,e) to revise manually 
# data are then saved into the preparation dataset along with 
# elements 5530 and 5532 (LCU and USD)

##-- send Email with notification of correct execution ----

from = "sws@fao.org"
to = swsContext.userEmail
subject = "The plug-in has correctly run"
body = paste('Number fo revised values: ', nrow(revision2control), '. Number of outlier detected: ', nrow(outliers), sep = '')
sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)
paste0("Email sent to ", swsContext.userEmail)

