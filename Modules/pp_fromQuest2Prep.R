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
                     token = SETTINGS[["token"]])#'4e9d9a2e-5258-48b6-974f-3656d1af8217')
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
  #sessionCountry <- swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys
  countries <- GetCodeList(domainPP, datasetPrep, "geographicAreaM49")[ type == 'country']$code
  # Make sure only countries not areas
  sessionCountry <- countries#sessionCountry[sessionCountry %in% countries]
}
message(paste("Prod Prices: countries selected ", paste0(sessionCountry, collapse = ', '), '.', sep = ''))

# Mandatory year values.
maxyear <- as.numeric(swsContext.computationParams$maxyear)
minyear <- as.numeric(swsContext.computationParams$minyear)
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

if(priceData[flagObservationStatus == 'B',.N] > 0){
  changes <- unique(priceData[flagObservationStatus == 'B',.(geographicAreaM49, timePointYears)])
  msg <- paste('Currency change in country ', paste0(unique(changes$geographicAreaM49), collapse = ', '), 
               ' respectively in years ', paste0(unique(changes$timePointYears), collapse = ', '),
               'Please make sure the SLC series are consistent in the preparation dataset running the pp_Conversion plugin for these countries.', 
               sep = '') 
} else { msg <- ''}

setkey(priceData)
priceMetaData <- GetMetadata(priceKey)
currencyPerYear <- priceMetaData[Metadata_Element == 'COMMENT']
currencyPerYear <- currencyPerYear[,.N, .(geographicAreaM49, timePointYears, Metadata_Value)]
currencyPerYear[, currency_new := gsub('Currency: ', '', Metadata_Value)]
currencyPerYear[, currency_new := gsub(' ', '', currency_new)]

dt2update <- ReadDatatable('country_year_lcu', readOnly = FALSE)

updatedTable <- merge(dt2update, currencyPerYear[,.(geographicAreaM49,
                                                    timePointYears,
                                                    currency_new)], 
                      by.x = c('geographicaream49', 'timepointyears'),
                      by.y = c('geographicAreaM49', 'timePointYears'), all = T)

updatedTable[!is.na(currency_new) & is.na(currency_code), currency_code := currency_new]
updatedTable[currency_new != currency_code, currency_code := currency_new]
updatedTable[,currency_new := NULL]

cng <- Changeset('country_year_lcu')
AddModifications(cng, updatedTable)
Finalize(cng)

#-- USD conversion ----

# Pull exchange rates dataset

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
                keys = 'LCU'),
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
                keys = 'LCU'),
      Dimension(name = "timePointYears", 
                keys = GetCodeList('common', 'exchange_rates_annual', 'timePointYears')[code %in% selectedYears, code]))
    
  )
  
  erdt <- GetData(erKey, flags = F) 
  
}

erdt[,c('measuredElement', 'to_currency')] <- NULL


priceconverted <- convert_currency(priceData = priceData, erdt = erdt, sessionElement = 'LCU')

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

#-- Data Saving ----
  
  data2save <- priceconverted[!is.na(Value), .(geographicAreaM49,
                                         timePointYears,
                                         measuredElement,
                                         measuredItemCPC,
                                         Value,
                                         flagObservationStatus,
                                         flagMethod)]
  
  SaveData(domainPP, datasetPrep, data2save)
  

#-- Merge QUEST and VAL ----

pptot <- merge(priceconverted[measuredElement == '5530'], val_price[measuredElement == '5530'], by = c('geographicAreaM49', 
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

difference_tolerance <- 0.1
revisions2control <- pptot[abs(diff) > difference_tolerance & !is.na(diff)]
revisions2control[, refuse_update := FALSE]

names(revisions2control) <- tolower(names(revisions2control))

m49 <- GetCodeList(domainPP, datasetVal, 'geographicAreaM49')[,.(code, description)]
cpc <- GetCodeList(domainPP, datasetVal, 'measuredItemCPC')[,.(code, description)]

revisions2control <- merge(revisions2control, m49, by.x = 'geographicaream49', by.y = 'code', all.x = T)
setnames(revisions2control, 'description', 'country')
revisions2control <- merge(revisions2control, cpc, by.x = 'measureditemcpc', by.y = 'code', all.x = T)
setnames(revisions2control, 'description', 'product')

if(nrow(revisions2control) > 0){
  r2c <- ReadDatatable('revisions2control', readOnly = F)
  changeset <- Changeset('revisions2control')
  AddDeletions(changeset, r2c[geographicaream49 %in% unique(pptot$geographicAreaM49)])
  Finalise(changeset)
  changeset <- Changeset('revisions2control')
  AddInsertions(changeset, revisions2control)
  Finalize(changeset)
}




#-- send Email with notification of correct execution ----

mailbody <- paste(msg,
                   paste('Number fo revised values: ', nrow(revisions2control), sep = ''),
                   paste('Number fo revised values: ', nrow(revisions2control), sep = ''))

from = "sws@fao.org"
to = swsContext.userEmail
subject = "The plug-in has correctly run"
body = mailbody
sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)
paste0("Email sent to ", swsContext.userEmail)