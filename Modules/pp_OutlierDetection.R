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
CurrElCode <- c('5530', '5531', '5532')

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

#-- Pull preparation data ----

lastyear <- as.character(as.numeric(format(Sys.Date(), '%Y')))

priceKey = DatasetKey(
  domain = domainPP,
  dataset = datasetPrep,
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = GetCodeList(domainPP, datasetPrep, 'geographicAreaM49')[code %in% sessionCountry, code]),
    Dimension(name = "measuredElement", 
              keys = GetCodeList(domainPP, datasetPrep, 'measuredElement')[ code %in% CurrElCode, code]),
    Dimension(name = "measuredItemCPC",
              keys = GetCodeList(domainPP, datasetPrep, 'measuredItemCPC')[, code]),
    Dimension(name = "timePointYears", 
              keys = GetCodeList(domainPP, datasetPrep, 'timePointYears')[code > as.character(as.numeric(lastyear)-22), code]))
  
)


priceData <- GetData(priceKey, flags = TRUE)

newYear <- max(priceData$timePointYears)

#-- Outlier detection ----

ppout <- copy(priceData)
ppout <- ppout[order(timePointYears)]
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
ppout[!timePointYears %in% newYear, outlier2 := FALSE]
ppout[outlier2 == TRUE, method := 'IQR']

#-- tsclean ----
ppnew <- copy(priceData)
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


ppnew[timePointYears == newYear & Value > 1.25*Value_clean, 
      c('flagObservationStatus_clean',
        'flagMethod_clean'):= list('E', 'e')]

ppnew[timePointYears == newYear & Value < 0.75*Value_clean, 
      c('flagObservationStatus_clean',
        'flagMethod_clean'):= list('E', 'e')]

ppnew[, method := 'tsclean']

#-- Manual outlier ----

ppout2 <- copy(priceData)
ppout2 <- ppout2[order(timePointYears)]
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
ppout2[!timePointYears %in% newYear, outlier2 := FALSE]
ppout2[outlier2 == TRUE, method := 'IQR_level']

outliers <- rbind(ppnew[flagObservationStatus_clean == 'E' & flagMethod_clean == 'e'], 
                  ppout[timePointYears %in% newYear & outlier2 == TRUE],
                  ppout2[timePointYears %in% newYear & outlier2 == TRUE],
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

priceData[ outliers, c('flagObservationStatus', 'flagMethod') := list('E','e'),
       on = c('geographicAreaM49', 'timePointYears', 'measuredItemCPC')]

outlierDetected <- priceData[flagObservationStatus == 'E' & flagMethod == 'e']

#-- Data Saving ----
SaveData(domainPP, datasetPrep, outlierDetected)

# Save in datatable

revisions <- copy(outlierDetected[measuredElement == '5531'])

names(revisions) <- tolower(names(revisions))


m49 <- GetCodeList(domainPP, datasetVal, 'geographicAreaM49')[,.(code, description)]
cpc <- GetCodeList(domainPP, datasetVal, 'measuredItemCPC')[,.(code, description)]

revisions <- merge(revisions, m49, by.x = 'geographicaream49', by.y = 'code', all.x = T)
setnames(revisions, 'description', 'country')
revisions <- merge(revisions, cpc, by.x = 'measureditemcpc', by.y = 'code', all.x = T)
setnames(revisions, 'description', 'product')

if(nrow(revisions) > 0){
  od <- ReadDatatable('outlier_detected', readOnly = F)
  changeset <- Changeset('outlier_detected')
  AddDeletions(changeset, od[geographicaream49 %in% unique(revisions$geographicaream49)])
  Finalise(changeset)
  changeset <- Changeset('outlier_detected')
  AddInsertions(changeset, revisions[,c("measureditemcpc",
                                        "geographicaream49",
                                        "timepointyears",
                                        "value","country",
                                        "product"), with = F])
  Finalize(changeset)
}


#-- send Email with notification of correct execution ----

from = "sws@fao.org"
to = swsContext.userEmail
subject = "The plug-in has correctly run"
body = paste('. Number of outlier detected: ', nrow(outliers), sep = '')
sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)
paste0("Email sent to ", swsContext.userEmail)
