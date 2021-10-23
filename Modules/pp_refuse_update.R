# Plugin to transfer from imputed DT to dataset

# -- Load Packages ----

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
  library(sendmailR)
})

# -- Token QA ----

if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = '38d4e9be-4a56-4356-8c7f-95927fd9c441')# SETTINGS[["token"]])#'4c304ada-522c-4110-bac6-34a3bc0703e8')  #SETTINGS[["token"]])#'4e9d9a2e-5258-48b6-974f-3656d1af8217')
}


conflict <- ReadDatatable('revisions2control')
conflict2update <- conflict[refuse_update == TRUE]

setnames(conflict2update, c("geographicaream49",
                            "timepointyears",
                            "measuredelement",
                            "measureditemcpc",
                            "value_old",
                            "flagobservationstatus_old",
                            "flagmethod_old"),
         c("geographicAreaM49",
           "timePointYears",
           "measuredElement",
           "measuredItemCPC",
           "Value",
           "flagObservationStatus",
           "flagMethod"))


setkey(conflict2update)
codes <- conflict2update[,c("geographicAreaM49",
                            "timePointYears",
                            "measuredItemCPC"), with =F]

domainPP <- 'prod_prices'
datasetVal <- 'annual_producer_prices_validation'
datasetPrep <- 'annual_producer_prices_prep'

valpriceKey = DatasetKey(
  domain = domainPP,
  dataset = datasetVal,
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = GetCodeList(domainPP, datasetVal, 'geographicAreaM49')[code %in% unique(codes$geographicAreaM49), code]),
    Dimension(name = "measuredElement", 
              keys = GetCodeList(domainPP, datasetVal, 'measuredElement')[code %in% c('5530', '5531', '5532'), code]),
    Dimension(name = "measuredItemCPC",
              keys = GetCodeList(domainPP, datasetVal, 'measuredItemCPC')[code %in% unique(codes$measuredItemCPC), code]),
    Dimension(name = "timePointYears", 
              keys = GetCodeList(domainPP, datasetVal, 'timePointYears')[code %in% unique(codes$timePointYears), code]))
  
)

val_price <- GetData(valpriceKey, flags = TRUE)

data2ripristinate <- val_price[codes, on = c("geographicAreaM49",
                                             "timePointYears",
                                             "measuredItemCPC")]


#-- Data Saving ----
SaveData(domainPP, datasetPrep, data2ripristinate)

paste0("Email sent to ", swsContext.userEmail)