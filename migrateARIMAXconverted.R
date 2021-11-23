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
  library(openxlsx)
})

# -- Token QA ----

if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = '4f3e6048-1ba0-40e3-a908-5e14a45d9c6b')  #SETTINGS[["token"]])#'4e9d9a2e-5258-48b6-974f-3656d1af8217')
}

domainPP <- 'prod_prices'
datasetPrep <- 'annual_producer_prices_validation' 

valpriceKey = DatasetKey(
  domain = domainPP,
  dataset = datasetPrep,
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = GetCodeList(domainPP, datasetPrep, 'geographicAreaM49')[, code]),
    Dimension(name = "measuredElement",
              keys = GetCodeList(domainPP, datasetPrep, 'measuredElement')[code %in% c('5530', '5531', '5532'), code]),
    Dimension(name = "measuredItemCPC",
              keys = GetCodeList(domainPP, datasetPrep, 'measuredItemCPC')[, code]),
    Dimension(name = "timePointYears",
              keys = GetCodeList(domainPP, datasetPrep, 'timePointYears')[, code]))

)

val_price <- GetData(valpriceKey)


newarimax <- read.xlsx('ExchangeRates/PP_1-80 crops_ARIMAX_USD_AmandaFile.xlsx') # to update currency_code DT: sheet = 3)

# newarimax <- as.data.table(newarimax)
# 
# newarimax$pk <- NULL
# 
# setnames(newarimax, c( "NewCurrency", "OldCurrency", "XCRate" ), c('new_currency_code','old_currency_code','exchange_rate'))
# 
# ReadDatatable('currency_changes')
# chng <- Changeset('currency_changes')
# AddInsertions(chng, newarimax)
# Finalize(chng)


# newarimax <- read.xlsx('ExchangeRates/PP_1-80 crops_ARIMAX_USD_AmandaFile.xlsx', sheet = 4) # to update currency_code DT: sheet = 3)
# 
# newarimax <- as.data.table(newarimax)
# newarimax[,.N, areacode]
# newarimax[, areacode := as.character(areacode)]
# newarimax[ , geographicaream49 := fs2m49(areacode)]
# newarimax[geographicaream49 == '1248', geographicaream49 := '156']
# newarimax$areacode <- NULL
# # 
# newarimax$pk <- NULL
# # 
# setnames(newarimax, c("year", "currencycode" ), c('timepointyears','currency_code'))
# # 
# names(ReadDatatable('country_year_lcu')) %in% names(newarimax)
# 
# chng <- Changeset('country_year_lcu')
# AddInsertions(chng, newarimax)
# Finalize(chng)

# 
# newarimax <- as.data.table(newarimax)
# newarimax <- newarimax[!is.na(integrated_pprice)]

newarimax <- as.data.table(newarimax)
newarimax <- newarimax[!is.na(integrated_pprice)]
#View(newarimax[is.na(LCU_Value),])

newarimax$faocode <- as.character(newarimax$faocode)

newarimax[,.N,flag]

newarimax[, geographicAreaM49 := fs2m49(faocode)]


newarimax[geographicAreaM49 == '1248', geographicAreaM49 := '156']

newarimax[, cropcode0 := formatC(cropcode, width = 4, flag = "0")]
newarimax[, measuredItemCPC := fcl2cpc(cropcode0)]
newarimax[is.na(measuredItemCPC)]

names(newarimax)

newarimax[,c("ID", "faocode", "cropcode", "faoname", "pk",
             "currency", "USD_SLC_XC", "SLC_Currency_Code",
             "LCU_Code", "SLC_LCU_XC", "cropcode0")] <- NULL

dt2save <- melt(newarimax, measure.vars = c("integrated_pprice", "SLC_Value", "LCU_Value"))
dt2save[variable == "integrated_pprice", measuredElement := '5532']
dt2save[variable == "SLC_Value", measuredElement := '5531']
dt2save[variable == "LCU_Value", measuredElement := '5530']
dt2save[is.na(measuredElement)]
dt2save[, variable := NULL]
setnames(dt2save, c('year', 'flag', 'method', 'value'),
         c("timePointYears", "flagObservationStatus", "flagMethod", "Value"))

dt2save[flagObservationStatus == 'I', flagMethod := 'e']
dt2save[flagObservationStatus == 'q', flagMethod := 'q']
dt2save[flagMethod == 'q', flagObservationStatus := '']
dt2save <- dt2save[!is.na(Value)]
dt2save$timePointYears <- as.character(dt2save$timePointYears)

test <- merge(dt2save, val_price, by = c('geographicAreaM49', 'measuredItemCPC', 'measuredElement', "timePointYears"), 
              suffixes = c('AR', 'OR'),
      all = T)

test[,.N, .(flagObservationStatusOR, flagMethodOR)]

test[!is.na(ValueAR) & is.na(ValueOR),.N] + test[is.na(ValueAR) & !is.na(ValueOR),.N] + test[!is.na(ValueAR) & !is.na(ValueOR),.N]

test[, c('Value', 'flagObservationStatus', 'flagMethod') := list(ValueOR,
                                                                 flagObservationStatusOR,
                                                                 flagMethodOR)]

View(test[flagObservationStatusAR == ''])
test[,.N,flagMethodAR]
test[flagObservationStatusAR == '' & measuredElement == '5532',.N]
test[flagObservationStatusAR == '' & measuredElement == '5532' & ValueAR == ValueOR,.N]

test[flagObservationStatusAR == '',.N, .(flagObservationStatusOR, flagMethodOR)]

test[!flagObservationStatusAR %in% c('', 'X') &
       !flagObservationStatusOR %in% c('', 'X') &
       !is.na(ValueAR), c('Value', 'flagObservationStatus', 
                                                                   'flagMethod', 'Metadata_Value') := list(ValueAR,
                                                                                                           flagObservationStatusAR,
                                                                                                           flagMethodAR,
                                                                                                           "ARIMAX revision 2021")]

test[is.na(flagObservationStatusOR) & !is.na(ValueAR), c('Value', 'flagObservationStatus', 
                                                         'flagMethod', 'Metadata_Value') := list(ValueAR,
                                                                                                 flagObservationStatusAR,
                                                                                                 flagMethodAR,
                                                                                                 "ARIMAX revision 2021")]

test[Value != ValueAR,.N] + test[Value == ValueAR,.N] 


newdata <- test[,.(geographicAreaM49, measuredItemCPC, measuredElement, timePointYears,
                   Value, flagObservationStatus, flagMethod, Metadata_Value)]
newdata <- newdata[!is.na(Metadata_Value)]

metadt <-newdata[flagObservationStatus == 'I',.(geographicAreaM49, measuredItemCPC, measuredElement, timePointYears, Metadata_Value)]

metadt[,Metadata:="GENERAL"]
metadt[,Metadata_Element:="COMMENT"]
metadt[,Metadata_Language:="en"]
#metadt[,Metadata_Value:="ARIMAX revision 2021"]

write.xlsx(test[!is.na(Metadata_Value)], 'arimaxDataInserted.xlsx', row.names = FALSE)

SaveData(domain = domainPP, dataset = datasetPrep,data = newdata, metadata = metadt)
