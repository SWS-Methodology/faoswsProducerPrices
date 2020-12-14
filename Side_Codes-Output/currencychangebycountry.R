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
                     token = 'd94e5437-4d18-4705-8634-9db153fa6c96')
}


lcu_2_m49 <- ReadDatatable('lcu_2_m49')

xr <- fread('ECOteamDocuments/countrycurrencyXR.csv')

sws <- GetCodeList('producerprices', 'exchange_rates_test', 'from_currency')

missingcodes <- c(unique(xr[!to_currency %in% sws$code]$to_currency), 
  unique(xr[!from_currency %in% sws$code]$from_currency))

missingcodes <- unique(missingcodes)
write.csv(missingcodes, 'codesInPPinputSystemNotInSWS.csv')

xryear <- merge(xr, sws[,.(code, endDate)], by.x = 'from_currency', by.y = 'code')
xryear[,endDate := format(as.Date(endDate), "%Y")]

xrm49 <- merge(xryear, unique(lcu_2_m49[,.(code_iso, code_m49)]), by.y = 'code_iso', 
               by.x = 'from_currency', all.x = T, allow.cartesian = T)

xrm49 <- merge(xrm49, unique(lcu_2_m49[,.(code_iso, code_m49)]), by.y = 'code_iso', 
               by.x = 'to_currency', all.x = T, allow.cartesian = T)

xrcountry <- xrm49[code_m49.x == code_m49.y]

xrcountry[ , code_m49.x := NULL]
setnames(xrcountry, c("to_currency",
                      "from_currency",
                      "Value",
                      "endDate",
                      "code_m49.y"),
         c("to_currency",
           "from_currency",
           "Value",
           "timePointYears",
           "geographicAreaM49"))

xrcountry[,c('flagObservationStatus', 'flagMethod', 'flagCurrency') := list('B', 'p', '')]
write.csv(xrcountry, 'xrcountry2upload.csv', row.names = F)
SaveData(domain = 'producerprices', dataset = 'exchange_rates_test', data = xrcountry)
