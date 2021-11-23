
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
                     token = '4c304ada-522c-4110-bac6-34a3bc0703e8')  #SETTINGS[["token"]])#'4e9d9a2e-5258-48b6-974f-3656d1af8217')
}



erKey = DatasetKey(
  domain = 'common',
  dataset = 'exchange_rates_annual',
  dimensions = list(
    Dimension(name = 'geographicAreaM49',
              keys = GetCodeList('common', 'exchange_rates_annual', 'geographicAreaM49')[, code]),
    Dimension(name = "from_currency",
              keys = GetCodeList('common', 'exchange_rates_annual', 'from_currency')[code != 'ECU', code]),
    Dimension(name = "to_currency", 
              keys = GetCodeList('common', 'exchange_rates_annual', 'to_currency')[code == 'USD', code]),
    Dimension(name = 'measuredElement',
              keys = 'LCU'),
    Dimension(name = "timePointYears", 
              keys = GetCodeList('common', 'exchange_rates_annual', 'timePointYears')[, code]))
  
)

erdt <- GetData(erKey, flags = F) 

erdt2 <- nameData('common', 'exchange_rates_annual', erdt)

View(erdt2[,.N, geographicAreaM49_description])

complete <- as.data.table(expand.grid(timePointYears = as.character(1970:2020),
                                            geographicAreaM49 = as.character(unique(erdt2$geographicAreaM49))))

check <- merge(complete, erdt2, by = c('geographicAreaM49', 'timePointYears'), all = T)

missing <- unique(check[is.na(geographicAreaM49_description)]$geographicAreaM49)

View(check[geographicAreaM49 %in% missing])

m49 <- GetCodeList('common', 'exchange_rates_annual', 'geographicAreaM49')[type == 'country']

m49[, c('description', 'selectionOnly',    'type')] <- NULL
m49[is.na(startDate), startDate := '1900']
m49[is.na(endDate), endDate := '3000']
check3 <- merge(check[geographicAreaM49 %in% missing], m49, by.x = 'geographicAreaM49', by.y = 'code', all.x = T)
check3[, startYear := gsub('-.*', '', startDate)]
check3[, endYear := gsub('-.*', '', endDate)]

check4 <- check3[timePointYears > startYear]
unique(check[geographicAreaM49 %in% unique(check3[timePointYears < startYear]$geographicAreaM49)]$geographicAreaM49_description)
check4 <- check4[timePointYears < endYear]
library(openxlsx)

write.xlsx(check4, 'xr_non_continuous_series.xlsx')

