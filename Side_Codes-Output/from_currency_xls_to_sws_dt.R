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
  library(readxl)
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

doc <- read_xls('ECOteamDocuments/currency.xls', sheet = 1)
doc2 <- read_xls('ECOteamDocuments/currency.xls', sheet = 2)

doc3 <- read_xls('ECOteamDocuments/currency.xls', sheet = 3)
doc4 <- read_xls('ECOteamDocuments/currency.xls', sheet = 4)


unique(doc$pk_Currency_Code)[! unique(doc$pk_Currency_Code) %in% 
                               unique(doc2$fk_CurrencyCountry_CurrencyCode_Currency)]

unique(doc2$fk_CurrencyCountry_CurrencyCode_Currency)[! unique(doc2$fk_CurrencyCountry_CurrencyCode_Currency) %in% 
                                                        unique(doc$pk_Currency_Code) ]

unique(doc$pk_Currency_Code)[! unique(doc$pk_Currency_Code) %in% 
                               unique(doc3$fk_CurrencyCountryXC_NewCurrencyCode_Currency)]

unique(doc3$fk_CurrencyCountryXC_OldCurrencyCode_Currency)[! unique(doc3$fk_CurrencyCountryXC_OldCurrencyCode_Currency) %in%
                                                             doc4$fk_CurrencyDollarXC_CurrencyCode_Currency]

unique(doc4$fk_CurrencyDollarXC_CurrencyCode_Currency)[! doc4$fk_CurrencyDollarXC_CurrencyCode_Currency %in%
                                                         unique(doc$pk_Currency_Code)]

unique(doc$pk_Currency_Code)[!unique(doc$pk_Currency_Code) %in% 
                               unique(doc4$fk_CurrencyDollarXC_CurrencyCode_Currency)]


ccdt <- ReadDatatable('country_currency')
iso <- GetCodeList('producerprices', 'exchange_rates_test', 'from_currency')

View(iso[!code %in% unique(ccdt$currency_code)])
unique(doc3$fk_CurrencyCountryXC_NewCurrencyCode_Currency)[ ! unique(doc3$fk_CurrencyCountryXC_NewCurrencyCode_Currency) %in% 
                                                              unique(iso$code)]

unique(doc3$fk_CurrencyCountryXC_OldCurrencyCode_Currency)[ ! unique(doc3$fk_CurrencyCountryXC_OldCurrencyCode_Currency) %in% 
                                                              unique(iso$code)]


ccdt[, end_date := as.POSIXct(as.integer(gsub('.{3}$', '',end_date)), origin = '1970-01-01')]
ccdt[, start_date := as.POSIXct(as.integer(gsub('.{3}$', '',start_date)), origin = '1970-01-01')]
doc2 <- as.data.table(doc2)
doc2[ , m49 := fs2m49(as.character(fk_CurrencyCountry_AreaCode_Area)) ]
checkswsdt <- merge(doc2, ccdt, by.x = c('fk_CurrencyCountry_CurrencyCode_Currency', 'm49'),
by.y = c('currency_code', 'country_code'), all = T)

withdr <- read_xls('ECOteamDocuments/list_historical_currency_withdrawal.xls')

checkenddate <- merge(doc2, withdr, by.x = 'fk_CurrencyCountry_CurrencyCode_Currency',
      by.y = 'X__2', all = T)
