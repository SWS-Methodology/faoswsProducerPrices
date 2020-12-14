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

#-- Pull Fisheries XR data from PROD environment ----

# Pull exchange rates dataset
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

fixr <- GetData(erKey, flags = F) 


if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = 'c9cd8ef0-5dee-49ed-8b40-93c96c917fdf')
}

#-- Prod prices data ----

ppxr <- fread('Exchange_rate_E_All_Data_(Normalized).csv', colClasses = c(rep('character', 9),
                                                                          'numeric', rep('character', 2)))

ppxr[Currency == 'Euro' & Area != 'Germany', Value := NA]
ppxr <- ppxr[!is.na(Value)]

#-- CLFS team ----

fbsxr <- ReadDatatable('eur_conversion_usd')

#-- IMF ----

library(readxl)

imfxr0 <- read_xlsx('International_Financial_Statistics.xlsx', sheet = 1, skip = 1)
imfxr <- melt(imfxr0, id.vars = 1,
              measure.vars = 2:ncol(imfxr0),
              variable.name = 'timePointYears',
              value.name = 'Value')
imfxr <- as.data.table(imfxr)[!is.na(Value)]
setnames(imfxr, 'X__1', 'Area') 
unique(ppxr$Area)
unique(imfxr[!Area %in% ppxr$Area ]$Area)
#-- Initial analysis ----

min(fixr$timePointYears)
max(fixr$timePointYears)

min(ppxr$`Year Code`)
max(ppxr$`Year Code`)

fixr1 <- fixr[timePointYears %in% unique(ppxr$Year)]
fixr1 <- fixr1[,.(from_currency, timePointYears, Value)]
names(fixr1)
names(ppxr)
ppxr1 <- ppxr[ ,c("ISO Currency Code", 'Year', 'Value'), with = F]
setkey(ppxr1)
ppxr1 <- unique(ppxr1)

min(fixr1$timePointYears)
max(fixr1$timePointYears)

min(ppxr$Year)
max(ppxr$Year)

# Rows FI: 7305
nrow(fixr1)

# Rows PP: 7685
nrow(ppxr1)

#-- Merge FI and PP ----

fipp <- merge(fixr1, ppxr1, by.x = c('from_currency', 'timePointYears'),
              by.y = c("ISO Currency Code", 'Year'), all = T,
              suffixes = c('_fi', '_pp'))
fipp[,Value_pp := as.numeric(Value_pp)]
fipp[, Value_pp_1 := 1/Value_pp]

fipp[, Diff := Value_fi-Value_pp_1]

fipp[, problem := 0]
fipp[abs(Diff) > 0.1 , problem := 1]

currencyproblem <- unique(fipp[problem == 1]$from_currency)
currencyproblemID <- GetCodeList(domain = 'producerprices', 
                                 'exchange_rates_test', 'from_currency')[code %in% currencyproblem]
write.csv(paste(currencyproblemID$description, collapse = ', '), 'currencyProblems.csv', row.names = F)
