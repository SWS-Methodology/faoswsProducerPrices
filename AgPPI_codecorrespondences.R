
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
                     token = SETTINGS[["token"]])#'4e9d9a2e-5258-48b6-974f-3656d1af8217')
}

library(openxlsx)

conv <- read.xlsx('ECOteamDocuments/EUROSTAT_CPC_conversion.xlsx', sheet = 1)
conv <- as.data.table(conv)
domainPP <- 'prod_prices'
datasetPrep <- 'annual_producer_prices_prep'

cpc <- GetCodeList(domain = domainPP,
                   dataset = datasetPrep, 
                   dimension = "measuredItemCPC")[ , .(code, description)]

conv <- merge(conv, cpc, by.x = 'CPC', by.y = 'code', all.x = T)

names(conv)

conv <- merge(conv, cpc, by.x = 'X3', by.y = 'code', all.x = T, suffixes = c('1', '2'))
conv <- merge(conv, cpc, by.x = 'X4', by.y = 'code', all.x = T)
names(conv)
setcolorder(conv,c("PRODUCT", "CPC",  "description1", 'X3', "description2", 'X4', "description"))
setnames(conv, c("PRODUCT", "CPC",  "description1", 'X3', "description2", 'X4', "description"), 
         c("EUROSTAT", "CPC1",  "description1", 'CPC2', "description2", 'CPC3', "description3"))

write.xlsx(conv, 'correspondenceTableEUROSTAT_CPC.xlsx')
