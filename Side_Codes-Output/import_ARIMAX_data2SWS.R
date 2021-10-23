suppressMessages({
#  library(readxl)
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
  library(openxlsx)
})



#-- Token QA ----

if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = '0211dad4-4c6a-4f66-a2f0-f21a1edef8a2')
}


ppdt <- read.xlsx('producer price_41-80_ARIMAX results.xlsx')
ppdt <- as.data.table(ppdt)
ppdt$faocode <- as.character(ppdt$faocode)
ppdt$cropcode <- as.character(ppdt$cropcode)
ppdt$year <- as.character(ppdt$year)


names(ppdt)
#unique(ppdt$data_type)
ppann <- copy(ppdt) #[data_type == 'annual']
ppann[, geographicAreaM49 := fs2m49(faocode)]

ppann[nchar(cropcode) == 3, 
      cropcode:= paste('0', cropcode, sep = '')]

ppann[nchar(cropcode) == 2, 
      cropcode:= paste('00', cropcode, sep = '')]

ppann[ , measuredItemCPC := fcl2cpc(cropcode)]

setnames(ppann, c('currency', 'year'), 
         c('measuredElement', 'timePointYears'))
ppann[measuredElement == 'USD' , measuredElement := '5532']

unique(ppann$pp_flag)
unique(ppann$method)

ppann[pp_flag == 'I', c('flagObservationStatus',
                       'flagMethod') := list('I', 'e')]

ppann[pp_flag == 'q', c('flagObservationStatus',
                        'flagMethod') := list('', 'q')]

ppann[, c('pp_flag', 'method', 'ID', "faocode",
          "cropcode", "faoname"  ) ] <- NULL

setnames(ppann, c('pp_final'),
         c('Value'))

domainPP <- 'prod_prices'
#datasetVal <- 'annual_producer_prices_validation'
datasetPrep <- 'annual_producer_prices_legacy_arimax' 
SLCelement <- '5532'


preppriceKey = DatasetKey(
  domain = domainPP,
  dataset = datasetPrep,
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = GetCodeList('agriculture', 'aproduction', 'geographicAreaM49')[, code]),
    Dimension(name = "measuredElement", 
              keys = SLCelement),
    Dimension(name = "measuredItemCPC",
              keys = GetCodeList('agriculture', 'aproduction', 'measuredItemCPC')[, code]),
    Dimension(name = "timePointYears", 
              keys = GetCodeList(domainPP, datasetPrep, 'timePointYears')[code >= as.character(min(as.numeric(ppann$timePointYears))), code]))
  
)

prep_price0 <- GetData(preppriceKey, flags = TRUE)

comp <- merge(prep_price0, ppann, by = c("timePointYears",
                                 "measuredElement",
                                 "geographicAreaM49",
                                 "measuredItemCPC"), all = TRUE, suffixes = c('DT', 'AR'))
comp[,.N]
comp[is.na(ValueDT) & !is.na(ValueAR),.N] 
comp[!is.na(ValueDT) & is.na(ValueAR),.N] 
comp[!is.na(ValueDT) & !is.na(ValueAR),.N]
comp[is.na(ValueDT) & is.na(ValueAR),.N]
ppann[is.na(Value), c('flagObservationStatus',
                      'flagMethod', 'Value') := list('O', 'q', 0)]
ppann[is.na(Value)]

erKey = DatasetKey(
  domain = 'common',
  dataset = 'exchange_rates_annual',
  dimensions = list(
    Dimension(name = 'geographicAreaM49',
              keys = GetCodeList('common', 'exchange_rates_annual', 'geographicAreaM49')[code %in% c(unique(ppann$geographicAreaM49), '156') , code]),
    Dimension(name = "from_currency",
              keys = GetCodeList('common', 'exchange_rates_annual', 'from_currency')[code != 'ECU' & endDate >= '1991-01-01' | 
                                                                                       is.na(endDate), code]),
    Dimension(name = "to_currency", 
              keys = GetCodeList('common', 'exchange_rates_annual', 'to_currency')[code == 'USD', code]),
    Dimension(name = 'measuredElement',
              keys = c('LCU', 'SLC')),
    Dimension(name = "timePointYears", 
              keys = GetCodeList('common', 'exchange_rates_annual', 'timePointYears')[code %in% unique(ppann$timePointYears), code]))
  
)

erdt <- GetData(erKey, flags = F) 

erdt2 <- dcast(erdt, geographicAreaM49 + from_currency + timePointYears ~ measuredElement, value.var = 'Value')
lcu <- erdt2[!is.na(LCU)]
slc <- erdt2[!is.na(SLC)]

ppann[geographicAreaM49 == '1248', geographicAreaM49 := '156']

conlcu <- merge(lcu, ppann, by = c("geographicAreaM49", "timePointYears"), all = T)
conlcu$Value <- as.numeric(conlcu$Value)
conlcu[ , convValue := Value*LCU]
conlcu[ , c('LCU', 'SLC', 'Value')] <- NULL
setnames(conlcu, 'convValue', 'Value')
conlcu[, measuredElement := '5530']


conslc <- merge(slc, ppann, by = c("geographicAreaM49", "timePointYears"), all = T)
conslc$Value <- as.numeric(conslc$Value)
conslc[ , convValue := Value*SLC]
conslc[ , c('LCU', 'SLC', 'Value')] <- NULL
setnames(conslc, 'convValue', 'Value')
conslc[, measuredElement := '5531']

tot <- rbind(conlcu[, from_currency := NULL], conslc[, from_currency := NULL], ppann)
tot <- tot[!is.na(Value)]
tot[,.N, measuredElement]

includemetadata <- copy(tot[flagObservationStatus == 'I',c("geographicAreaM49", "measuredItemCPC", "timePointYears", "measuredElement"), with = F])
includemetadata[,Metadata:="GENERAL"]
includemetadata[,Metadata_Element:="COMMENT"]
includemetadata[,Metadata_Language:="en"]
includemetadata[ ,  Metadata_Value := "ARIMAX"]


if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = '07cb9584-b15d-442f-8498-e83c00872f9c')
}



SaveData( domain = domainPP,
          dataset = datasetPrep,
          data = tot,
          metadata = includemetadata)

preppriceKey2 = DatasetKey(
  domain = domainPP,
  dataset = datasetPrep,
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = GetCodeList('agriculture', 'aproduction', 'geographicAreaM49')[, code]),
    Dimension(name = "measuredElement", 
              keys = SLCelement),
    Dimension(name = "measuredItemCPC",
              keys = GetCodeList('agriculture', 'aproduction', 'measuredItemCPC')[, code]),
    Dimension(name = "timePointYears", 
              keys = GetCodeList(domainPP, datasetPrep, 'timePointYears')[code >= as.character(min(as.numeric(ppann$timePointYears))), code]))
  
)

prep_pricenew <- GetData(preppriceKey, flags = TRUE)
xxx <- merge(tot, prep_pricenew, by = c("geographicAreaM49", "measuredItemCPC", "timePointYears", "measuredElement"), all = T,
      suffixes = c('_file','_sws'))


xxx[measuredElement == '5532' & is.na(Value_sws) & !is.na(Value_file),.N]
