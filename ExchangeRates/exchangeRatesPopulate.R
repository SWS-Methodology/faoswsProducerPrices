if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])
}


pricevalKey = DatasetKey(
  domain = 'Fisheries',
  dataset = 'exchange_rates_fi',
  dimensions = list(
    Dimension(name = "from_currency",
              keys = GetCodeList('Fisheries', 'exchange_rates_fi', 'from_currency')[, code]),
    Dimension(name = "to_currency", 
              keys = GetCodeList('Fisheries', 'exchange_rates_fi', 'to_currency')[, code]),
    Dimension(name = "split_year",
              keys = GetCodeList('Fisheries', 'exchange_rates_fi', 'split_year')[, code]),
    Dimension(name = "timePointYears", 
              keys = GetCodeList('Fisheries', 'exchange_rates_fi', 'timePointYears')[, code])))

priceVal <- GetData(
  pricevalKey,
  flags = FALSE)
xrfish <- priceVal[split_year == 0]

if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])
}


curr2geo <- ReadDatatable('lcu_2_m49')

xr2processAll <- merge(xrfish, curr2geo, by.x = 'from_currency', by.y = 'code_iso', all.x = T,
                    allow.cartesian = T)

xr2process <- xr2processAll[timePointYears>=1990]

if(!all(unique(xr2process$from_currency) %in% GetCodeList('common', 'exchange_rates_annual', 'from_currency')$code)){
  print('Currency code missing!')}
  
if(nrow(xr2process[timePointYears < start_year_iso])>0){
  print('Problem with currency-country time')
}

if(nrow(xr2process[is.na(code_m49) & from_currency != 'ECU'])>0){
  print('Missing country!')
}


xrfinal <- copy(xr2process)
xrfinal[,Value:= 1/Value]
names(xrfinal)
xrfinalcol <- xrfinal[,c("from_currency",
                         "to_currency",
                         "timePointYears",
                         "code_m49",
                         "Value"), with = F]
xrfinalcol[,measuredElement := 'LCU']
xrfinalcol[,flagObservationStatus := 'P'] # Provisional
xrfinalcol[,flagMethod := 'c'] # copied from Fisheries

setnames(xrfinalcol, c("code_m49"), c('geographicAreaM49'))

if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = '9dd71bef-3bbb-48ba-aee7-c88070a93be8')
}

SaveData(domain = 'common', dataset = 'exchange_rates_annual', data = xrfinalcol)
