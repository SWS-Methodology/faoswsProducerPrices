# The shiny app is based on the Macro used during the last ten years by the producer prices team.

.libPaths( c( 
  "/home/shiny/R/x86_64-pc-linux-gnu-library/3.2",
  "/usr/local/lib64/R-3.1.2/library",
  "/work/SWS_R_Share/shiny/Rlib/3.1",
  .libPaths()))


suppressMessages({
  
  library(plotly)
  #library(data.table)
  library(DT)
  #library(ggplot2)
  library(zoo)
  
  library(rhandsontable)
  library(shiny)
  library(shinyWidgets)
  #library(shinyalert)
  
  library(faosws)
  library(faoswsFlag)
  library(faoswsProcessing)
  library(faoswsUtil)
  library(faoswsImputation)
  
  
  library(tseries)
  library(forecast)
  library(imputeTS)
})

localrun <- TRUE
live <- TRUE

#-- Token QA ----

if(live){
if(localrun){
  if(CheckDebug()){
    library(faoswsModules)
    SETTINGS = ReadSettings("sws.yml")
    R_SWS_SHARE_PATH = SETTINGS[["share"]]
    SetClientFiles(SETTINGS[["certdir"]])
    GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                       token = '0211dad4-4c6a-4f66-a2f0-f21a1edef8a2')
  }
  
} else {
  R_SWS_SHARE_PATH = "Z:"
  SetClientFiles("/srv/shiny-server/.R/QA/")
  GetTestEnvironment(baseUrl = "https://swsqa.aws.fao.org:8181",
                     token = "04fc0c00-a4f3-4640-bee6-49a906863095")
}

  
tokenTab <- ReadDatatable('pp_token', readOnly = FALSE)
  
tokenOutlier <- tokenTab$token[1]
tokenSeries <- tokenTab$token[2]

dateOutlierToken <- tokenTab$last_upd[1]

domainPP <- 'prod_prices'
datasetPrep <- 'annual_producer_prices_prep'
datasetVal <- 'annual_producer_prices_validated'

startYear <- as.character(as.numeric(format(Sys.Date(), '%Y')) - 10)

# Country input all countries in M49
M49 <- GetCodeList(domain =domainPP, 
                   dataset = datasetPrep, 
                   dimension = "geographicAreaM49")

M49 <- M49[ type == "country", .( description, code)]
M49[, description := iconv(description, from = 'UTF-8')]

country_input <-  sort(sprintf("%s - %s", M49$description, as.numeric(M49$code)))
country_input <- data.table(label = country_input, code = sub(" ", "", sub(".*-", "", country_input)))

# Years from 1991 until present
currentYear <- as.numeric(gsub("\\-[0-9]*", "", Sys.Date()))
years_input <- as.character(sort(1991:currentYear, decreasing = TRUE))

# Products from CPC dimension
cpc <- GetCodeList(domain = domainPP,
                   dataset = datasetPrep, 
                   dimension = "measuredItemCPC")[ , .(code, description)]

cpc_list <- sort(sprintf("%s - %s", cpc$description, cpc$code))
cpc_list <- data.table(label = cpc_list, code = sub(" ", "", sub(".*-", "", cpc_list)))

currElement <- c('5531', '5532', '5530')
#-- Datasets ----

# Preparation
priceKey = DatasetKey(
  domain = domainPP,
  dataset = datasetPrep,
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = GetCodeList(domainPP, datasetPrep, 'geographicAreaM49')[, code]),
    Dimension(name = "measuredElement", 
              keys = GetCodeList(domainPP, datasetPrep, 'measuredElement')[code %in% currElement, code]),
    Dimension(name = "measuredItemCPC",
              keys = GetCodeList(domainPP, datasetPrep, 'measuredItemCPC')[, code]),
    Dimension(name = "timePointYears", 
              keys = GetCodeList(domainPP, datasetPrep, 'timePointYears')[code >= startYear, code])))

priceData <- GetData(
  priceKey,
  flags = TRUE)
priceData <- nameData(domainPP, datasetPrep, priceData)
priceData[,timePointYears_description := NULL]

priceDataLCU <- priceData[measuredElement == '5530' ]
priceDataSLC <- priceData[measuredElement == '5531' ]
priceDataUSD <- priceData[measuredElement == '5532' ]

maxYear <- as.character(max(as.numeric(priceData$timePointYears)))
#-- Estimated data proposed

priceproposed <- ReadDatatable('imputation_annual_prices', readOnly = F)
priceproposed <- priceproposed[timepointyears == max(timepointyears)]
priceproposed <- priceproposed[approach != 'Kalman']

# Validated

# pricevalKey = DatasetKey(
#   domain = domainPP,
#   dataset = datasetVal,
#   dimensions = list(
#     Dimension(name = "geographicAreaM49",
#               keys = GetCodeList(domainPP, datasetVal, 'geographicAreaM49')[, code]),
#     Dimension(name = "measuredElement", 
#               keys = GetCodeList(domainPP, datasetVal, 'measuredElement')[code %in% currElement, code]),
#     Dimension(name = "measuredItemCPC",
#               keys = GetCodeList(domainPP, datasetVal, 'measuredItemCPC')[, code]),
#     Dimension(name = "timePointYears", 
#               keys = GetCodeList(domainPP, datasetPrep, 'timePointYears')[code >= startYear, code])))
# 
# priceVal <- GetData(
#   pricevalKey,
#   flags = TRUE)
# # priceMD <- GetMetadata(pricevalKey)
# 
# setnames(priceVal, 'flag_obs_status_v2', 'flagObservationStatus')
# price2val <- nameData(domainPP, datasetVal, priceVal)
# price2val[,timePointYears_description := NULL]

# -- Price ratio ----

ppr <- ReadDatatable('pp_tcf')

# -- Geo and CPC hierachy + geo-currency link

geohier <- ReadDatatable('geographic_hierarchy_ebx5')
cpchier <- ReadDatatable('cpc_hierarchy_ebx5')
lcu_2_m49 <- ReadDatatable('lcu_2_m49')

# Check if all products are there!!!!!!!1
comm_groups <- unique(cpchier[code_l2 %in% c('01','02','21') & !is.na(name_en_l3),.(name_en_l3, code_l3)])

# -- Other data ----

toi <- ReadDatatable('toi_data')

} else {
  
#  load('data2use.RData')
 load('dataInput.RData')
  }



#- New ExpandYear function ----
expandYear <- function (data, areaVar = "geographicAreaM49", elementVar = "measuredElement", 
                        itemVar = "measuredItemCPC", yearVar = "timePointYears", 
                        valueVar = "Value", obsflagVar = "flagObservationStatus", 
                        methFlagVar = "flagMethod", newYears = NULL) 
{
  key = c(elementVar, areaVar, itemVar)
  keyDataFrame = data[, key, with = FALSE]
  keyDataFrame = keyDataFrame[with(keyDataFrame, order(get(key)))]
  keyDataFrame = keyDataFrame[!duplicated(keyDataFrame)]
  yearDataFrame = unique(data[, get(yearVar)])
  if (!is.null(newYears)) {
    yearDataFrame = unique(c(yearDataFrame, newYears, newYears - 
                               1, newYears - 2))
  }
  yearDataFrame = data.table(yearVar = yearDataFrame)
  colnames(yearDataFrame) = yearVar
  completeBasis = data.table(merge.data.frame(keyDataFrame, 
                                              yearDataFrame))
  expandedData = merge(completeBasis, data, by = colnames(completeBasis), 
                       all.x = TRUE)
  expandedData = fillRecord(expandedData, areaVar = areaVar, 
                            itemVar = itemVar, yearVar = yearVar,
                            flagObsVar = obsflagVar, 
                            flagMethodVar = methFlagVar)
  seriesToBlock = expandedData[(get(methFlagVar) != "u"), ]
  seriesToBlock[, `:=`(lastYearAvailable, max(get(yearVar))), 
                by = key]
  seriesToBlock[, `:=`(flagComb, paste(get(obsflagVar), get(methFlagVar), 
                                       sep = ";"))]
  seriesToBlock = seriesToBlock[get(yearVar) == lastYearAvailable & 
                                  flagComb == "M;-"]
  if (nrow(seriesToBlock) > 0) {
    seriesToBlock = seriesToBlock[, {
      max_year = max(as.integer(.SD[, timePointYears]))
      data.table(timePointYears = seq.int(max_year + 1, 
                                          newYears), Value = NA_real_, flagObservationStatus = "M", 
                 flagMethod = "-")[max_year < newYears]
    }, by = key]
    expandedData = merge(expandedData, seriesToBlock, by = c(areaVar, 
                                                             elementVar, itemVar, yearVar), all.x = TRUE, suffixes = c("", 
                                                                                                                       "_MDash"))
    expandedData[!is.na(flagMethod_MDash), `:=`(flagMethod, 
                                                flagMethod_MDash)]
    expandedData = expandedData[, colnames(data), with = FALSE]
  }
  expandedData
}


imputeVariable <- function(data, imputationParameters){
  if (!exists("ensuredImputationData") || !ensuredImputationData) 
    ensureImputationInputs(data = data, imputationParameters = imputationParameters)
  if (imputationParameters$newImputationColumn == "") {
    newValueColumn = imputationParameters$imputationValueColumn
    newObsFlagColumn = imputationParameters$imputationFlagColumn
    newMethodFlagColumn = imputationParameters$imputationMethodColumn
  }
  else {
    newValueColumn = paste0("Value_", imputationParameters$newImputationColumn)
    newObsFlagColumn = paste0("flagObservationStatus_", 
                              imputationParameters$newImputationColumn)
    newMethodFlagColumn = paste0("flagMethod_", imputationParameters$newImputationColumn)
  }
  imputeSingleObservation(data, imputationParameters)
  missingIndex = data[[imputationParameters$imputationFlagColumn]] == 
    "M" & data[[imputationParameters$imputationMethodColumn]] == 
    "u"
  ensemble = ensembleImpute(data = data, imputationParameters = imputationParameters)
  if(!is.null(nrow(ensemble))) {
    data = cbind(data, ensemble)
  
    data[missingIndex & !is.na(ensemble), `:=`(c(newValueColumn), ensemble)]
    data = data[, `:=`(ensemble, NULL)]
  }
  imputedIndex = missingIndex & !is.na(data[[newValueColumn]])
  invisible(data[imputedIndex, `:=`(c(newObsFlagColumn, newMethodFlagColumn), 
                                    list(imputationParameters$imputationFlag, imputationParameters$newMethodFlag))])
  return(data)
}