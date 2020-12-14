# The shiny app is based on the Macro used during the last ten years by the producer prices team.

 .libPaths( c("/usr/local/lib64/R-3.1.2/library","/work/SWS_R_Share/shiny/Rlib/3.1",.libPaths()))
 

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

localrun <- TRUE

#-- Token QA ----

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


domainPP <- 'prod_prices'
datasetPrep <- 'annual_producer_prices_prep'
datasetVal <- 'annual_producer_prices_validated'

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

SLCelement <- '5531'
# cpc_list <- sort(sprintf("%s - %s", cpc$description, cpc$code))
# cpc_list <- data.table(label = country_input, code = )
  
#-- Datasets ----
priceKey = DatasetKey(
  domain = domainPP,
  dataset = datasetPrep,
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = GetCodeList(domainPP, datasetPrep, 'geographicAreaM49')[, code]),
    Dimension(name = "measuredElement", 
              keys = GetCodeList(domainPP, datasetPrep, 'measuredElement')[code == SLCelement, code]),
    Dimension(name = "measuredItemCPC",
              keys = GetCodeList(domainPP, datasetPrep, 'measuredItemCPC')[, code]),
    Dimension(name = "timePointYears", 
              keys = as.character((as.numeric(format(Sys.Date(), '%Y'))-10):format(Sys.Date(), '%Y')))))

    priceData <- GetData(
      priceKey,
      flags = TRUE)
    priceData <- nameData(domainPP, datasetPrep, priceData)
    priceData[,timePointYears_description := NULL]

outlierlist <- unique(priceData[flagObservationStatus == 'E' & flagMethod == 'i', .(geographicAreaM49_description,
                                                                                    measuredItemCPC_description)])

outlier_input <-  sort(sprintf("%s - %s", outlierlist$geographicAreaM49_description,
                               outlierlist$measuredItemCPC_description))    


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

#----


