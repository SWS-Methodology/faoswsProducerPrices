# The shiny app is based on the Macro used during the last ten years by the producer prices team.

# .libPaths( c("/usr/local/lib64/R-3.1.2/library","/work/SWS_R_Share/shiny/Rlib/3.1",
#             .libPaths()))

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

if(CheckDebug()){
  library(faoswsModules)
  sett <- ReadSettings("sws.yml")
  
  SetClientFiles(sett$certdir)
  GetTestEnvironment(sett$server, sett$token)
  files = dir("R", full.names = TRUE)
  invisible(sapply(files, source))
}

domain <- 'prod_prices'
dataset <- 'annual_producer_prices_validated'

# Country input all countries in M49
M49 <- GetCodeList(domain ="prod_prices", 
                   dataset = "annual_producer_prices_validated", 
                   dimension = "geographicAreaM49")

M49 <- M49[ type == "country", .( description, code)]
M49[, description := iconv(description)]

country_input <-  sort(sprintf("%s - %s", M49$description, as.numeric(M49$code)))
country_input <- data.table(label = country_input, code = sub(" ", "", sub(".*-", "", country_input)))

# Years from 1991 until present
currentYear <- as.numeric(gsub("\\-[0-9]*", "", Sys.Date()))
years_input <- as.character(sort(1991:currentYear, decreasing = TRUE))

# Products from CPC dimension
cpc <- GetCodeList(domain ="prod_prices",
                   dataset = "annual_producer_prices_validated", 
                   dimension = "measuredItemCPC")[ , .(code, description)]

# cpc_list <- sort(sprintf("%s - %s", cpc$description, cpc$code))
# cpc_list <- data.table(label = country_input, code = )
  
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


