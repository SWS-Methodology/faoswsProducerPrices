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