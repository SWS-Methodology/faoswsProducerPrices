function(input, output) {


  #-- Data stored ----
  #Data to store: from questionnaire, preparation, validated and all (with data 0,M,u)
  
  dataPP <- reactiveValues(prepUSD =  data.table(),#priceDataUSD,
                           prepSLC = data.table(),#priceDataSLC,
                           prepLCU = data.table(),#priceDataLCU,
                           prepOut = data.table(),#rbind(priceDataLCU[flagObservationStatus == 'E' & flagMethod == 'e'],
                                           #priceDataSLC[flagObservationStatus == 'E' & flagMethod == 'e'],
                                           #priceDataUSD[flagObservationStatus == 'E' & flagMethod == 'e']),
                           validated = data.table())#,#priceData,
                           #valRev = priceproposed)
  
  
  data2imp <- reactiveValues(prepUSD =  data.table(),#priceDataUSD,
                           prepSLC = data.table(),#priceDataSLC,
                           prepLCU = data.table(),#priceDataLCU,
                        #   prepOut = data.table(),#rbind(priceDataLCU[flagObservationStatus == 'E' & flagMethod == 'e'],
                           #priceDataSLC[flagObservationStatus == 'E' & flagMethod == 'e'],
                           #priceDataUSD[flagObservationStatus == 'E' & flagMethod == 'e']),
                           validated = data.table(),#priceData,
                           valRev = data.table()) #priceproposed)
  
  #++  outlier validation tab ----
  source("tabs/outlier.R", local = TRUE) 
  
  #++  Bulk outlier validation tab ----
  source("tabs/bulkApprove.R", local = TRUE) 
  
  #++  last year imputation tab ----
  source("tabs/lastyearimp.R", local = TRUE) 
  
  #++  last year imputation tab ----
  source("tabs/bulkImpute.R", local = TRUE) 
  
  #++  interpolation tab ----
  source("tabs/interpolation.R", local = TRUE) 
  
  
  #++  series tab ----
  source("tabs/changeseries2.R", local = TRUE)
  
  #++  comparison tab ----
  source("tabs/comparisonPlots.R", local = TRUE)
  
  
   #++  PPI tab ----
  source("tabs/ppi_series.R", local = TRUE)
  
 # source("tabs/interpolation.R", local = TRUE)
  
  observeEvent(input$btn_update_data , {
    
    withProgress(message = 'Saving country data', value = 0.1, {
      
      maxYear <- as.numeric(format(Sys.time(), "%Y"))-4
      
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
                    keys = GetCodeList(domainPP, datasetPrep, 'timePointYears')[code %in% as.character(1991:maxYear), code])))
      
      Sys.sleep(0.1)
      incProgress(0.1)
      
      priceData <- GetData(
        priceKey,
        flags = TRUE)
      
      Sys.sleep(0.1)
      incProgress(0.6)
      
      priceData <- nameData(domainPP, datasetPrep, priceData)
      priceData[,timePointYears_description := NULL]
      saveRDS(priceData,'priceDataFull.rds')
      
      priceDataPast <<- priceData
      
      showModal(modalDialog(
        title = "Data saved!" ,
        sprintf("The local file has been updated with new data up to ")
        ))
      
      Sys.sleep(0.1)
      incProgress(1)
      
    })
    
  })
  
  
}