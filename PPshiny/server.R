function(input, output) {
  
  
  #-- Data stored ----
  #Data to store: from questionnaire, preparation, validated and all (with data 0,M,u)
  
  dataPP <- reactiveValues(prepUSD = priceDataUSD,
                           prepSLC = priceDataSLC,
                           prepLCU = priceDataLCU,
                           prepOut = priceDataSLC[flagObservationStatus == 'E' & flagMethod == 'i'],
                           validated = priceData,
                           valRev = priceproposed)
  
  
  #++  outlier validation tab ----
  source("tabs/outlier.R", local = TRUE) 
  
  #++  last year imputation tab ----
  source("tabs/lastyearimp.R", local = TRUE) 
  
  #++  series tab ----
  source("tabs/changeseries.R", local = TRUE)
  
  
}