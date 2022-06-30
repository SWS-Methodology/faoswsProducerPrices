
observeEvent(input$btn_bulk_country, {
  
  ppPrep <- rbind(copy(dataPP$prepUSD), copy(dataPP$prepSLC), copy(dataPP$prepLCU))

  if(any(input$btn_country_accept == 'All')){
    
    sel_country <- country_input$code
    
  } else {
  
  sel_country <- country_input[country_input$label %in% input$btn_country_accept, code]
}

    withProgress(message = 'Loading country data', value = 0.05, {
      # Preparation
      priceKey = DatasetKey(
        domain = domainPP,
        dataset = datasetPrep,
        dimensions = list(
          Dimension(name = "geographicAreaM49",
                    keys = GetCodeList(domainPP, datasetPrep, 'geographicAreaM49')[code %in% sel_country, code]),
          Dimension(name = "measuredElement",
                    keys = GetCodeList(domainPP, datasetPrep, 'measuredElement')[code %in% currElement, code]),
          Dimension(name = "measuredItemCPC",
                    keys = GetCodeList(domainPP, datasetPrep, 'measuredItemCPC')[, code]),
          Dimension(name = "timePointYears",
                    keys = GetCodeList(domainPP, datasetPrep, 'timePointYears')[code > maxYear, code])))
      
      Sys.sleep(0.1)
      incProgress(0.1)
      
      priceData <- GetData(
        priceKey,
        flags = TRUE)
      
      Sys.sleep(0.1)
      incProgress(0.6)
      
      priceData <- nameData(domainPP, datasetPrep, priceData)
      priceData[,timePointYears_description := NULL]
      
      priceData <- rbind(priceDataPast[timePointYears > 1999], priceData)
      
      priceDataLCU <- priceData[measuredElement == '5530' ]
      priceDataSLC <- priceData[measuredElement == '5531' ]
      priceDataUSD <- priceData[measuredElement == '5532' ]
      
      Sys.sleep(0.1)
      incProgress(0.8)
      
      dataPP$prepUSD <- priceData[measuredElement == '5532' ]
      dataPP$prepSLC <- priceData[measuredElement == '5531' ]
      dataPP$prepLCU <- priceData[measuredElement == '5530' ]
      
      Sys.sleep(0.1)
      incProgress(1)
      
    })
    
    message('Dataset loaded')

    priceData[geographicAreaM49 %in% sel_country  &
            flagObservationStatus == 'E' & flagMethod == 'e' &
            measuredElement == '5530', c('flagObservationStatus',
                                         'flagMethod') := list('', 'q')]
  
    priceData[geographicAreaM49 %in% sel_country  &
           flagObservationStatus == 'E' & flagMethod == 'e' &
           measuredElement %in% c('5531', '5532'), c('flagObservationStatus',
                                                     'flagMethod') := list('', 'q')]
  
  dataPP$prepLCU <- priceData[measuredElement == '5530']
  dataPP$prepSLC <- priceData[measuredElement == '5531']
  dataPP$prepUSD <- priceData[measuredElement == '5532']
  
  ###########
  
  
  ppPrep <- rbind(copy(dataPP$prepUSD), copy(dataPP$prepSLC), copy(dataPP$prepLCU))
  
  #outliers <- dataPP$prepOut
  
  #pp2save <- merge(outliers, ppPrep, by = names(outliers)[!names(outliers) %in% c('Value',
  #                                                                      'flagObservationStatus',
  #                                                                      'flagMethod')],
  #      suffixes = c('_out', ''))
  
  pp2save <- ppPrep[timePointYears == year_out & geographicAreaM49 %in% sel_country]
  
  #pp2save <- pp2save[flagObservationStatus != flagObservationStatus_out | flagMethod != flagMethod_out]
  
  #pp2save[ , names(pp2save)[grepl('_out', names(pp2save))]] <- NULL
  pp2save[ , names(pp2save)[grepl('_description', names(pp2save))]] <- NULL
  
  showModal(modalDialog(
    title = "Data reverted!" ,
    sprintf("Data for the selected country(ies) have been classified in bulk as non-outliers. 
            Now data are being saved in SWS. Please wait until confirmaiton message is displayed.")
  ))
  
  
  if(localrun){
    if(CheckDebug()){
      library(faoswsModules)
      SETTINGS = ReadSettings("sws.yml")
      R_SWS_SHARE_PATH = SETTINGS[["share"]]
      SetClientFiles(SETTINGS[["certdir"]])
      GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                         token = tokenOutlier)
    }
  } else {
    R_SWS_SHARE_PATH = "Z:"
    SetClientFiles("/srv/shiny-server/.R/PROD/")
    GetTestEnvironment(baseUrl = "https://sws.aws.fao.org:8181",
                       token = tokenOutlier)
  }
  
  SaveData(domain = domainPP,
           dataset = datasetPrep,
           data = pp2save,
           waitTimeout = Inf)
  
  outlier_det <- ReadDatatable('outlier_detected', readOnly = F)
  # Outlier removed
  mod <- merge(outlier_det, pp2save, 
               by.x = c('geographicaream49', 'measureditemcpc', 'timepointyears'),
               by.y = c('geographicAreaM49', 'measuredItemCPC', 'timePointYears'),
               all.x = T)
  
  toDelete <- outlier_det[!mod[flagObservationStatus == 'E' & flagMethod == 'e' | !geographicaream49 %in% unique(pp2save$geographicAreaM49), .(geographicaream49, measureditemcpc, timepointyears), ],
                          on = c('geographicaream49', 'measureditemcpc', 'timepointyears')]
  
  chng <- Changeset('outlier_detected')
  AddDeletions(chng, toDelete)
  Finalise(chng)

  
  showModal(modalDialog(
    title = "Data saved!" ,
    sprintf("Please check and save the SWS session data to the dataset.")
  ))
  
  
  
  
})

