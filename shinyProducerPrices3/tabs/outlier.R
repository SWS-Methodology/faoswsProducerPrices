# priceData[flagObservationStatus == 'I' & flagMethod == 'e']) # priceproposed

# -- Insert token ---- 
output$btn_tokenOut <- renderUI({
  
  tokenTab <- ReadDatatable('pp_token', readOnly = FALSE)
  tokenTab <<- tokenTab
  dateOutlierToken <- tokenTab[dataset == 'Annual Producer Prices (Preparation)']$last_upd
  tokenOutlier <- tokenTab[dataset == 'Annual Producer Prices (Preparation)']$token
  tokenOutlier <<- tokenOutlier
  textInput(inputId = 'btn_tokenOut', 
            label = paste("'Preparation' dataset session token. Last update: ", dateOutlierToken, sep = ''), 
            value = NA)
})

observeEvent(input$btn_upd_token, {
 
  t1 <- ifelse(is.na(input$btn_tokenOut), tokenOutlier, input$btn_tokenOut)
  date <- as.character(Sys.Date())

  # validate( need((t1 != "" & !is.na(t1)), 'Please insert a vaild token.'))
    
  tokenTab[dataset == 'Annual Producer Prices (Preparation)' , token := c(t1)]
  tokenTab[dataset == 'Annual Producer Prices (Preparation)' , last_upd := date]
  
  changeset <- Changeset('pp_token')
  AddModifications(changeset, tokenTab)
  Finalise(changeset)
  
  tokenOutlier <<- t1
  
  showModal(modalDialog(
    title = "Token updated." ,
    sprintf("The chosen session will be used in the following tabs.")
  ))
  
})

# -- Country list button ----
output$btn_country_out <- renderUI({
  
  outlier_det <- ReadDatatable('outlier_detected')
  outlier_det <<- outlier_det
  country_out <- country_input[code %in% unique(outlier_det$geographicaream49)]
  country_out <<- country_out
  year_out <- unique(outlier_det$timepointyears)
  year_out <<- year_out
  startYear <- ifelse(outlier_det[,.N] == 0, as.character(as.numeric(format(Sys.Date(), '%Y')) - 10),
                      as.character(as.numeric(year_out) - 10))
  startYear <<- startYear
  
  selectInput(inputId = "btn_country_out",
              label = 'Country',
              choices = c('', country_out$label)#, 
              #  selected = 'Afghanistan - 4'
  )  
  
})

# -- End year button ----
observeEvent(input$btn_country_out,{
  
  validate(
    need(nrow(outlier_det) > 0, "No outlier detected for any country in the last questionnaire round. 
         Please run the plugin 'pp_OutlierDetection' or skip this step.")
  )
  
  if(input$btn_country_out != ""){
  outYear <<- year_out
  }
  
  
  
})

# -- Outlier list button ----
output$btn_outlier <- renderUI({

  req(input$btn_country_out) #, year_out)
  
  input$btn_country_out
  
  input$btn_not_out
  
  input$outlier_impute
  
  #input$btn_year_out
  
  sel_country <- country_out[country_out$label == input$btn_country_out, code] 
  
  lastyear <- year_out# as.character(input$btn_year_out) # max(as.numeric(dataPP$prepSLC$timePointYears))
  
  priceData <- dataPP$prepSLC

  if(lastyear != "" & priceData[,.N] > 0){
    
    dataPP$PreOut <- priceOut <- priceData[geographicAreaM49 == sel_country & flagObservationStatus == 'E' & 
                            flagMethod == 'e']
  
  validate(
    need(nrow(priceOut) > 0, "No outlier detected for this country. Please select another country.")
  )
  
  outlierlist <- priceOut[ , .(measuredItemCPC_description,
                                     measuredItemCPC)]

  outlier_input <-  sort(sprintf("%s - %s", outlierlist$measuredItemCPC_description,
                                 outlierlist$measuredItemCPC))


  selectInput(inputId = "btn_outlier",
              label = 'Outlier',
              choices = c('', outlier_input))#,
  #selected = 'Sri Lanka - 144')
  }
  
})

output$save_out_sws <- renderUI({
  actionBttn('save_out_sws', 
             label = 'Save in SWS',
             style = 'gradient', 
             color = 'default')
})


#-- Load Preparation data ----
# Once year and country are selected the 'annual_producer_prices_prep' dataset is loaded and stored

observeEvent(input$btn_country_out, {
  
  if(input$btn_country_out != ''){
  withProgress(message = 'Loading country data', value = 0.05, {
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
  }
  
})


#-- Outlier data ----

# If the value has been detected as outlier a tabel with country and commodity data is shown
output$out_tab_comp <- DT::renderDataTable(server = FALSE, {
  #req(input$btn_outlier) #, input$btn_year, input$btn_start_year)
  
  req(input$btn_outlier, input$btn_country_out)#, input$btn_year_out)
  
  #ppPrep0 <- copy(dataPP$prepUSD)
  ppPrep0 <- rbind(copy(dataPP$prepUSD), copy(dataPP$prepSLC), copy(dataPP$prepLCU))

  country <- gsub('.*- ','', input$btn_country_out)
  product <- gsub('.*- ','', input$btn_outlier)
  #product <- substr(product,1,nchar(product)-7)
  
  ppPrep <- ppPrep0[geographicAreaM49 == country | 
                      measuredItemCPC == product]
  
  geohierneeded <- geohier[, c("name_en_l2", "name_en_l3", 
                               "code_l5"), with = F]
  ppPrep <- merge(ppPrep, geohierneeded, by.x = 'geographicAreaM49',
                  by.y = 'code_l5', all.x = T)
  
  ppPrep <- ppPrep[order(timePointYears)]
  ppPrep[ , Variation := round(c(NA,diff(Value))/shift(Value), 3), 
          by = c('geographicAreaM49', 'measuredItemCPC', 'measuredElement')]
  
  setnames(ppPrep, c('geographicAreaM49_description', 'measuredItemCPC_description', 
                     'timePointYears', "flagObservationStatus",
                     "flagMethod", "name_en_l2", "name_en_l3", 'measuredElement'),
           c('Country', 'CPC', 'Year', 'Status', 'Method', 'Region', 'Sub-Region', 'Type'))
  
  ppPrep[Type == '5530', Type := 'LCU']
  ppPrep[Type == '5531', Type := 'SLC']
  ppPrep[Type == '5532', Type := 'USD']
  ppPrep <- ppPrep[order(-Year)]
  DT::datatable(ppPrep[Year %in% startYear:outYear,.(Region, `Sub-Region`, Country, CPC, Year, Type, 
                          Value, Status, Method, Variation)], filter = 'top',
                rownames = FALSE)
})

output$gg_plot_out_comp <- renderPlotly({
  req(input$btn_outlier,  input$btn_country_out)#, input$btn_start_year) # input$btn_year_out,
 if(input$btn_outlier != '' &  input$btn_country_out != '') {
  
  if(input$slcORusd == 1){
    ppPrep <- copy(dataPP$prepSLC)
    chosenEl <- currElement[1]
    chosencurrEl <- 'SLC'
  } else {
    ppPrep <- copy(dataPP$prepUSD)
    chosenEl <- currElement[2]
    chosencurrEl <- 'USD' 
  }
  
  country <- gsub('.*- ','', input$btn_country_out)
  product <- gsub('.*- ','', input$btn_outlier)
  #product <- substr(product,1,nchar(product)-7)

  year <- year_out#year <- gsub('.*[(]','', input$btn_outlier)
  #year <- gsub('[)]','', year)
  
  ppPrep <- ppPrep[geographicAreaM49 == country  &
                     measuredItemCPC == product ]
  ppPrep[, Outlier := FALSE]
  ppPrep[flagObservationStatus == 'E' & flagMethod == 'e', Outlier := TRUE]
  ppPrep[ , Flag := paste(flagObservationStatus, flagMethod, sep = ';')]
  
  # -- Get metadada ----
  priceMDkey = DatasetKey(
    domain = domainPP,
    dataset = datasetVal,
    dimensions = list(
      Dimension(name = "geographicAreaM49",
                keys = GetCodeList(domainPP, datasetVal, 'geographicAreaM49')[code == unique(ppPrep$geographicAreaM49) , code]),
      Dimension(name = "measuredElement", 
                keys = GetCodeList(domainPP, datasetVal, 'measuredElement')[code == '5530', code]),
      Dimension(name = "measuredItemCPC",
                keys = GetCodeList(domainPP, datasetVal, 'measuredItemCPC')[code == unique(ppPrep$measuredItemCPC), code]), 
      Dimension(name = "timePointYears", 
                keys = GetCodeList(domainPP, datasetPrep, 'timePointYears')[code %in% startYear:outYear, code])))
  #undebug(GetMetadata)

  priceMD <- tryCatch( GetMetadata(priceMDkey), error = function(priceMDkey) NA)
  if(length(priceMD) == 1){
    if(is.na(priceMD)){
      ppPrep[,Metadata := NA]}
  
    } else {
    priceMD <- priceMD[Metadata_Element == 'COMMENT']
    names(priceMD)
  
    priceMD <- priceMD[,c("geographicAreaM49", #"measuredElement",
                          "measuredItemCPC", "timePointYears", "Metadata_Value"), with = F]
    setnames(priceMD, "Metadata_Value", "Metadata")
    
    ppPrep <- merge(ppPrep, priceMD[,c("geographicAreaM49",# "measuredElement",
                                       "measuredItemCPC", "timePointYears", "Metadata"), with = F], 
                    by = c("geographicAreaM49", #"measuredElement",
                           "measuredItemCPC",
                           "timePointYears"), all.x = T)
  }
  
  miny <- min(ppPrep$Value) - 0.1*min(ppPrep$Value)
  maxy <- max(ppPrep$Value) + 0.1*max(ppPrep$Value)
  ppPrep$Value <- round(ppPrep$Value, 2)
#  ppPrep$timePointYears <- base::as.Date(as.integer(ppPrep$timePointYears), format = '%Y')
  ggplotly(ggplot(ppPrep[timePointYears %in% startYear:outYear], aes(x = timePointYears, 
                              y = Value, label= Flag, label2= Metadata))+
             geom_line(aes(x = timePointYears, 
                           y = Value), group = 1, col = "grey31") +
             geom_point(aes(col = Outlier),size = 2)+
             scale_colour_manual(values = c("FALSE" = "grey31", "TRUE" = "coral"))+
             ylim(miny, maxy)+
             theme(legend.position="none") +
            # scale_x_date(date_labels = "%Y")+
             ggtitle(paste('Series in ', chosencurrEl,':', country, ' - ',product)))
  
  
  #  ggplotly(aa)
  
}
  
  })

# Show series with variation 
output$out_tab_slc <- DT::renderDataTable(server = FALSE, {
  req(input$btn_outlier) #, input$btn_year, input$btn_start_year)
  
  ppPrep <- copy(dataPP$prepSLC)
  
  country <- gsub('.*- ','', input$btn_country_out)
  product <- gsub('.*- ','', input$btn_outlier)
 # product <- substr(product,1,nchar(product)-7)
 # year <- gsub('.*[(]','', input$btn_outlier)
 # year <- gsub('[)]','', year)
  
  ppPrep <- ppPrep[geographicAreaM49 == country  &
                     measuredItemCPC == product ]
  ppPrep[, Outlier := FALSE]
  ppPrep[flagObservationStatus == 'E' & flagMethod == 'e', Outlier := TRUE]
  
  setnames(ppPrep, c('timePointYears', "flagObservationStatus",
                     "flagMethod"),
           c('Year', 'Status', 'Method'))
  ppPrep <- ppPrep[order(Year)]
  ppPrep[ , Variation := round(c(NA,diff(Value))/shift(Value), 3), 
          by = c('geographicAreaM49', 'measuredItemCPC')]
  ppPrep[,Flag := paste(Status, Method, sep = ';')]
  ppPrep <- ppPrep[order(-Year)]
  
  col2hide <- which(names(ppPrep)=='Outlier')
  
  DT::datatable(ppPrep[Year %in% startYear:outYear,.(Year, Value, Flag, Variation)], filter = 'top',
                rownames = FALSE) #%>% 
  #formatStyle("Outlier", target = 'row', color=styleEqual(TRUE, "red"))
})

#--- Impute outlier ----
output$btn_outlier_manual <- renderUI({
  req(input$btn_is_out)
  
  input$btn_country_out
  
  input$btn_outlier
  
  input$outlier_impute
  
  # input$btn_not_out
  # Button for manual ratio input
  numericInput(inputId = 'outlier_manual', label = 'Manual input', value = NA)
  
})

#--- Is outlier ----
# Save data to SWS when the work is finished (or whenever useful to not lose the work)
observeEvent(input$outlier_impute, {
  
  manualvalue <- input$outlier_manual
  
  ppPrep <- rbind(copy(dataPP$prepUSD), copy(dataPP$prepSLC), copy(dataPP$prepLCU))
  
  country <- gsub('.*- ','', input$btn_country_out)
  product <- gsub('.*- ','', input$btn_outlier)
  #product <- substr(product,1,nchar(product)-7)
  year <- year_out#input$btn_year_out#gsub('.*[(]','', input$btn_outlier)
  #year <- gsub('[)]','', year)
  
  if(!is.na(manualvalue)){
    
    # Manual value inserted for SLC only to be converted
    ppPrep[geographicAreaM49 == country  &
             measuredItemCPC == product &
            # timePointYears == year &
             measuredElement == '5531' &
             flagObservationStatus == 'E' & flagMethod == 'e', c('Value',
                                                                 'flagObservationStatus',
                                                                 'flagMethod') := list(manualvalue, 'E', 'f')]
    ##########################################
    #take only data of interest (ARE THERE OTHER?)
    data2change <- ppPrep[geographicAreaM49 == country  &
                            measuredItemCPC == product &
                            timePointYears == year &
                            measuredElement == '5531' &
                           flagObservationStatus == 'E' & flagMethod == 'f',
                        .(geographicAreaM49,measuredElement,measuredItemCPC,
                           timePointYears, Value,
                           flagObservationStatus, flagMethod)]
    #setnames(data2change, 'Value', 'ValueSLC')

    if(any(data2change$geographicAreaM49 == '275')){
      
      erKey = DatasetKey(
        domain = 'common',
        dataset = 'exchange_rates_annual',
        dimensions = list(
          Dimension(name = 'geographicAreaM49',
                    keys = GetCodeList('common', 'exchange_rates_annual', 'geographicAreaM49')[code %in% c(unique(data2change$geographicAreaM49), '376') , code]),
          Dimension(name = "from_currency",
                    keys = GetCodeList('common', 'exchange_rates_annual', 'from_currency')[code != 'ECU' & endDate >= '1991-01-01' | 
                                                                                             is.na(endDate), code]),
          Dimension(name = "to_currency", 
                    keys = GetCodeList('common', 'exchange_rates_annual', 'to_currency')[code == 'USD', code]),
          Dimension(name = 'measuredElement',
                    keys = c('LCU')),
          Dimension(name = "timePointYears", 
                    keys = GetCodeList('common', 'exchange_rates_annual', 'timePointYears')[code %in% unique(data2change$timePointYears), code]))
        
      )
      
      erdt <- GetData(erKey, flags = F) 
      
      if(erdt[geographicAreaM49 == '275',.N] == 0){
        # Palestine copy Israel
        palestine <- erdt[geographicAreaM49 == '376']
        palestine[,geographicAreaM49 := '275']
        erdt <- rbind(erdt, palestine)
        
      } else {
        
        erdt <- erdt[geographicAreaM49 != '275']
        palestine <- erdt[geographicAreaM49 == '376']
        palestine[,geographicAreaM49 := '275']
        erdt <- rbind(erdt, palestine)
      }
      
      
    
      } else {
      
      erKey = DatasetKey(
        domain = 'common',
        dataset = 'exchange_rates_annual',
        dimensions = list(
          Dimension(name = 'geographicAreaM49',
                    keys = GetCodeList('common', 'exchange_rates_annual', 'geographicAreaM49')[code %in% unique(data2change$geographicAreaM49), code]),
          Dimension(name = "from_currency",
                    keys = GetCodeList('common', 'exchange_rates_annual', 'from_currency')[code != 'ECU' & endDate >= '1991-01-01' | 
                                                                                             is.na(endDate), code]),
          Dimension(name = "to_currency", 
                    keys = GetCodeList('common', 'exchange_rates_annual', 'to_currency')[code == 'USD', code]),
          Dimension(name = 'measuredElement',
                    keys = c('LCU')),
          Dimension(name = "timePointYears", 
                    keys = GetCodeList('common', 'exchange_rates_annual', 'timePointYears')[code %in% unique(data2change$timePointYears), code]))
        
      )
      
      erdt <- GetData(erKey, flags = F) 
      
    }
    
    erdt[,c('to_currency', 'measuredElement')] <- NULL
    
    message('pp_Conversion: Starting computations')
    
    priceConverted <- convert_currency(priceData = data2change, erdt = erdt, sessionElement = 'SLC')
    
  
    # Start conversion into USD and SLC merging with XR
    
    pper3 <- priceConverted[ !is.na(Value)]
    
    ppPrep[geographicAreaM49 == country  &
             measuredItemCPC == product &
             timePointYears == year &
             flagObservationStatus == 'E' & flagMethod == 'e', c('Value',
                                                                 'flagObservationStatus',
                                                                 'flagMethod') := list(manualvalue, 'E', 'f')]
    
    listLCU <- list(pper3[measuredElement == '5530']$Value,
                    pper3[measuredElement == '5530']$flagObservationStatus,
                    pper3[measuredElement == '5530']$flagMethod)
    
    listSLC <- list(pper3[measuredElement == '5531']$Value,
                    pper3[measuredElement == '5531']$flagObservationStatus,
                    pper3[measuredElement == '5531']$flagMethod)
    
    listUSD <- list(pper3[measuredElement == '5532']$Value,
                    pper3[measuredElement == '5532']$flagObservationStatus,
                    pper3[measuredElement == '5532']$flagMethod)
    
    ########################################
  } else {
    
    # Flag to be changed
  
    listLCU <- list(0,'M', 'u')
    listSLC <- list(0,'M', 'u')
    listUSD <- list(0,'M', 'u')
    
  }
  
  dataPP$prepLCU[geographicAreaM49 == country  &
                   measuredItemCPC == product & 
                   timePointYears == year &
                   flagObservationStatus == 'E' & flagMethod == 'e', 
                 c('Value', 'flagObservationStatus', 'flagMethod') := listLCU]
  
  dataPP$prepSLC[geographicAreaM49 == country  &
                   measuredItemCPC == product & 
                   timePointYears == year &
                   flagObservationStatus == 'E' & flagMethod == 'e', 
                 c('Value', 'flagObservationStatus', 'flagMethod') := listSLC]
  
  dataPP$prepUSD[geographicAreaM49 == country  &
                   measuredItemCPC == product & 
                   timePointYears == year &
                   flagObservationStatus == 'E' & flagMethod == 'e', 
                 c('Value', 'flagObservationStatus', 'flagMethod') := listUSD]
  
})

#--- Is NOT outlier ----
# If the value is not recognized as outlier the value is kept and the flag is set again to questionnaire data 
observeEvent(input$btn_not_out ,{
  
  ppPrep <- rbind(copy(dataPP$prepUSD), copy(dataPP$prepSLC), copy(dataPP$prepLCU))

  country <- gsub('.*- ','', input$btn_country_out)
  product <- gsub('.*- ','', input$btn_outlier)
  #product <- substr(product,1,nchar(product)-7)
  year <- year_out#input$btn_year_out
  #year <- gsub('.*[(]','', input$btn_outlier)
  #year <- gsub('[)]','', year)
  
  ppPrep[geographicAreaM49 == country  &
           measuredItemCPC == product & 
           timePointYears == year &
           measuredElement == '5530' &
           flagObservationStatus == 'E' & flagMethod == 'e', c('flagObservationStatus',
                                                               'flagMethod') := list('', 'q')]
  
  ppPrep[geographicAreaM49 == country  &
           measuredItemCPC == product &
           timePointYears == year &
           flagObservationStatus == 'E' & flagMethod == 'e' &
           measuredElement %in% c('5531', '5532'), c('flagObservationStatus',
                                                     'flagMethod') := list('', 'q')]
  
  dataPP$prepLCU <- ppPrep[measuredElement == '5530']
  dataPP$prepSLC <- ppPrep[measuredElement == '5531']
  dataPP$prepUSD <- ppPrep[measuredElement == '5532']
  
})

#-- Save data to SWS when the work is finished (or whenever useful to not lose the work) ----
observeEvent(input$save_out_sws, {

  ppPrep <- rbind(copy(dataPP$prepUSD), copy(dataPP$prepSLC), copy(dataPP$prepLCU))
  
  sel_country <- country_out[country_out$label == input$btn_country_out, code]
  
  #outliers <- dataPP$prepOut

  #pp2save <- merge(outliers, ppPrep, by = names(outliers)[!names(outliers) %in% c('Value',
  #                                                                      'flagObservationStatus',
  #                                                                      'flagMethod')],
  #      suffixes = c('_out', ''))
  
  pp2save <- ppPrep[timePointYears == year_out & geographicAreaM49 == sel_country]

  #pp2save <- pp2save[flagObservationStatus != flagObservationStatus_out | flagMethod != flagMethod_out]
  
  #pp2save[ , names(pp2save)[grepl('_out', names(pp2save))]] <- NULL
  pp2save[ , names(pp2save)[grepl('_description', names(pp2save))]] <- NULL
  
  showModal(modalDialog(
    title = "Saving data..." ,
    sprintf("Please wait until confirmaiton message is displayed.")
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
