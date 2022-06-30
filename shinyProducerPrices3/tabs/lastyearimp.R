# -- ARIMA results ----

# -- list to review button ----
output$btn_validate_country <- renderUI({
  
  #input$btn_accept
  #input$btn_refuse
  #input$btn_impute
  
  #reviewlist <- unique(data2imp$valRev[, .(geographicaream49#,
                                         #measureditemcpc
  #                                       )])

  # reviewlist <- merge(reviewlist, M49, 
  #                     by.x = 'geographicaream49', by.y = 'code', all.x = T)
  # 
  # reviewlist <- merge(reviewlist, cpc, 
  #                     by.x = 'measureditemcpc', by.y = 'code', 
  #                     all.x = T, suff = c('_geo', '_cpc'))
  
  priceproposed <- ReadDatatable('imputation_annual_prices', readOnly = F)
  # priceproposed <- priceproposed[timepointyears == max(timepointyears)]
  # priceproposed <- priceproposed[approach != 'Kalman']
  
  # Do not include already imputed values
  imputed <- priceproposed[selected == TRUE]
  priceproposed <- priceproposed[!imputed, on = c("geographicaream49",
                                                  "measureditemcpc",
                                                  "timepointyears" )]
  priceproposed <<- priceproposed
  data2imp$valRev <- priceproposed
  countries_imp <- unique(priceproposed$geographicaream49)
  
  
  review_input <-  country_input[code %in% countries_imp]$label #sort(sprintf("%s - %s", reviewlist$description_geo,
                                #reviewlist$description_cpc))   
  
  selectInput(inputId = "btn_validate_country",
              label = 'Country to review',
              choices = c('', review_input))#, 
  #selected = 'Sri Lanka - 144')
  
})

output$btn_validate_product <- renderUI({
  
  req(input$btn_validate_country)
  
  input$btn_accept
  #input$btn_refuse
  input$btn_impute
  
  sel_country <- country_input[country_input$label == input$btn_validate_country, code] 

  # reviewlist <- unique(data2imp$valRev[, .(geographicaream49,
  #                                         measureditemcpc)])
  reviewlist <- unique(data2imp$valRev[geographicaream49 == sel_country, 
                                           ]$measureditemcpc)
  # reviewlist <- merge(reviewlist, M49, 
  #                     by.x = 'geographicaream49', by.y = 'code', all.x = T)
  
  reviewlist <- cpc[code %in% reviewlist, .(code, description)] #merge(reviewlist, cpc, 
                      # by.x = 'measureditemcpc', by.y = 'code', 
                      # all.x = T, suff = c('_geo', '_cpc'))
  
  review_input <-  sort(sprintf("%s - %s", #reviewlist$description_geo,
                                 reviewlist$description,
                                reviewlist$code))   
  

  selectInput(inputId = "btn_validate_product",
              label = 'Product to review',
              choices = c('', review_input))#, 
  #selected = 'Sri Lanka - 144')
  
})

observeEvent(input$btn_validate_country,{
  
  if(input$btn_validate_country != ''){
    withProgress(message = 'Loading country data', value = 0.1, {
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
      
      data2imp$prepUSD <- priceData[measuredElement == '5532' ]
      data2imp$prepSLC <- priceData[measuredElement == '5531' ]
      data2imp$prepLCU <- priceData[measuredElement == '5530' ]
      data2imp$validated <- priceData
      
      
      Sys.sleep(0.1)
      incProgress(1)
      
    })
    
    message('Dataset loaded')
  }
  
  
})

output$save_est_sws <- renderUI({
  actionBttn('save_est_sws', 
             label = 'Save in SWS',
             style = 'gradient', 
             color = 'default')
})

# If the value has been detected as outlier a tabel with country and commodity data is shown
output$est_tab <- DT::renderDataTable(server = FALSE, {
  req(input$btn_validate_country, input$btn_validate_product) #, input$btn_year, input$btn_start_year)

  ppVal0 <- copy(data2imp$validated)
  
  country <- gsub(' -.*','', input$btn_validate_country)
  product <- gsub('.*- ','', input$btn_validate_product)
  #prodcode <- cpc[description == product & code %in% ppVal0$measuredItemCPC]$code
  ppVal <- ppVal0[geographicAreaM49_description == country |
                    measuredItemCPC == product]
  
  geohierneeded <- geohier[, c("name_en_l2", "name_en_l3", 
                               "code_l5"), with = F]
  ppVal <- merge(ppVal, geohierneeded, by.x = 'geographicAreaM49',
                 by.y = 'code_l5', all.x = T)
  
  setnames(ppVal, c('geographicAreaM49_description', 'measuredItemCPC_description', 
                    'timePointYears', "flagObservationStatus",
                    "flagMethod", "name_en_l2", "name_en_l3", 'measuredElement'),
           c('Country', 'CPC', 'Year', 'Status', 'Method', 'Region', 'Sub-Region', 'Type'))
  
  ppVal[Type == '5530', Type := 'LCU']
  ppVal[Type == '5531', Type := 'SLC']
  ppVal[Type == '5532', Type := 'USD']
  
  DT::datatable(ppVal[,.(Region, `Sub-Region`, Country, CPC, Year, Type, 
                         Value, Status, Method)], filter = 'top',
                rownames = FALSE)
})

output$gg_plot_est <- renderPlotly({
  req(input$btn_validate_country, input$btn_validate_product) #, input$btn_year, input$btn_start_year)

  ppVal <- copy(data2imp$validated)
  ppVal <- ppVal[measuredElement == '5531']
  pp2Rev <- copy(data2imp$valRev)
  
  country <- gsub(' -.*','', input$btn_validate_country)
  product <- gsub('.*- ','', input$btn_validate_product)
 # prodcode <- cpc[description == product & code %in% ppVal$measuredItemCPC]$code
  ppVal <- ppVal[geographicAreaM49_description == country  &
                   measuredItemCPC == product ]
  
  pp2Rev <- pp2Rev[geographicaream49 == M49[description == country]$code &
                     measureditemcpc %in%  product ]
  
  pricevalKey = DatasetKey(
    domain = domainPP,
    dataset = datasetVal,
    dimensions = list(
      Dimension(name = "geographicAreaM49",
                keys = GetCodeList(domainPP, datasetPrep, 'geographicAreaM49')[code == 
                                                                                unique(ppVal$geographicAreaM49), 
                                                                              code]),
      Dimension(name = "measuredElement", 
                keys = GetCodeList(domainPP, datasetPrep, 'measuredElement')[code %in% '5530', code]),
      Dimension(name = "measuredItemCPC",
                keys = GetCodeList(domainPP, datasetPrep, 'measuredItemCPC')[code == product, code]),
      Dimension(name = "timePointYears", 
                keys = GetCodeList(domainPP, datasetPrep, 'timePointYears')[code >= min(ppVal$timePointYears), code])))
  
  priceVal <- data2imp$prepSLC[geographicAreaM49 == unique(ppVal$geographicAreaM49) &
                                 measuredItemCPC == product &
                                 timePointYears >= min(ppVal$timePointYears)] # GetData(
    #pricevalKey,
    #flags = TRUE)
  
  priceValMD <- tryCatch( GetMetadata(pricevalKey), error = function(priceMDkey) NA)
  
  if(length(priceValMD) == 1){
    if(is.na(priceValMD)){
      series2est$metadata <- priceValMD}
    
  } else {
    priceValMD <- priceValMD[Metadata_Element == 'COMMENT']
    priceValMD <- priceValMD[,c("geographicAreaM49",# "measuredElement",
                                "measuredItemCPC", "timePointYears", "Metadata_Value"), with = F]
    setnames(priceValMD, "Metadata_Value", "Metadata")
    
    series2est$metadata <- priceValMD
  }
  
  ppVal[,Approach := 'SWS - Metadata']
  ppVal[,Flag := paste(flagObservationStatus, flagMethod, sep = ';')]
  setnames(pp2Rev, c('estimation', 'approach', 'timepointyears'), c('Value', 'Approach', 'Year'))
  setnames(ppVal, c('timePointYears'), c('Year'))
  #names(ppVal) <- tolower(names(ppVal))
  pp2plot <- rbind(ppVal[,.(Year, Value, 
                            Approach, Flag)], pp2Rev[,.(Year, Value, Approach)], fill = T)
  pp2plot <- merge(pp2plot, priceValMD[,.(timePointYears,
                                          Metadata)], by.x = 'Year',by.y = 'timePointYears', all.x = T)
  
  pp2plot$Year <- as.numeric(pp2plot$Year)

   miny <- min(pp2plot$Value) - 0.1*(min(pp2plot$Value))
   maxy <- max(pp2plot$Value) + 0.1*(max(pp2plot$Value))
  
   pp2plot <- pp2plot[order(Year)]
   
   # group.colors <- c(`SWS - Metadata` = 'grey31',
   #                   ARIMAX = '#F8766D',
   #                   Ensemble = '#39B600',
   #                   CPI = '#00ABFD',
   #                   LM = '#DC71FA',
   #                   Comm_Group = '#BB9D00',
   #                   PriceRatio = '#00C1AA',
   #                   GDP = 'blue')
   # 
   pp2plot$Year <- as.character(pp2plot$Year)
   pp2plot$Year <- format(as.Date(pp2plot$Year, format = "%Y"),"%Y")
   pp2plot$Value <- round(pp2plot$Value, 2)
   
   if(length(unique(pp2plot[Approach != 'SWS - Metadata']$Year)) == 1 ){
  ggplotly(ggplot()+
     geom_line(data = pp2plot[!is.na(Flag)], group = 1, col = 'grey31', 
               mapping = aes(x = Year, y = Value, label= Flag, label2 = Metadata)) +
     geom_point(data = pp2plot[is.na(Flag)], aes(x = Year, y = Value, col = Approach),size = 2)+
    geom_point(data = pp2plot[!is.na(Flag)], col = 'grey31', 
               mapping = aes(x = Year, y = Value, label= Flag, label2 = Metadata)) +
     scale_color_brewer(palette="Set1") +
     #scale_color_manual(values = c(`SWS - Metadata` = 'grey31')) +
     ylim(miny, maxy)+
    # theme(legend.position="none") +
     ggtitle(paste('Series:', country, ' - ',product)) #, 
    # Used to impute by clicking on the graph, not fully implemented
    # source = "imputation"
    )
   } else {
     
     previousYear <- min(as.numeric(unique(pp2plot[is.na(Flag)]$Year))) - 1
     
     n <- length(unique(pp2plot[Approach != 'SWS - Metadata']$Approach))
     repPreviousY <- do.call("rbind", replicate(n, pp2plot[Year == previousYear], simplify = FALSE))
     
    repPreviousY[, Approach := unique(pp2plot[Approach != 'SWS - Metadata']$Approach)]
    
   line2plot <- rbind(repPreviousY, pp2plot[is.na(Flag)]) 
   pp2plot$Value <- round(pp2plot$Value, 2)
   
     ggplotly(ggplot()+
                geom_line(data = pp2plot[!is.na(Flag)], group = 1, col = 'grey31', 
                          mapping = aes(x = Year, y = Value, label= Flag, label2 = Metadata)) +
                geom_line(data = line2plot,
                          group = 1, mapping = aes(x = Year, 
                                                   y = Value,
                                                   col = Approach)) +
                geom_point(data = pp2plot[is.na(Flag)], aes(x = Year, y = Value, col = Approach),size = 2)+
                geom_point(data = pp2plot[!is.na(Flag)], col = 'grey31', 
                           mapping = aes(x = Year, y = Value, label= Flag, label2 = Metadata)) +
                scale_color_brewer(palette="Set1") +
                #scale_color_manual(values = c(`SWS - Metadata` = 'grey31')) +
                ylim(miny, maxy)+
                # theme(legend.position="none") +
                ggtitle(paste('Series:', country, ' - ',product)) #, 
              # Used to impute by clicking on the graph, not fully implemented
              # source = "imputation"
     )
     
   }
  
  
})

# Used to impute by clicking on the graph, not fully implemented
# output$tbl <- renderDataTable({
#   event.data <- event_data("plotly_click", source = "imputation")
#   
#   if(is.null(event.data) == T) return(NULL)
#   print(event.data[ ,c(3:4)])
# })

output$est_tab_slc <- DT::renderDataTable(server = FALSE, {
  req(input$btn_validate_country, input$btn_validate_product) #, input$btn_year, input$btn_start_year)
  
  ppVal <- copy(data2imp$validated)
  ppVal <- ppVal[measuredElement == '5531']
  pp2Rev <- copy(data2imp$valRev)
  
  country <- gsub(' -.*','', input$btn_validate_country)
  product <- gsub('.*- ','', input$btn_validate_product)
 # prodcode <- cpc[description == product & code %in% ppVal$measuredItemCPC]$code
  
  ppVal <- ppVal[geographicAreaM49_description == country  &
                   measuredItemCPC == product ]
  
  setnames(ppVal, c('timePointYears', "flagObservationStatus",
                    "flagMethod"),
           c('Year', 'Status', 'Method'))
  ppVal <- ppVal[order(Year)]
  ppVal[ , Variation := round(c(NA,diff(Value))/shift(Value), 3), 
        by = c('geographicAreaM49', 'measuredItemCPC')]
  ppVal[,Flag := paste(Status, Method, sep = ';')]
  ppVal <- ppVal[order(-Year)]
  DT::datatable(ppVal[,.(Year, Value, Flag, Variation)], filter = 'top',
                rownames = FALSE)
})


output$btn_summary_approach <- renderUI({
  req(input$btn_validate_country, input$btn_validate_product)
  
  pp2Rev <- copy(data2imp$valRev)
  
  country <- gsub(' -.*','', input$btn_validate_country)
  product <- gsub('.*- ','', input$btn_validate_product)
  #prodcode <- cpc[description == product & code %in% pp2Rev$measureditemcpc]$code
  
  pp2Rev <- pp2Rev[geographicaream49 == M49[description == country]$code &
                     measureditemcpc ==  product ]
  
  choices_names <- unique(pp2Rev$approach) #sprintf("%s - %s", pp2Rev$approach, pp2Rev$estimation)
  # Have something similar to: paste(c('Model-based:', 'Exports:', 'Primary Prod.:', 'Manual input', 'Data', 'None'),
  # c(model_based_app, export_app, primary_app, '', dataValue, ''))
  btn_radio <- radioGroupButtons(
    inputId = "btn_approach",
    individual = FALSE,
    label = "Approach",
    selected = 1,
    choiceNames = choices_names, 
    choiceValues = choices_names, #1:6,
    status = "primary",
    justified = FALSE,
    direction = "vertical",
    checkIcon = list(
      yes = icon("ok", 
                 lib = "glyphicon"),
      no = icon("remove",
                lib = "glyphicon"))
  )
  
  btn_radio
  
})

# If the proposed value is accepted the selected approach is inserted
observeEvent(input$btn_accept ,{

  approachsel <- input$btn_approach
  approachsel <- gsub(' .*', '', approachsel)
  pp2Rev <- copy(data2imp$valRev)
  
  country <- gsub(' -.*','', input$btn_validate_country)
  product <- gsub('.*- ','', input$btn_validate_product)
  #prodcode <- cpc[description == product & code %in% pp2Rev$measureditemcpc]$code
  
  pp2Rev <- pp2Rev[geographicaream49 == M49[description == country]$code &
                     measureditemcpc ==  product ]
  
  pp2Rev[, selected := FALSE]
  pp2Rev[approach == approachsel, selected := TRUE]
  
  data2imp$valRev <- data2imp$valRev[!pp2Rev, on = c('geographicaream49', 'measureditemcpc')]
  
  changeset <- Changeset('imputation_annual_prices')
  AddModifications(changeset, pp2Rev)
  Finalize(changeset)
  
  showModal(modalDialog(
    title = "Data saved!" ,
    sprintf("The choice has been saved into the SWS datatable. 
            Once all data have been validated, the plugin to save imputation to the dataset can be run.")
  ))
  
})

# If the value is refused the manual button appears
# observeEvent(input$btn_refuse ,{
# 
#   # ppPrep <- copy(dataPP$prep)
#   # ppOut <- copy(dataPP$prepOut)
#   #
#   # country <- gsub(' -.*','', input$btn_outlier)
#   # product <- gsub('.*- ','', input$btn_outlier)
#   #
#   # ppPrep <- ppPrep[geographicAreaM49_description == country  &
#   #                    measuredItemCPC_description == product &
#   #                    flagObservationStatus == 'E' & flagMethod == 'i', c('flagObservationStatus',
#   #                                                                        'flagMethod') := list('', 'q')]
#   #
#   # dataPP$prep <- ppPrep
# 
# })

#-- Manual button
output$out_btn_manual <- renderUI({
  req(input$btn_validate_country, input$btn_validate_product)
  # Button for manual ratio input
  numericInput(inputId = 'btn_manual', label = 'Manual input', value = NA)
  
})

# Save data to SWS when the work is finished (or whenever useful to not lose the work)
observeEvent(input$btn_impute, {
  
  manualvalue <- input$btn_manual
  
  if(!is.na(manualvalue)){
    
    ppVal <- copy(data2imp$validated)
    country <- gsub(' -.*','', input$btn_validate_country)
    product <- gsub('.*- ','', input$btn_validate_product)
    #prodcode <- cpc[description == product & code %in% ppVal$measuredItemCPC]$code
    # Flag to be changed
    ppVal[geographicAreaM49_description == country  &
            measuredItemCPC == product &
            flagObservationStatus == 'I' & flagMethod == 'e', c('Value','flagObservationStatus',
                                                                'flagMethod') := list(manualvalue, 'E', 'f')]
    
    
    pp2Rev <- copy(data2imp$valRev)
    
    country <- gsub(' -.*','', input$btn_validate_country)
    product <- gsub('.*- ','', input$btn_validate_product)
    #prodcode <- cpc[description == product & code %in% pp2Rev$measureditemcpc]$code
    pp2Rev <- pp2Rev[geographicaream49 == M49[description == country]$code &
                       measureditemcpc ==  product ]
    
    pp2Rev[, selected := FALSE]
    
    data2imp$valRev <- data2imp$valRev[!pp2Rev, on = c('geographicaream49', 'measureditemcpc')]
    
    manualappline <- unique(pp2Rev[,.(geographicaream49, measureditemcpc, timepointyears)])
    manualappline[, c('approach', 'estimation', 'selected'):= list('Manual', manualvalue, TRUE)]
 
    changeset <- Changeset('imputation_annual_prices')
    AddModifications(changeset, pp2Rev)
    AddInsertions(changeset, manualappline)
    Finalize(changeset)
    
    
  }
  
  showModal(modalDialog(
    title = "Data saved!" ,
    sprintf("The choice has been saved into the SWS datatable. 
            Once all data have been validated, the plugin to save imputation to the dataset can be run.")
  ))
  
 data2imp$validated <- ppVal
  
})


# 
# observeEvent(input$save_est_sws, {
#   
#   data2save <- copy(dataPP$validated)
#   # to reintroduce
#   # if(localrun){
#   #   if(CheckDebug()){
#   #     library(faoswsModules)
#   #     SETTINGS = ReadSettings("sws.yml")
#   #     R_SWS_SHARE_PATH = SETTINGS[["share"]]
#   #     SetClientFiles(SETTINGS[["certdir"]])
#   #     GetTestEnvironment(baseUrl = SETTINGS[["server"]],
#   #                        token = tokenOutlier)
#   #   }
#   # } else {
#   #   R_SWS_SHARE_PATH = "Z:"
#   #   SetClientFiles("/srv/shiny-server/.R/QA/")
#   #   GetTestEnvironment(baseUrl = "https://swsqa.aws.fao.org:8181",
#   #                      token = tokenOutlier)
#   # }
#   #
#   # SaveData(domain = domainPP,
#   #          dataset = datasetVal,
#   #          data = data2save,
#   #          waitTimeout = Inf)
#   #
# })
