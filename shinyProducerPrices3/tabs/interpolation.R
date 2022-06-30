#-- List of interpolated series ----

# -- Outlier list button ----
# output$btn_interp <- renderUI({
#   
#   input$btn_accept_interp
#   
#   input$btn_no_interp
#   
#   
#   interplist <- unique(interpolation[, .(geographicaream49, measureditemcpc)])
#   
#   interplist <- merge(interplist, M49, by.x = 'geographicaream49', by.y = 'code', all.x = T)
#   interplist <- merge(interplist, cpc, by.x = 'measureditemcpc', by.y = 'code', all.x = T, suffixes = c('_geo', '_cpc'))
#   
#   interp_input <-  sort(sprintf("%s - %s", interplist$description_geo,
#                                 interplist$description_cpc))   
#   
#   
#   selectInput(inputId = "btn_interp",
#               label = 'Interpolated series',
#               choices = c('', interp_input))
#   
# })


output$btn_interp_country <- renderUI({
  
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
  
  interpolation0 <- ReadDatatable('interpolation_annual_prices', readOnly = F)

  interpolation0 <<- interpolation0
  interpolation <- copy(interpolation0[selected == FALSE | is.na(selected)])
  interpolation <<- interpolation
  # Do not include already imputed values
  priceintproposed <- interpolation[, on = c("geographicaream49",
                                             "measureditemcpc",
                                             "timepointyears" )]
  priceintproposed <<- priceintproposed
  countries_int <- unique(priceintproposed$geographicaream49)
  countries_int <<- countries_int
  
  review_input <-  country_input[code %in% countries_int]$label #sort(sprintf("%s - %s", reviewlist$description_geo,
  #reviewlist$description_cpc))   
  
  selectInput(inputId = "btn_interp_country",
              label = 'Country to review',
              choices = c('', review_input))#, 
  #selected = 'Sri Lanka - 144')
  
})

output$btn_interp_product <- renderUI({
  
  req(input$btn_interp_country)
  
  input$btn_accept_interp
  #input$btn_refuse
  input$btn_no_interp
  
  sel_country <- country_input[country_input$label == input$btn_interp_country, code] 
  
  # reviewlist <- unique(data2imp$valRev[, .(geographicaream49,
  #                                         measureditemcpc)])
  reviewlist <- unique(interpolation[geographicaream49 == sel_country, 
                                       ]$measureditemcpc)
  # reviewlist <- merge(reviewlist, M49, 
  #                     by.x = 'geographicaream49', by.y = 'code', all.x = T)
  
  reviewlist <- cpc[code %in% reviewlist, .(code, description)] #merge(reviewlist, cpc, 
  # by.x = 'measureditemcpc', by.y = 'code', 
  # all.x = T, suff = c('_geo', '_cpc'))
  
  review_input <-  sort(sprintf("%s - %s", #reviewlist$description_geo,
                                reviewlist$description,
                                reviewlist$code))   
  
  
  selectInput(inputId = "btn_interp_product",
              label = 'Product to review',
              choices = c('', review_input))#, 
  #selected = 'Sri Lanka - 144')
  
})



observeEvent(input$btn_interp_country,{
  
  if(input$btn_interp_country != '' & data2imp$validated[,.N] == 0){
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


#-- Plot interpolation ----

output$gg_plot_int_comp <- renderPlotly({
  req(input$btn_interp_country, input$btn_interp_product) #, input$btn_year, input$btn_start_year)

  country <- gsub(' -.*','', input$btn_interp_country)
  product <- gsub('.*- ','', input$btn_interp_product)
  #product <- gsub(' [(].*','', product)
  #prodcode <- cpc[description == product & code %in% interpolation$measureditemcpc]$code
  currentInterp <- copy(interpolation[geographicaream49 == M49[description == country]$code & measureditemcpc == product])
  
  currentInterp[ , Flag := paste(flagobservationstatus, flagmethod, sep = ';')]

  # -- Get metadada ----
  priceMDkey = DatasetKey(
    domain = domainPP,
    dataset = datasetPrep,
    dimensions = list(
      Dimension(name = "geographicAreaM49",
                keys = GetCodeList(domainPP, datasetPrep, 'geographicAreaM49')[code == unique(currentInterp$geographicaream49), code]),
      Dimension(name = "measuredElement", 
                keys = GetCodeList(domainPP, datasetPrep, 'measuredElement')[code == '5530', code]), # Metadata on LCU
      Dimension(name = "measuredItemCPC",
                keys = GetCodeList(domainPP, datasetPrep, 'measuredItemCPC')[code == unique(currentInterp$measureditemcpc), code]), #TO SUBSTITUTE WITH ACTUAL COMMODITY!!!! unique(currentInterp$measuredItemCPC)
      Dimension(name = "timePointYears", 
                keys = GetCodeList(domainPP, datasetPrep, 'timePointYears')[code %in% startYear:maxYear, code])))
  #undebug(GetMetadata)

  priceMD <- tryCatch( GetMetadata(priceMDkey), error = function(priceMDkey) NA)
  if(length(priceMD) == 1){
    if(is.na(priceMD)){
      currentInterp[,Metadata := NA]
      }
  } else {
    priceMD <- priceMD[Metadata_Element == 'COMMENT']
   
    priceMD <- priceMD[,c("geographicAreaM49", #"measuredElement",
                          "measuredItemCPC", "timePointYears", "Metadata_Value"), with = F]

    setnames(priceMD, "Metadata_Value", "Metadata")
    
    currentInterp <- merge(currentInterp, priceMD[,c("geographicAreaM49", "measuredItemCPC",
                                                     "timePointYears", "Metadata"), with = F], 
                           by.x = c("geographicaream49", #"measuredElement",
                                    "measureditemcpc", 
                                    "timepointyears"),
                           by.y = c("geographicAreaM49", #"measuredElement",
                                    "measuredItemCPC", 
                                    "timePointYears"), all.x = T)
  }
  currentInterp[, timepointyears := as.numeric(timepointyears)]
  
  currentInterp1 <- melt(currentInterp,
                        measure.vars = c('value', 'interpolation'),
                        variable.name = 'type')
  
  currentInterp1[type == 'value', type := 'current']

  if(length(currentInterp1$value) > 0 & abs(min(c(currentInterp1$value), na.rm = T)) != Inf & 
     abs(max(currentInterp1$value, na.rm = T)) != Inf ){
    miny <- min(currentInterp1$value, na.rm = T) - 0.1*min(currentInterp1$value, na.rm = T)
    maxy <- max(currentInterp1$value, na.rm = T) + 0.1*max(currentInterp1$value, na.rm = T)
    currentInterp1$timepointyears <- as.integer(currentInterp1$timepointyears)
    ggplotly(ggplot(currentInterp1, aes(x = timepointyears, y = value, label= Flag, label2= Metadata))+
               geom_line(aes(col = type, group = type)) +
               geom_point(aes(col = type),size = 2)+
               scale_color_brewer(palette="Set1") +
               ylim(miny, maxy)+
               # theme(legend.position="none") +
               ggtitle(paste('Series in:', country, ' - ',product)))
  } else {
    currentInterp1$timepointyears <- as.integer(currentInterp1$timepointyears)
    ggplotly(ggplot(currentInterp1, aes(x = timepointyears, y = value, label= Flag, label2= Metadata))+
               geom_line(aes(col = type)) +
               geom_point(aes(col = type),size = 2)+
               scale_color_brewer(palette="Set1") +
               #ylim(miny, maxy)+
               # theme(legend.position="none") +
               ggtitle(paste('Series in:', country, ' - ',product)))
    
  }

  
})


output$int_tab_slc <- DT::renderDataTable(server = FALSE, {
  req(input$btn_interp_country, input$btn_interp_product) #, input$btn_year, input$btn_start_year)
  
  country <- gsub(' -.*','', input$btn_interp_country)
  product <- gsub('.*- ','', input$btn_interp_product)
  
  interpTab <- copy(interpolation)
#  country <- gsub(' -.*','', input$btn_interp)
#  product <- gsub('.*- ','', input$btn_interp)
#  prodcode <- cpc[description == product & code %in% interpolation$measureditemcpc]$code
  
  interpTab <- interpTab[geographicaream49 == M49[description == country]$code  &
                           measureditemcpc == product]
  
  setnames(interpTab, c('timepointyears', "flagobservationstatus",
                    "flagmethod", 'value', 'interpolation'),
           c('Year', 'Status', 'Method', 'Current', 'Interp'))
  interpTab <- interpTab[order(Year)]
 # interpTab[ , Variation := round(c(NA,diff(Value))/shift(Value), 3), 
 #        by = c('geographicAreaM49', 'measuredItemCPC')]
  interpTab[,Flag := paste(Status, Method, sep = ';')]
  interpTab <- interpTab[order(-Year)]
  DT::datatable(interpTab[,.(Year, Current, Interp, Flag)], filter = 'top',
                rownames = FALSE)
})


#-- Information datatable ----
# If the value has been detected as outlier a tabel with country and commodity data is shown
output$interp_tab <- DT::renderDataTable(server = FALSE, {
  req(input$btn_interp_country, input$btn_interp_product) #, input$btn_year, input$btn_start_year)
  
  country <- gsub(' -.*','', input$btn_interp_country)
  product <- gsub('.*- ','', input$btn_interp_product)
  
  ppVal0 <- copy(data2imp$validated)
  
 # country <- gsub(' -.*','', input$btn_interp)
 # product <- gsub('.*- ','', input$btn_interp)
 # prodcode <- cpc[description == product & code %in% ppVal0$measuredItemCPC]$code
  
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

# If the proposed value is accepted the selected approach is inserted
observeEvent(input$btn_accept_interp ,{
  
  country <- gsub(' -.*','', input$btn_interp_country)
  product <- gsub('.*- ','', input$btn_interp_product)
  
  # prodcode <- cpc[description == product & code %in% interpolation$measureditemcpc]$code
  
  currentInterp <- copy(interpolation[geographicaream49 == M49[description == country]$code  &
                                        measureditemcpc == product])

  currentInterp[, selected := TRUE]
  
  interpolation <<- interpolation[!currentInterp, on = c('geographicaream49', 'measureditemcpc')]
  
  currentInterp[, value := interpolation]
  changeset <- Changeset('interpolation_annual_prices')
  AddModifications(changeset, currentInterp)
  Finalize(changeset)

  showModal(modalDialog(
    title = "Data saved!" ,
    sprintf("The choice has been saved into the SWS datatable. 
            Once all data have been validated, the plugin to save imputation to the dataset can be run.")
    ))
  
})


# If the value is not recognized as outlier the value is kept and the flag is set again to questionnaire data 
observeEvent(input$btn_no_interp ,{
  country <- gsub(' -.*','', input$btn_interp_country)
  product <- gsub('.*- ','', input$btn_interp_product)
  
  # prodcode <- cpc[description == product & code %in% interpolation$measureditemcpc]$code
  
  currentInterp <- copy(interpolation[geographicaream49 == M49[description == country]$code  &
                                        measureditemcpc == product])
  
  interpolation <<- interpolation[!currentInterp, on = c('geographicaream49', 'measureditemcpc')]
  
  currentInterp[, selected := TRUE]
  
  changeset <- Changeset('interpolation_annual_prices')
  AddModifications(changeset, currentInterp)
  Finalize(changeset)

  showModal(modalDialog(
    title = "Data saved!" ,
    sprintf("The choice has been saved into the SWS datatable. 
            Once all data have been validated, the plugin to save imputation to the dataset can be run.")
    ))
  
  
})