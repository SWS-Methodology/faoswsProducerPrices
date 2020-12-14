# priceData[flagObservationStatus == 'I' & flagMethod == 'e']) # priceproposed

# -- Insert token ---- 
output$btn_tokenOut <- renderUI({
  textInput(inputId = 'btn_tokenOut', 
            label = paste("'Preparation' dataset session token. Last update: ", dateOutlierToken, sep = ''), 
            value = NA)
})

observeEvent(input$btn_upd_token, {

  tokenTab <- ReadDatatable('pp_token', readOnly = FALSE)
  
  t1 <- ifelse(is.na(input$btn_tokenOut), tokenTab$token[1], input$btn_tokenOut)
  date <- as.character(Sys.Date())
  
  tokenTab[1 , token := c(t1)]
  tokenTab[1 , last_upd := date]
  
  changeset <- Changeset('pp_token')
  AddModifications(changeset, tokenTab)
  Finalise(changeset)
  
  tokenOutlier <<- t1
  
  showModal(modalDialog(
    title = "Token updated." ,
    sprintf("The chosen session will be used in the following tabs.")
  ))
  
})



# -- Outlier list button ----
output$btn_outlier <- renderUI({
  
  input$outlier_impute
  
  input$btn_not_out
  
  
  outlierlist <- unique(dataPP$prepSLC[timePointYears %in% as.character(2014:2016) &
                                         flagObservationStatus == 'E' & flagMethod == 'i', .(geographicAreaM49_description,
                                                                                             measuredItemCPC_description,
                                                                                             timePointYears)])
  
  outlier_input <-  sort(sprintf("%s - %s (%s)", outlierlist$geographicAreaM49_description,
                                 outlierlist$measuredItemCPC_description,
                                 outlierlist$timePointYears))   
  
  
  selectInput(inputId = "btn_outlier",
              label = 'Outlier',
              choices = c('', outlier_input))#, 
  #selected = 'Sri Lanka - 144')
  
})

output$save_out_sws <- renderUI({
  actionBttn('save_out_sws', 
             label = 'Save in SWS',
             style = 'gradient', 
             color = 'default')
})


#-- Load Preparation data ----
# Once year and country are selected the 'annual_producer_prices_prep' dataset is loaded and stored


#-- Outlier data ----

# If the value has been detected as outlier a tabel with country and commodity data is shown
output$out_tab_comp <- DT::renderDataTable(server = FALSE, {
  req(input$btn_outlier) #, input$btn_year, input$btn_start_year)
  
  #ppPrep0 <- copy(dataPP$prepUSD)
  ppPrep0 <- rbind(copy(dataPP$prepUSD), copy(dataPP$prepSLC), copy(dataPP$prepLCU))
  country <- gsub(' -.*','', input$btn_outlier)
  product <- gsub('.*- ','', input$btn_outlier)
  product <- gsub(' [(].*','', product)
  
  ppPrep <- ppPrep0[geographicAreaM49_description == country | 
                      measuredItemCPC_description == product]
  
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
  DT::datatable(ppPrep[,.(Region, `Sub-Region`, Country, CPC, Year, Type, 
                          Value, Status, Method, Variation)], filter = 'top',
                rownames = FALSE)
})

output$gg_plot_out_comp <- renderPlotly({
  req(input$btn_outlier) #, input$btn_year, input$btn_start_year)
  
  ppPrep <- copy(dataPP$prepSLC)
  country <- gsub(' -.*','', input$btn_outlier)
  product <- gsub('.*- ','', input$btn_outlier)
  product <- gsub(' [(].*','', product)
  year <- gsub('.*[(]','', input$btn_outlier)
  year <- gsub('[)]','', year)
  
  ppPrep <- ppPrep[geographicAreaM49_description == country  &
                     measuredItemCPC_description == product ]
  ppPrep[, Outlier := FALSE]
  ppPrep[flagObservationStatus == 'E' & flagMethod == 'i' & timePointYears == year, Outlier := TRUE]
  ppPrep[ , Flag := paste(flagObservationStatus, flagMethod, sep = ';')]
  
  # -- Get metadada ----
  priceMDkey = DatasetKey(
    domain = domainPP,
    dataset = datasetVal,
    dimensions = list(
      Dimension(name = "geographicAreaM49",
                keys = GetCodeList(domainPP, datasetVal, 'geographicAreaM49')[code == unique(ppPrep$geographicAreaM49) , code]),
      Dimension(name = "measuredElement", 
                keys = GetCodeList(domainPP, datasetVal, 'measuredElement')[code == '5531', code]),
      Dimension(name = "measuredItemCPC",
                keys = GetCodeList(domainPP, datasetVal, 'measuredItemCPC')[code == "0115", code]), #TO SUBSTITUTE WITH ACTUAL COMMODITY!!!!
      Dimension(name = "timePointYears", 
                keys = GetCodeList(domainPP, datasetPrep, 'timePointYears')[code %in% startYear:maxYear, code])))
  #undebug(GetMetadata)

  priceMD <- tryCatch( GetMetadata(priceMDkey), error = function(priceMDkey) NA)
  if(length(priceMD) == 1){
    if(is.na(priceMD)){
    ppPrep[,Metadata := NA]}
  } else {
  priceMD <- priceMD[Metadata_Element == 'COMMENT']
  names(priceMD)
  priceMD <- priceMD[,c("geographicAreaM49", "measuredElement",
                        "measuredItemCPC", "timePointYears", "Metadata_Value"), with = F]
  setnames(priceMD, "Metadata_Value", "Metadata")

  ppPrep <- merge(ppPrep, priceMD[,c("geographicAreaM49", "measuredElement",
                                     "timePointYears", "Metadata"), with = F], by = c("geographicAreaM49", "measuredElement",
                              # "measuredItemCPC", TO REINTRODUCE!!!!
                               "timePointYears"), all.x = T)
  }
  ppPrep$timePointYears <- as.numeric(ppPrep$timePointYears)
  miny <- min(ppPrep$Value) - 0.1*min(ppPrep$Value)
  maxy <- max(ppPrep$Value) + 0.1*max(ppPrep$Value)
 
  ggplotly(ggplot(ppPrep, aes(x = timePointYears, y = Value, label= Flag, label2= Metadata))+
    geom_line(col = "grey31") +
    geom_point(aes(col = Outlier),size = 2)+
    scale_colour_manual(values = c("FALSE" = "grey31", "TRUE" = "coral"))+
    ylim(miny, maxy)+
    theme(legend.position="none") +
    ggtitle(paste('Series:', country, ' - ',product)))
  
  
#  ggplotly(aa)
  
})

# Show series with variation 
output$out_tab_slc <- DT::renderDataTable(server = FALSE, {
  req(input$btn_outlier) #, input$btn_year, input$btn_start_year)
  
  ppPrep <- copy(dataPP$prepSLC)
  
  country <- gsub(' -.*','', input$btn_outlier)
  product <- gsub('.*- ','', input$btn_outlier)
  product <- gsub(' [(].*','', product)
  year <- gsub('.*[(]','', input$btn_outlier)
  year <- gsub('[)]','', year)
  
  ppPrep <- ppPrep[geographicAreaM49_description == country  &
                     measuredItemCPC_description == product ]
  ppPrep[, Outlier := FALSE]
  ppPrep[flagObservationStatus == 'E' & flagMethod == 'i' & timePointYears == year, Outlier := TRUE]

  setnames(ppPrep, c('timePointYears', "flagObservationStatus",
                     "flagMethod"),
           c('Year', 'Status', 'Method'))
  ppPrep <- ppPrep[order(Year)]
  ppPrep[ , Variation := round(c(NA,diff(Value))/shift(Value), 3), 
          by = c('geographicAreaM49', 'measuredItemCPC')]
  ppPrep[,Flag := paste(Status, Method, sep = ';')]
  ppPrep <- ppPrep[order(-Year)]
  
  col2hide <- which(names(ppPrep)=='Outlier')
  
  DT::datatable(ppPrep[,.(Year, Value, Flag, Variation)], filter = 'top',
                rownames = FALSE) #%>% 
    #formatStyle("Outlier", target = 'row', color=styleEqual(TRUE, "red"))
})

# If the value is detected as outlier the the value is replaced by NA and will be estimated with the imputation plugin
# observeEvent(input$btn_is_out ,{
#   
#   ppPrep <- copy(dataPP$prep)
#   ppOut <- copy(dataPP$prepOut)
#   
#   country <- gsub(' -.*','', input$btn_outlier)
#   product <- gsub('.*- ','', input$btn_outlier)
#   product <- gsub(' [(].*','', product)
#   year <- gsub('.*[(]','', input$btn_outlier)
#   year <- gsub('[)]','', year)
#   
#   ppPrep <- ppPrep[geographicAreaM49_description == country  &
#                      measuredItemCPC_description == product &
#                      timePointYears == year &
#                      flagObservationStatus == 'E' & flagMethod == 'i', c('Value',
#                                                                          'flagObservationStatus',
#                                                                          'flagMethod') := list(NA, 'M', 'u')]
#   
#   dataPP$prep <- ppPrep
#   
# })
# 

output$btn_outlier_manual <- renderUI({
  req(input$btn_is_out)
  # Button for manual ratio input
  numericInput(inputId = 'outlier_manual', label = 'Manual input', value = NA)
  
})

# Save data to SWS when the work is finished (or whenever useful to not lose the work)
observeEvent(input$outlier_impute, {
  
  manualvalue <- input$outlier_manual
  
  ppPrep <- copy(dataPP$prepSLC)
  ppOut <- copy(dataPP$prepOut)
  
  country <- gsub(' -.*','', input$btn_outlier)
  product <- gsub('.*- ','', input$btn_outlier)
  product <- gsub(' [(].*','', product)
  year <- gsub('.*[(]','', input$btn_outlier)
  year <- gsub('[)]','', year)
  
  if(!is.na(manualvalue)){
    
    # Flag to be changed
    ppPrep <- ppPrep[geographicAreaM49_description == country  &
                       measuredItemCPC_description == product &
                       timePointYears == year &
                       flagObservationStatus == 'E' & flagMethod == 'i', c('Value',
                                                                           'flagObservationStatus',
                                                                           'flagMethod') := list(manualvalue, 'E', 'f')]
  } else {
    
    # Flag to be changed
    ppPrep <- ppPrep[geographicAreaM49_description == country  &
                       measuredItemCPC_description == product &
                       timePointYears == year &
                       flagObservationStatus == 'E' & flagMethod == 'i', c('Value',
                                                                           'flagObservationStatus',
                                                                           'flagMethod') := list(NA, 'M', 'u')]
  }
  
  dataPP$prepSLC <- ppPrep
  
})

# If the value is not recognized as outlier the value is kept and the flag is set again to questionnaire data 
observeEvent(input$btn_not_out ,{
  
  ppPrep <- copy(dataPP$prepSLC)
  ppOut <- copy(dataPP$prepOut)
  
  country <- gsub(' -.*','', input$btn_outlier)
  product <- gsub('.*- ','', input$btn_outlier)
  product <- gsub(' [(].*','', product)
  year <- gsub('.*[(]','', input$btn_outlier)
  year <- gsub('[)]','', year)
  
  ppPrep <- ppPrep[geographicAreaM49_description == country  &
                     measuredItemCPC_description == product & 
                     timePointYears == year &
                     flagObservationStatus == 'E' & flagMethod == 'i', c('flagObservationStatus',
                                                                         'flagMethod') := list('', 'q')]
  
  dataPP$prepSLC <- ppPrep
  
})

#-- Save data to SWS when the work is finished (or whenever useful to not lose the work) ----
observeEvent(input$save_out_sws, {
  
  data2save <- copy(dataPP$prepSLC)
  data2save <- data2save[,.(geographicAreaM49,measuredElement,measuredItemCPC,
                            timePointYears, Value,
                            flagObservationStatus, flagMethod)]
  setnames(data2save, 'Value', 'ValueSLC')
  
  # if(nrow(pper0[is.na(Value_er)]) >0){
  #   misscountry <- unique(pper0[is.na(Value_er)]$geographicAreaM49)
  #   message(paste('Missing exchange rate for: ', misscountry, sep = ''))
  # }
  
  data2save[, Value := ValueSLC]
  
  # get appropriate shape and flags (USD and SLC calculated, 'i')
  pper <- melt(data2save, measure.vars = c('Value', 'ValueSLC'),
               value.name = 'Value')

  pper[variable == 'Value', c('measuredElement', 
                                 'flagMethod') := list('5530', 'tbd')]
  pper[ , c('variable')] <- NULL
  
  
  if(any(pper$flagObservationStatus == 'B')){
 
    geotimecomb <- unique(pper[flagObservationStatus == 'B', .(geographicAreaM49, timePointYears, from_currency)])
    
    # Get datatable with conversion rates 
    # If change of currency (the datatable has to be updated)
    conv_rates <- ReadDatatable('currency_changes')
    
    conv_rates_needed <- merge(conv_rates, geotimecomb, by.x  = 'new_currency_code',
                               by.y = 'from_currency')
    
    slcval <- merge(data2save, conv_rates_needed, by = 'geographicAreaM49', 
                    all.x = T, suffixes = c('', '_change'))
    
    slcval[measuredElement == '5530' & timePointYears < timePointYears_change, c('Value',
                                                                                 'flagObservationStatus', 
                                                                                 'flagMethod'):= list(Value*exchange_rate,
                                                                                                      flagObservationStatus,
                                                                                                      'i')]
    names(slcval)
    slcval[ , c("new_currency_code",    
                "old_currency_code",
                "exchange_rate",
                "timePointYears_change")] <- NULL
    
    slcquest <- merge(pper, conv_rates_needed,  by = 'geographicAreaM49',
                      all.x = T, suffixes = c('', '_change'))
    
    slcquest[measuredElement == '5530' & timePointYears < timePointYears_change, c('Value',
                                                                                   'flagObservationStatus', 
                                                                                   'flagMethod'):= list(Value*exchange_rate,
                                                                                                        flagObservationStatus,
                                                                                                        'i')]
    slcquest[ , c("new_currency_code",    
                  "old_currency_code",
                  "exchange_rate",
                  "timePointYears_change")] <- NULL
    
    
    
  } else {
   # slcval <- val_price
    datalcu <- pper
  }
  
  
  # USD conversion 
  # Get country-currency datatatble ADD withdraw year/effective change in series
  lcu_2_m49 <- ReadDatatable('lcu_2_m49')
  lcu_2_m49[start_year_iso == '', start_year_iso := '1900']
  lcu_2_m49[end_year_iso == '', end_year_iso := '9999']
  
  # Pull exchange rates dataset
  
  erKey = DatasetKey(
    domain = 'common',
    dataset = 'exchange_rates_annual',
    dimensions = list(
      Dimension(name = 'geographicAreaM49',
                keys = GetCodeList('common', 'exchange_rates_annual', 'geographicAreaM49')[code %in% unique(data2save$geographicAreaM49), code]),
      Dimension(name = "from_currency",
                keys = GetCodeList('common', 'exchange_rates_annual', 'from_currency')[code != 'ECU' & endDate >= '1991-01-01' | 
                                                                                         is.na(endDate), code]),
      Dimension(name = "to_currency", 
                keys = GetCodeList('common', 'exchange_rates_annual', 'to_currency')[code == 'USD', code]),
      Dimension(name = 'measuredElement',
                keys = 'LCU'),
      Dimension(name = "timePointYears", 
                keys = GetCodeList('common', 'exchange_rates_annual', 'timePointYears')[code %in% unique(data2save$timePointYears), code]))
    
  )
  
  erdt <- GetData(erKey, flags = F) 
  
  erdt[,c('measuredElement', 'to_currency')] <- NULL
  
  # # check on currency
  # if(!all(erdt$from_currency %in% lcu_2_m49$code_iso)){
  #   stop(paste('Missing countey-currency correspondence: ', 
  #              unique(erdt[!from_currency %in% lcu_2_m49$code_iso]$from_currency),
  #              'not in the lcu_2_m49 datatble. Please update it.'))
  # }
  
  # Start conversion into USD and SLC merging with XR
  
  ## FIX DUPLICATES!!!!!!
  
  pper0 <- merge(datalcu, erdt[!geographicAreaM49 %in% c('233','428','440')], by = c('geographicAreaM49', 'timePointYears'), all.x = T,
                 suffixes = c('', '_er'))
  
  pper0[measuredElement == '5530', ValueUSD := Value/Value_er]
  #### erdt[duplicated(erdt[,.( geographicAreaM49, timePointYears)])] !!!!!!!
  
  pper0[, c("Value_er")] <- NULL
  
  pper2 <- melt(pper0, measure.vars = c('Value', 'ValueUSD'),
               value.name = 'Value')
  
  pper2[variable == 'ValueUSD', c('measuredElement', 
                              'flagMethod') := list('5532', 'tbd')]
  pper2[ , c('variable', 'from_currency')] <- NULL
  
  pper3 <- pper2[ !is.na(Value)]
  
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
    SetClientFiles("/srv/shiny-server/.R/QA/")
    GetTestEnvironment(baseUrl = "https://swsqa.aws.fao.org:8181",
                       token = tokenOutlier)
  }
  
  SaveData(domain = domainPP,
           dataset = datasetPrep,
           data = pper3,
           waitTimeout = Inf)
  
  showModal(modalDialog(
    title = "Data saved!" ,
    sprintf("Please check and save the SWS session data to the dataset.")
  ))
  
})
