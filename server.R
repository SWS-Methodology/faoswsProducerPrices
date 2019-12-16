 function(input, output) {

#-- BUTTONS ---- 
   
#-- Selected year button ----
   # Once the country is selected, i.e. different from "", the year can be chosen 1991-present
   output$btn_year  <- renderUI({
     
     # Country button required
     req(input$btn_country)
     sel_country <- country_input[country_input$label == input$btn_country, code]
     
     if(sel_country != "") {
       
       # Input details
       selectInput(inputId = "btn_year",
                   label = 'End year',
                   choices = c("", years_input)#,
                   #selected = '2015'
       )
     }
   })
   
#-- Start year button ----
   # Same as year button but must be lower than btn_year
   output$btn_start_year <- renderUI({
     
     # Country and year buttons required
     req(input$btn_country, input$btn_year)
     sel_country <- country_input[country_input$label == input$btn_country, code]

     if(sel_country != "" & input$btn_year != "") {
       
       start_year_input <- years_input[years_input < input$btn_year]
       
       selectInput(inputId = "btn_start_year",
                   label = 'Start year',
                   choices = c("", start_year_input)#,
                  # selected = '2010'
       )
     }
   })
   
#-- Data stored ----
   #Data to store: from questionnaire, preparation, validated and all (with data 0,M,u)
   
   dataPP <- reactiveValues(quest = data.table(),
                            prep = data.table(),
                            validated = data.table(),
                            all = data.table())

#-- Load data ----
   # Once year and country are selected the 'annual_producer_prices_quest' (TO CHANGE IN THE SCRIPT NOW USING VALIDATED)
   # and 'annual_producer_prices_validated' datset are loaded and stored
   observeEvent(input$btn_start_year, {
     
     if(input$btn_start_year != ''){
     
                sel_country <- country_input[country_input$label == input$btn_country, code]
                sel_years <- as.character(input$btn_start_year:input$btn_year)
                
                if(sel_country != ''){
                  
                  priceKey = DatasetKey(
                    domain = domain,
                    dataset = dataset,
                    dimensions = list(
                      Dimension(name = "geographicAreaM49",
                                keys = sel_country),
                      Dimension(name = "measuredElement", 
                                keys = GetCodeList(domain, dataset, 'measuredElement')[, code]),
                      Dimension(name = "measuredItemCPC",
                                keys = GetCodeList(domain, dataset, 'measuredItemCPC')[, code]),
                      Dimension(name = "timePointYears", 
                                keys = sel_years)
                      
                    )
                  )
                  
                  withProgress(message = 'Data loading in progress',
                               value = 0, {
                                 
                                 Sys.sleep(0.25)
                                 incProgress(0.1)
                                 priceData = GetData(
                                   priceKey,
                                   flags = TRUE)
                                 incProgress(0.25)
                                 
                                 
                                 priceData <- nameData(domain, dataset, priceData)
                                 setnames(priceData, 'flag_obs_status_v2','flagObservationStatus')
                                 # countData <- priceData[, .N, timePointYears]
                                 # sum(countData$N)
                                
                                 incProgress(0.5)
                                 
                                 #-- Pull past data validated ----
                                 domain <- 'prod_prices'
                                 dataset <- 'annual_producer_prices_validated'
                               
                                 priceKey = DatasetKey(
                                   domain = domain,
                                   dataset = dataset,
                                   dimensions = list(
                                     Dimension(name = "geographicAreaM49",
                                               keys = sel_country),
                                     Dimension(name = "measuredElement", 
                                               keys = GetCodeList(domain, dataset, 'measuredElement')[, code]),
                                     Dimension(name = "measuredItemCPC",
                                               keys = GetCodeList(domain, dataset, 'measuredItemCPC')[, code]),
                                     Dimension(name = "timePointYears", 
                                               keys = sel_years)
                                     
                                   )
                                 )
                                 
                                 incProgress(0.75)
                                 priceDatapast = GetData(
                                   priceKey,
                                   flags = TRUE)
                                 
                                 incProgress(0.95)
                               })
                  
                  setnames(priceDatapast, 'flag_obs_status_v2','flagObservationStatus')
                
                  #----
                
                  dataPP$quest <- priceData
                  dataPP$validated <- priceDatapast
                }}
                })

   #-- TAB1 ----
   
   #-- Compare data ----
# merge quest and validated data and comparing them if data are not in a dataset they are flagged as (0,O,-)
# two objects are returned, one for tab and the other for plot
# The tab shows item by item, the plot only shows the total of the elements
  compareTab_reac <- reactive({

    req(input$btn_country, input$btn_year, input$btn_start_year)

    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
    
    

    ppquest <- dataPP$quest
    #years2compare <- unique(ppquest$timePointYears) # HOW TO DEAL WITH THAT WITH QUESTIONNAIRE? HOW TO KNOW THE NUMBER OF YEAR?
                                                     # ERASE THE DATASET EVRY TIME NEW DATA COME? SELECT THE YEARS IN A DIFFERENT WAY...
    years2compare <- sel_years
  
    ppval <- dataPP$validated[timePointYears %in% years2compare]
    # Now only showing value present both in quest and valid, CHANGE?
    questVSval <- merge(ppquest, ppval,
                          by = c('geographicAreaM49', 'measuredItemCPC',
                                 'measuredElement', 'timePointYears'),
                          suffixes = c('Quest', 'Valid'), all = TRUE)

    questVSval[is.na(ValueQuest), flagObservationStatusquest := 'O']
    questVSval[is.na(ValueValid), flagObservationStatusvalid := 'O']

    questVSval[is.na(ValueQuest), flagMethodquest := '-']
    questVSval[is.na(ValueValid), flagMethodvalid := '-']

    questVSval[is.na(ValueQuest), ValueQuest := 0]
    questVSval[is.na(ValueValid), ValueValid := 0]

    quest2plot <- questVSval[ , .(geographicAreaM49,
                                     measuredItemCPC,
                                     measuredElement,
                                     timePointYears,
                                     ValueQuest)]
    quest2plot[ , type := 'quest']

    valid2plot <- questVSval[ , .(geographicAreaM49,
                                  measuredItemCPC,
                                  measuredElement,
                                  timePointYears,
                                   ValueValid)]
    valid2plot[ , type := 'valid']

    setnames(quest2plot, 'ValueQuest', 'Value')
    setnames(valid2plot, 'ValueValid', 'Value')

    data4plot <- rbind(quest2plot, valid2plot)

    return(list(tab = questVSval, plot = data4plot))

  })

# If the variation is 
  output$out_tab_comp <- DT::renderDataTable(server = FALSE, {
    req(input$btn_country, input$btn_year, input$btn_start_year)

    ppCompare <- copy(compareTab_reac()$tab)

    setnames(ppCompare, c('geographicAreaM49', 'measuredItemCPC',
                             'measuredElement', 'timePointYears',
                             'flagObservationStatusQuest', 'flagMethodQuest',
                             'flagObservationStatusValid', 'flagMethodValid'),
             c('Country', 'CPC', 'Element', 'Year', 'Flagquest1', 'Flagquest2',
               'Flagvalid1', 'Flagvalid2'))

    ppCompare[ , Variation := round( (ValueQuest - ValueValid)/ValueValid, 3)]
    
    DT::datatable(ppCompare, extensions = 'Buttons', filter = 'top',
                  rownames = FALSE, options = list(dom = 'Bfrtip',
                                                   buttons = c('csv', 'excel', 'pdf'))) %>%
      formatStyle(columns = c('Variation'), target = 'row',
                  color = styleInterval(c(-0.001, 0.001), c('red', ' ', 'red')))
  })

  output$gg_plot_out_comp <- renderPlot({
    req(input$btn_country, input$btn_year, input$btn_start_year)
    
    ppCompare <- copy(compareTab_reac()$plot)

    # Make grand total by element
    grandtotal <- copy(ppCompare)
    grandtotal <- grandtotal[ , c('Value', 'measuredItemCPC') := list(sum(Value, na.rm = TRUE), 'Total'),
                              by = c('geographicAreaM49',
                                     'measuredElement', 
                                     'timePointYears', 'type')]
    setkey(grandtotal)
    grandtotal <- grandtotal[!duplicated(grandtotal)]

    ggplot(data = grandtotal, aes(x = timePointYears, y = Value)) +
      geom_line(aes(group = type, color = type), size = 0.7) +
      facet_wrap( ~ measuredElement, scales="free") +
      labs(x = 'Year', color = '')

  })
  
  
#-- TAB2 ----
  
#-- Show data & validate available data ----
  # Show questionnaire data
  # If anything to change it can be done then the new data (years in the questionnaires) 
  # are appended to validated data (years not in the questionnaire) 
  valQuestTab_reac <- reactive({
    req(input$btn_country, input$btn_year, input$btn_start_year)
    
    priceData <- dataPP$quest
    
    return(priceData)
  })
  
  output$rawData <- renderRHandsontable({
    
    req(input$btn_country, input$btn_year, input$btn_start_year)
    
    priceData <- valQuestTab_reac()
    #priceData <- dataPP$quest
    rhandsontable(priceData, rowHeaders = NULL, width = 'auto', height = 'auto') 
  })
  
#-- Validate button ----
  
  observeEvent(input$validateQuest, {

    dataPP$quest <- valQuestTab_reac()
    
    past <- dataPP$validated
    new <- valQuestTab_reac()
    new <- new[ , -grep("_description", colnames(new)), with = FALSE]
    yearsnew <- unique(new$timePointYears)
    
    # TO CHECK IF CORRECT!!!!
    newData <- rbind(past[!timePointYears %in% yearsnew], new)
    addMissing <- expandYear(newData,
                             # obsflagVar = "flag_obs_status_v2",
                             newYears = as.integer(input$btn_year))
    
    dataPP$all <- addMissing
    
    showModal(modalDialog(
      title = "Series validated" ,
      sprintf("Data are ready for imputation.")
    ))
    
    
  })
  
#-- TAB3 ----
#-- Button missing ----
  output$btn_missing <- renderUI({
    
    selectInput(inputId = "btn_missing",
                label = 'Type of data',
                choices = c('', 'Missing', 'Official'), 
                selected = '')

  })
  
  
#-- Button products ----
  # Choice of the product
  
  output$btn_product <- renderUI({
  req(input$btn_missing)
    sel_country <- country_input[country_input$label == input$btn_country, code]
    
    if(input$btn_missing == 'Missing'){
      
      products <- unique(dataPP$all[geographicAreaM49 == sel_country &
                                    measuredElement == '5530' &
                               timePointYears == input$btn_year &
                               flagObservationStatus == 'M' &
                                   flagMethod == 'u']$measuredItemCPC)
      
    } else if(input$btn_missing == 'Official'){
      
      products <- unique(dataPP$all[geographicAreaM49 == sel_country &
                                      measuredElement == '5530' &
                                      timePointYears == input$btn_year &
                                      flagMethod != 'u']$measuredItemCPC)
      
    } else {
      products <- ''
    }
    
    product_list <- cpc[code %in% products]$description
    
    selectInput(inputId = 'btn_product',
                label = 'Product',
                choices = c('', product_list),
                selected = ''
                )
  })

#-- Panel overview ----
  # For the chosen product displaying the tab and the plot
  
  YP_reac <- reactive({
    req(input$btn_product)
    
    if(input$btn_product != ''){
    # sel_years not needed as showing the whole time series
    # sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
    sel_country <- country_input[country_input$label == input$btn_country, code]
    
    sel_product <- cpc[description == input$btn_product]$code
    #Year and price value table
    tabYP <- dataPP$all[geographicAreaM49 == sel_country &
                          measuredItemCPC == sel_product & 
                          measuredElement == '5530']
    
    return(tabYP)
    }
  })

  output$YPtab <- DT::renderDataTable(server = FALSE, {
    req(input$btn_missing, input$btn_product)
    if(is.null(YP_reac)) return(NULL)
    sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
    tabYP <- YP_reac()[ ,.(timePointYears, Value)]

    DT::datatable(tabYP, rownames = FALSE)
  })
  
  output$gg_plot_YP <- renderPlot({
    req(input$btn_product)
    # Here time series can be customized ? Or plot that can be enlarged
    # sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
    sel_country <- country_input[country_input$label == input$btn_country, code]
    
    if(is.null(YP_reac)) return(NULL)
    plotYP <- YP_reac()
    
    ggplot(data = plotYP, aes(x = timePointYears, y = Value)) +
      geom_point(size = 2) +
      #geom_line(size = 1) +
      labs(x = 'Year')
    
  })
  
#-- TAB GROUP INFO ----  
# Take last growth rate for commodities selected and show among these commodities the mean, median, max and min rate
  # List of commodities that are not missing 
  output$btn_group_info <- renderUI({
    req(input$btn_country, input$btn_year)
    sel_country <- country_input[country_input$label == input$btn_country, code] 
    data2use <- dataPP$all[geographicAreaM49 == sel_country &
                         flagMethod != 'u']
    data2use_label <- nameData(domain, dataset, data2use)
    list <- unique(data2use_label$measuredItemCPC_description)
    
    checkboxGroupInput(inputId = "btn_group_info",
                       label = 'Select products',
                       choices = list,
                       selected = NULL)
    
  })
  
  # Computing growth rate for VALIDATED (?) data chosen, only selected years 
  group_info_reac <- reactive({
    sel_country <- country_input[country_input$label == input$btn_country, code] 
    sel_group_products <- cpc[description %in% input$btn_group_info]$code 
    lastyears <- as.character((as.numeric(input$btn_year)-1):as.numeric(input$btn_year))
    
    dataGroup <- dataPP$validated
    dataGroup <- dataGroup[geographicAreaM49 == sel_country &
                             measuredElement == '5530' &
                             measuredItemCPC %in% sel_group_products &
                             timePointYears %in% lastyears]
    dataGroup <- dataGroup[ order(timePointYears), ]
    dataGroup$Growth <- with(dataGroup, ave(Value, measuredItemCPC, 
                                          FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))
    
    return(dataGroup[!is.na(Growth)])
  })
  
  output$groupInfo_tab <- DT::renderDataTable(server = FALSE, {
    req(input$btn_group_info)
    #sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
    groupTab <- group_info_reac()
    
    DT::datatable(groupTab, rownames = FALSE, filter = 'top')
  })
 
  output$summary_growth_rate <- renderUI({ 
    req(input$btn_group_info)
    groupTab <- group_info_reac()
    
    # Average growth rate
    avG <- mean(groupTab$Growth, na.rm = TRUE)
    # Median growth rate
    N <- length(groupTab$Growth)
    
    if(N%%2 == 0){
      N <- c((N/2) - 1, (N/2) + 1)
      medG <- mean(sort(groupTab$Growth)[N], na.rm = TRUE)
    } else {
      N <- (N/2)+0.5
      medG <- mean(sort(groupTab$Growth)[N], na.rm = TRUE)
    }
    
    maxG <- max(groupTab$Growth, na.rm = TRUE)
    minG <- min(groupTab$Growth, na.rm = TRUE)
    
    choices_names <- paste(c('Average:', 'Median:', 'Maximum:', 'Minimum:', 'Manual'),
                           c(avG, medG, maxG, minG, ''))
    
  btn_radio <- radioGroupButtons(
    inputId = "btn_gR",
    individual = FALSE,
    label = "Growth rate",
    choiceNames = choices_names, 
    choiceValues = 1:5,
    status = "info",
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
  
  # Insert manual button to choos the value manually
  output$out_btn_manual <- renderUI({
    if(input$btn_gR != 5) return(NULL)
    numericInput(inputId = 'btn_manual', label = 'Manual rate', value = NA)
    
    
  })
  
  # Button to impute the growth rate and compute the missing price
  observeEvent(input$btn_imp_group, {
  
    
    })
  

#-- TAB AUX INFO ----
  
  output$contents <- renderTable({
    req(input$file1)
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header #,
                       #   sep = input$sep,
                       #  quote = input$quote
        )
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    return(df)
    
  })
  
  output$aux_var <- renderUI({ 
    
    choices_names <- c('International Commodity Priced',
                       'CPI & Food CPI',
                       'GDP & Agr. GDP Deflators',
                       'Eurostat AgPPI Indices',
                       'GIEWS prices',
                       'Exchange rates')

    btn_aux <- radioGroupButtons(
      inputId = "btn_aux_var_choice",
      individual = FALSE,
      label = "Auxiliary Variables",
      choiceNames = choices_names,
      choiceValues = 1:6,
      status = "info",
      justified = FALSE,
      direction = "vertical",
      checkIcon = list(
        yes = icon("ok",
                   lib = "glyphicon"),
        no = icon("remove",
                  lib = "glyphicon"))
    )

    btn_aux

  })

 
#-- TAB PRICE RATIO ----
 
 output$btn_price_ratio <- renderUI({
   
   sel_country <- country_input[country_input$label == input$btn_country, code] 
   data2use <- dataPP$all[geographicAreaM49 == sel_country &
                            flagMethod != 'u']
   data2use_label <- nameData(domain, dataset, data2use)
   list <- unique(data2use_label$measuredItemCPC_description)
   
   radioButtons(inputId = "btn_price_ratio",
                label = 'Select products',
                choices = list,
                selected = NULL)
   
 }) 
 
 
 output$out_btn_manual_tcf <- renderUI({
   numericInput(inputId = 'btn_manual_tcf', label = 'Manual rate', value = NA)
 })

 output$imp_value_price_ratio <- renderUI({ 
   
   tcf <- input$out_btn_manual_tcf
   pivot <- dataPP$all # select the commodity
   
   value <- pivot*tcf
   
   btn_pr <- radioGroupButtons(
     inputId = "btn_pr_choice",
     individual = FALSE,
     label = "Value to impute",
     choiceNames = value,
     choiceValues = 1,
     status = "info",
     justified = FALSE,
     direction = "vertical",
     checkIcon = list(
       yes = icon("ok",
                  lib = "glyphicon"),
       no = icon("remove",
                 lib = "glyphicon"))
   )
   
   btn_pr
   
 })
  
 
 }