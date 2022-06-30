# -- Dataset to show ----

output$btn_dataset_ppi <- renderUI({
  
  selectInput(inputId = "btn_dataset_ppi",
              label = 'Dataset',
              choices = c("", 'Preparation', 'Validated'))
  
})

# -- Country button ----

output$btn_country_ppi <- renderUI({
req(input$btn_dataset_ppi)
  
  selectInput(inputId = "btn_country_ppi",
              label = 'Country',
              choices = c('', country_input$label))  
  
})


# -- End year button ----
output$btn_year_ppi  <- renderUI({
  
  # Country button required
  req(input$btn_country_ppi)
  
  input$btn_country_ppi
  
  sel_country <- country_input[country_input$label == input$btn_country_ppi, code]
  
  if(sel_country != "") {
    
    # Input details
    selectInput(inputId = "btn_year_ppi",
                label = 'End year',
                choices = c("", years_input)#,
                #   selected = '2016'
    )
  }
})

# -- Start year button ----
# Same as year button but must be lower than btn_year
output$btn_start_year_ppi <- renderUI({
  
  # Country and year buttons required
  req(input$btn_country_ppi, input$btn_year_ppi)
  
  input$btn_country_ppi
  
  sel_country <- country_input[country_input$label == input$btn_country_ppi, code]
  
  if(sel_country != "" & input$btn_year_ppi != "") {
    
    start_year_input <- years_input[years_input < input$btn_year_ppi & years_input > 1990]
    
    selectInput(inputId = "btn_start_year_ppi",
                label = 'Start year',
                choices = c("", start_year_input)#,
                # selected = '2014'
    )
  }
})

# -- Commodity button ----
output$btn_commodity_ppi <- renderUI({
  
  # Country and year buttons required
  req(input$btn_country_ppi, input$btn_year_ppi, input$btn_start_year_ppi)
  
  input$btn_country_ppi
  
  sel_country <- country_input[country_input$label == input$btn_country_ppi, code]
  
  if(sel_country != "" & input$btn_year_ppi != "" & input$btn_start_year_ppi != "" & nrow(ppiseries$country_data) > 0) {
    
    sel_years <- input$btn_start_year_ppi:input$btn_year_ppi
    
    seriesAvailable <- ppiseries$country_data
    
    
    seriesAvailable <- expandYear(seriesAvailable, 
                                  newYears = max(as.numeric(seriesAvailable$timePointYears)))
    
    commlist <- unique(seriesAvailable[!is.na(measuredItemCPC_description), .(measuredItemCPC_description, measuredItemCPC)])
    
    comm_input <- cpc_list[code %in% commlist$measuredItemCPC]
    
    selectInput(inputId = "btn_commodity_ppi",
                label = 'Commodity',
                choices = c("", "All", comm_groups$name_en_l3, comm_input$label), 
                multiple = TRUE
    )
  }
})


ppiseries <- reactiveValues(country_data = data.table())

# -- Load data by country ---- 

observeEvent(input$btn_start_year_ppi,{
  # req(input$btn_country_ppi, input$btn_year_ppi)
  
  input$btn_country_ppi
  
  if(is.null(input$btn_country_ppi) | 
     is.null(input$btn_year_ppi) | is.null(input$btn_start_year_ppi)) return(NULL)
  
  sel_country <- country_input[country_input$label == input$btn_country_ppi, code]
  
  if(!sel_country %in% unique(ppiseries$country_data$geographicAreaM49)){
    
    if(length(sel_country) == 0) return(NULL)
    
    if( input$btn_start_year_ppi != ''){
      sel_country <- country_input[country_input$label == input$btn_country_ppi, code]
      #sel_years <- input$btn_start_year_ppi:input$btn_year_ppi
      
      #  prep_price_slc <- series2est$country_data[geographicAreaM49 == sel_country &
      #   measuredItemCPC == sel_commodity &
      #                                       measuredElement == '5531']
      withProgress(message = 'Loading country data', value = 0.1, {
        
        # if(startyearevaluation < min(startYear)){
        
        if(input$btn_dataset_ppi == 'Preparation'){
          
          dataset_ppi <- 'annual_producer_prices_prep'
          
        } else if (input$btn_dataset_ppi == 'Validated'){
          
          dataset_ppi <- 'annual_producer_prices_validation'
        }
        
        
        els <- GetCodeList(domainPP, dataset_ppi, 'measuredElement')
        PPIels <- els[grepl('5539', els$code)]$code
        
        priceKey = DatasetKey(
          domain = domainPP,
          dataset = dataset_ppi,
          dimensions = list(
            Dimension(name = "geographicAreaM49",
                      keys = GetCodeList(domainPP, dataset_ppi, 'geographicAreaM49')[code %in% sel_country, code]),
            Dimension(name = "measuredElement",
                      keys = GetCodeList(domainPP, dataset_ppi, 'measuredElement')[code %in% PPIels, code]),
            Dimension(name = "measuredItemCPC",
                      keys = GetCodeList(domainPP, dataset_ppi, 'measuredItemCPC')[, code]),
            Dimension(name = "timePointYears",
                      keys = GetCodeList(domainPP, dataset_ppi, 'timePointYears')[code <= input$btn_year_ppi, code])))
        
        Sys.sleep(0.1)
        incProgress(0.1)
        
        ppiData <- GetData(
          priceKey,
          flags = TRUE)
        
        Sys.sleep(0.1)
        incProgress(0.6)
        
        ppiData <- nameData(domainPP, dataset_ppi, ppiData)
        ppiData[,timePointYears_description := NULL]
        
        ppiseries$country_data <- ppiData
        
        
        Sys.sleep(0.9)
        incProgress(0.9)
        
      })
      
    }
  }
  
})


output$gg_plot_ppi <- renderPlotly({
  
  req(input$btn_year_ppi, input$btn_start_year_ppi, input$btn_country_ppi, input$btn_commodity_ppi)
  
  input$btn_country_ppi
  
  input$btn_year_ppi
  
  input$btn_start_year_ppi
  
  if(is.null(input$btn_country_ppi) |
     is.null(input$btn_year_ppi) | is.null(input$btn_start_year_ppi) |
     is.null(input$btn_commodity_ppi) ) return(NULL)
  
  if(input$btn_commodity_ppi != '' &  input$btn_country_ppi != '') {
    

    country <- gsub('.*- ','', input$btn_country_ppi)
    
    group <- gsub('.*- ','', input$btn_commodity_ppi)
    
    if(all(group != 'All') & group %in% comm_groups$name_en_l3){

      product <- c(cpchier[name_en_l3 %in% group,]$code_l4, 
                   cpchier[name_en_l3 %in% group,]$code_l5, 
                   cpchier[name_en_l3 %in% group,]$code_l6)
      product <- c(product[!is.na(product)], unique(PPIaggr[var_code_sws %in% product & var_group_code_sws != 'F2051']$var_group_code_sws))
     
    } else if(any(group == 'All')){
      product <- unique(ppiseries$country_data[geographicAreaM49 == country]$measuredItemCPC)
    } else {
      product <- group # gsub('.*- ','', input$btn_commodity_ppi)
    }
    
    
    #product <- substr(product,1,nchar(product)-7)
    sel_years <- input$btn_start_year_ppi:input$btn_year_ppi
    
    #year <- year_out#year <- gsub('.*[(]','', input$btn_outlier)
    #year <- gsub('[)]','', year)
    
    ppPrep <- ppiseries$country_data[geographicAreaM49 == country  &
                       measuredItemCPC %in% product ]
    ppPrep[ , Flag := paste(flagObservationStatus, flagMethod, sep = ';')]
    
   
    miny <- min(ppPrep$Value) - 0.1*min(ppPrep$Value)
    maxy <- max(ppPrep$Value) + 0.1*max(ppPrep$Value)
    ppPrep$Value <- round(ppPrep$Value, 2)
    
    ppPrep[ , measuredItemCPC_description_sub := ifelse(nchar(measuredItemCPC_description) < 15L, 
                                                        measuredItemCPC_description,
      paste(str_sub(measuredItemCPC_description, end = 9L), measuredItemCPC, sep = '-'))]
    
    #  ppPrep$timePointYears <- base::as.Date(as.integer(ppPrep$timePointYears), format = '%Y')
    ggplotly(ggplot(ppPrep[timePointYears %in% sel_years], aes(x = timePointYears, 
                                                y = Value, label= Flag))+
               geom_line(group = 1, col = "grey31") +
               geom_point()+#size = 2)+
              # scale_colour_manual(values = c("FALSE" = "grey31", "TRUE" = "coral"))+
               ylim(miny, maxy)+
               facet_wrap(~ measuredItemCPC_description_sub)+
               theme(legend.position="none",
                     strip.text.x = element_text(margin = margin(-1, 0, 0, 0)),
                     axis.text.x=element_blank())
               # scale_x_date(date_labels = "%Y")+
               #ggtitle(paste('Series in ', chosencurrEl,':', country, ' - ',product))
             )
    
    
    #  ggplotly(aa)
    
  }
  
})


