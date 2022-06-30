output$btn_commodity_compare <- renderUI({
  
  # Country and year buttons required
  req(input$btn_country_compare)
  
  input$btn_country_compare
  
  sel_country <- country_input[country_input$label == input$btn_country_compare, code]
  
  if(sel_country != "") {
    
    selectInput(inputId = "btn_commodity_compare",
                label = 'Commodity',
                choices = c("", "All", comm_groups$name_en_l3), 
                multiple = TRUE
    )
  }
})





output$comparison_plot <- renderPlotly({

  req(input$btn_country_compare, input$btn_commodity_compare)
  
  input$btn_country_compare
  
  inFile <- input$comparison 
  
  if (is.null(inFile))
    return(NULL)
  
  # req(input$updatedSUA)
  
  tryCatch(
    {
      df <- read.table(inFile$datapath, header = TRUE)
      #colClasses = "character"#input$header #,
      #   sep = input$sep,
      #  quote = input$quote
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  
  comparison <- as.data.table(df)
  
  sel_country <- country_input[country_input$label == input$btn_country_compare, code]
  validate(need(comparison[geographicAreaM49 == sel_country,.N] > 0,
           'No data for this country. Choose another country included in the comparison text file.'))

  if(input$btn_commodity_compare != '') {
    
    group <- gsub('.*- ','', input$btn_commodity_compare)
    
    if(all(group != 'All') & group %in% comm_groups$name_en_l3){
      
      product <- c(cpchier[name_en_l3 %in% group,]$code_l4, 
                   cpchier[name_en_l3 %in% group,]$code_l5, 
                   cpchier[name_en_l3 %in% group,]$code_l6)
      product <- c(product[!is.na(product)], unique(PPIaggr[var_code_sws %in% product & var_group_code_sws != 'F2051']$var_group_code_sws))
      
    } else if(any(group == 'All')){
      product <- unique(comparison$measuredItemCPC)
    } else {
      product <- group # gsub('.*- ','', input$btn_commodity_ppi)
    }
  }
  
  comparison <- comparison[geographicAreaM49 == sel_country]
  
  
  comparison2plot <- melt(comparison, measure.vars = c('ValueNew', 'ValueOld'))
  
  ggplotly(ggplot(comparison2plot[measuredItemCPC %in% product], aes(x = timePointYears, y = value, col = variable))+
             geom_point()+
             facet_wrap(~ measuredItemCPC, scales = 'free_y'))
  
  
  })