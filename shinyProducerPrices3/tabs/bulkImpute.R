observeEvent(input$btn_accept_bulk,{
  
  country <- gsub(' -.*','', input$btn_country_bulk)
  
  if(country != 'All' & length(country) == 1){
    
    countrycode <- M49[description %in% country]$code
    where2read <- paste("geographicaream49 = '", countrycode, "'", sep = '')
    pp2Rev <- ReadDatatable('imputation_annual_prices',  where = where2read, readOnly = F)
    
  } else if(country != 'All' & length(country) > 1) { 
    countrycode <- M49[description %in% country]$code
    where2read <- paste("geographicaream49 IN ('", paste(countrycode,collapse="', '"), "')", sep = '')
    pp2Rev <- ReadDatatable('imputation_annual_prices',  where = where2read, readOnly = F) 
    
  } else {
    
    countrycode <- M49$code
    pp2Rev <- ReadDatatable('imputation_annual_prices', readOnly = F)
    
  }
  
  group <- gsub('.*- ','', input$btn_comm_group)
  
  if(group != 'All'){
    product <- c(cpchier[name_en_l3 %in% group,]$code_l4, 
                 cpchier[name_en_l3 %in% group,]$code_l5, 
                 cpchier[name_en_l3 %in% group,]$code_l6)
  } else {
    product <- unique(pp2Rev[geographicaream49 %in% countrycode]$measureditemcpc)
  }
  
  prodcode <- product[!is.na(product)]
  
  pp2Rev <- pp2Rev[geographicaream49 %in% countrycode &
                     measureditemcpc %in%  prodcode ]
  
  approachsel <- input$btn_method_bulk
  
  pp2Rev[, selected := FALSE]
  pp2Rev[approach == approachsel, selected := TRUE]

  changeset <- Changeset('imputation_annual_prices')
  AddModifications(changeset, pp2Rev)
  Finalize(changeset)
  
  showModal(modalDialog(
    title = "Data saved!" ,
    sprintf("The choice has been saved into the SWS datatable. 
            Once all data have been validated, the plugin to save imputation to the dataset can be run.")
  ))
  
})