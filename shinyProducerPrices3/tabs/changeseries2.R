# -- End year button ----
output$btn_year  <- renderUI({
  
  # Country button required
  req(input$btn_country)
  
  input$btn_country
  
  sel_country <- country_input[country_input$label == input$btn_country, code]
  
  if(sel_country != "") {
    
    # Input details
    selectInput(inputId = "btn_year",
                label = 'End year',
                choices = c("", years_input)#,
                #   selected = '2016'
    )
  }
})

# -- Start year button ----
# Same as year button but must be lower than btn_year
output$btn_start_year <- renderUI({
  
  # Country and year buttons required
  req(input$btn_country, input$btn_year)
  
  input$btn_country
  
  sel_country <- country_input[country_input$label == input$btn_country, code]
  
  if(sel_country != "" & input$btn_year != "") {
    
    start_year_input <- years_input[years_input < input$btn_year & years_input > 1991]
    
    selectInput(inputId = "btn_start_year",
                label = 'Start year',
                choices = c("", start_year_input)#,
                # selected = '2014'
    )
  }
})

# -- Commodity button ----
output$btn_commodity <- renderUI({
  
  # Country and year buttons required
  req(input$btn_country, input$btn_year, input$btn_start_year)
  
  input$btn_country
  
  sel_country <- country_input[country_input$label == input$btn_country, code]
  
  if(sel_country != "" & input$btn_year != "" & input$btn_start_year != "" & nrow(series2est$country_data) > 0) {
    
    sel_years <- input$btn_start_year:input$btn_year
    
    seriesAvailable <- series2est$country_data[geographicAreaM49 == sel_country &
                                                 measuredElement == '5531', ]
    
    
    seriesAvailable <- expandYear(seriesAvailable, 
                                  newYears = max(as.numeric(seriesAvailable$timePointYears)))
    
    commlist <- unique(seriesAvailable[!is.na(measuredItemCPC_description), .(measuredItemCPC_description, measuredItemCPC)])
    
    comm_input <- cpc_list[code %in% commlist$measuredItemCPC]
    
    selectInput(inputId = "btn_commodity",
                label = 'Commodity',
                choices = c("", comm_input$label)
    )
  }
})

series2est <- reactiveValues(country_data = data.table(),
                             metadata = data.table(),
                             slc_series = data.table(),
                             macro_ind = data.table(),
                             yield = data.table(),
                             tcf = data.table())

# -- Load data by country ---- 

observeEvent(input$btn_start_year,{
  req(input$btn_country, input$btn_year)
  
  input$btn_country
  
  if(is.null(input$btn_country) | 
     is.null(input$btn_year) | is.null(input$btn_start_year)) return(NULL)
  
  sel_country <- country_input[country_input$label == input$btn_country, code]
  
  if(!sel_country %in% unique(series2est$country_data$geographicAreaM49)){
    
    if(length(sel_country) == 0) return(NULL)
    
    if( input$btn_start_year != ''){
      sel_country <- country_input[country_input$label == input$btn_country, code]
      sel_years <- input$btn_start_year:input$btn_year
      startyearevaluation <- as.character(1991)#as.character(as.numeric(input$btn_start_year)-10)
      
      #  prep_price_slc <- series2est$country_data[geographicAreaM49 == sel_country &
      #   measuredItemCPC == sel_commodity &
      #                                       measuredElement == '5531']
      withProgress(message = 'Loading country data', value = 0.1, {
        
        period_selected <- as.character(startyearevaluation)#:input$btn_year)
        # if(startyearevaluation < min(startYear)){
        
        if(maxYear < max(sel_years)){
        
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
                      keys = GetCodeList(domainPP, datasetPrep, 'timePointYears')[code > maxYear & code <= max(sel_years), code])))
        
        Sys.sleep(0.1)
        incProgress(0.1)
        priceData <- GetData(
          priceKey,
          flags = TRUE)
        
        Sys.sleep(0.1)
        incProgress(0.6)
        
        priceData <- nameData(domainPP, datasetPrep, priceData)
        priceData[,timePointYears_description := NULL]
        
        priceData <- rbind(priceDataPast, priceData)
        } else {
          priceData <- priceDataPast[timePointYears <= max(sel_years)]
        }
        priceVal <- priceData[timePointYears > period_selected]#%in% period_selected]
        
        dataPP$validated <- priceVal
        
        dataPP$prepUSD <- priceVal[measuredElement == '5532' ]
        dataPP$prepSLC <- priceVal[measuredElement == '5531' ]
        dataPP$prepLCU <- priceVal[measuredElement == '5530' ]
        
        price2val <-copy(priceVal)  #nameData(domainPP, datasetPrep, priceVal)
        
        series2est$country_data <- price2val[geographicAreaM49 == sel_country]
        
        prep_price_slc <- price2val[geographicAreaM49 == sel_country &
                                      measuredElement == '5531']
        
        
        #}
        
        series2est$slc_series <- prep_price_slc
        
        Sys.sleep(0.1)
        incProgress(0.1)
        
        # Macro indicators
        GDP_VAcode <- c('8005', '8028')
        
        MIKey = DatasetKey(
          domain = 'macro_stats',
          dataset = 'ess_eco_macroind_complete',
          dimensions = list(
            Dimension(name = "geographicAreaM49",
                      keys = GetCodeList('macro_stats', 
                                         'ess_eco_macroind_complete', 
                                         'geographicAreaM49')[code == sel_country, code]),
            Dimension(name = "measuredElementGdp", 
                      keys = GetCodeList('macro_stats', 
                                         'ess_eco_macroind_complete', 
                                         'measuredElementGdp')[code %in% GDP_VAcode, code]),
            Dimension(name = "timePointYears", 
                      keys = GetCodeList('macro_stats', 
                                         'ess_eco_macroind_complete', 
                                         'timePointYears')[code > period_selected, code])) # %in% period_selected, code]))
        )
        
        # Yield
        yieldcode <- '5421'
        sel_country_yield <- ifelse(any(sel_country %in% '156'),'1248', sel_country)
        yieldKey = DatasetKey(
          domain = 'agriculture',
          dataset = 'aproduction',
          dimensions = list(
            Dimension(name = "geographicAreaM49",
                      keys = GetCodeList('agriculture', 
                                         'aproduction', 
                                         'geographicAreaM49')[code == sel_country_yield, code]),
            Dimension(name = "measuredElement", 
                      keys = GetCodeList('agriculture', 
                                         'aproduction', 
                                         'measuredElement')[code == yieldcode, code]),
            Dimension(name = "measuredItemCPC",
                      keys = GetCodeList('agriculture', 'aproduction', 'measuredItemCPC')[, code]),
            Dimension(name = "timePointYears", 
                      keys = GetCodeList('agriculture', 
                                         'aproduction', 
                                         'timePointYears')[code > period_selected, code]))#%in% period_selected, code]))
        )
        
        # Macro Indicators
        macro_ind <- GetData(MIKey, flags = TRUE)
        
        macro_ind <- dcast(macro_ind, geographicAreaM49 + timePointYears ~ measuredElementGdp, 
                           value.var = 'Value' )
        
        setnames(macro_ind, c('8005', '8028'), c('ValueGDP', 'ValueVA'))
        
        Sys.sleep(0.3)
        incProgress(0.3)
        # Yield
        yield <- GetData(yieldKey, flags = TRUE)
        if(yield[geographicAreaM49 == '156',.N] == 0){
          yield[geographicAreaM49 == '1248', geographicAreaM49 := '156']
          
        }
        
        Sys.sleep(0.5)
        incProgress(0.5)
        
        series2est$macro_ind <- macro_ind
        series2est$yield <- yield
        
        tcf <- ReadDatatable('pp_tcf')
        setnames(tcf, c("country_code", "cpc2convert"), 
                 c('geographicAreaM49', 'measuredItemCPC'))
        
        series2est$tcf <- tcf
        
        Sys.sleep(0.7)
        incProgress(0.7)
        
        cpiKey = DatasetKey(
          domain = domainPP,
          dataset = 'consumer_price_indices',
          dimensions = list(
            Dimension(name = "geographicAreaM49",
                      keys = GetCodeList(domainPP, 'consumer_price_indices', 'geographicAreaM49')[code == sel_country, code]),
            Dimension(name = "measuredElement", 
                      keys = GetCodeList(domainPP, 'consumer_price_indices', 'measuredElement')[code %in% c('23012', '23013'), code]),
            Dimension(name = "timePointYears",
                      keys = GetCodeList(domainPP, 'consumer_price_indices', 'timePointYears')[code > period_selected, code]),# %in% period_selected, code]),
            Dimension(name = "timePointMonths", 
                      keys = GetCodeList(domainPP, 'consumer_price_indices', 'timePointMonths')[code == '7013', code]))
        )
        
        cpi0 <- GetData(cpiKey, flags = TRUE)
        
        cpi0[, timePointMonths := NULL]
        
        series2est$cpi <- cpi0
        
        
        Sys.sleep(0.9)
        incProgress(0.9)
        
      })
      
    }
  }
  
})

# -- Approach choice ----
output$btn_sel_approach <- renderUI({
  
  req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity)
  # Country and year buttons required
  input$btn_country
  
  if(is.null(input$btn_country) | 
     is.null(input$btn_year) | is.null(input$btn_start_year) |
     is.null(input$btn_commodity) ) return(NULL)
  
  sel_country <- country_input[country_input$label == input$btn_country, code]
  
  if(length(sel_country) == 0) return(NULL)
  
  #if(is.null(seriesEstimation())) return(NULL)
  
  imp2save <- seriesEstimation()
  sel_country <- country_input[country_input$label == input$btn_country, code]
  
  if(unique(imp2save$geographicAreaM49) != sel_country) return(NULL)
  
  #  if(nrow(imp2save) >0){
  approach_input <- as.character(unique(imp2save$Approach))
  # } else {
  #  approach_input <- 'Original series'
  #  }
  
  #c('ARIMAX',
  # 'Ensemble',
  # 'LM',
  # 'Comm_Group',
  # 'CPI',
  # 'Price Ratio')
  # selectInput
  checkboxGroupInput(inputId = "btn_sel_approach",
                     label = 'Approach to visualize',
                     choices = c('All', approach_input),
                     selected = 'All')
  
})

#-- Plot ----
seriesEstimation <- reactive({
  
  req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity)
  
  input$btn_country
  
  if(is.null(input$btn_country) |
     is.null(input$btn_year) | is.null(input$btn_start_year) |
     is.null(input$btn_commodity) ) return(NULL)
  
  sel_country <- country_input[country_input$label == input$btn_country, code]
  
  if(length(sel_country) == 0) return(NULL)
  
  sel_country <- country_input[country_input$label == input$btn_country, code]
  sel_years <- input$btn_start_year:input$btn_year
  startyearevaluation <- as.character(min(series2est$country_data$timePointYears))#as.numeric(input$btn_start_year)-10)
  sel_commodity <- cpc_list[label == input$btn_commodity, code]
  
  if(length(sel_country)>0){
    if(sel_country != '' & sel_commodity != ''){
      #prep_price_slc <- series2est$slc_series
      
      prep_price_slc <- series2est$country_data[geographicAreaM49 == sel_country &
                                                  measuredItemCPC == sel_commodity &
                                                  measuredElement == '5531']
      
    
      seriesresults <- classifySeries(prep_price0 = prep_price_slc, 
                                      minyear = min(as.numeric(sel_years)), 
                                      maxyear = max(as.numeric(sel_years)))
      
      prep_price_exp <- seriesresults$completedSeries
      
      series2forecastComplete <- seriesresults$series2forecast
      
  ###    prep_price_exp <- expandYear(prep_price_slc, #[timePointYears %in% sel_years], 
  ###                                 newYears = as.numeric(input$btn_year))#max(as.numeric(prep_price_slc$timePointYears)))
# 
# 
#       prep_price_slc <- prep_price_exp #rbind(prep_price_exp, prep_price0[!timePointYears %in% sel_years])
# 
#       # Re-impute all previous imputations
#       prep_price_slc[timePointYears %in% sel_years & !flagObservationStatus %in% protectedFlags , c('Value',
#                                                                                                 'flagObservationStatus',
#                                                                                                 'flagMethod') :=list(NA, 'M', 'u')]
# 
#       # If no official or imputed data before the 'M;u' then the series is not imputed
#       # If one year not in the selected ones is missing the system ignores them
#       setkey(prep_price_slc)
# 
#       prep_price_slc[, minRepY := min(as.numeric(timePointYears), na.rm = T)]
# 
#       todelete <- prep_price_slc[is.na(Value) & timePointYears < minRepY]
# 
#       prep_price_slc <- prep_price_slc[!todelete, on = c("geographicAreaM49", "measuredItemCPC", "timePointYears")]
# 
#       prep_price_slc[, minRepY := NULL]

      
      # prep_price_slc <- rbind(prep_price_exp[timePointYears %in% sel_years], prep_price_slc[!timePointYears %in% sel_years])
      
    #  validate(need(prep_price_slc[timePointYears %in% sel_years & !flagObservationStatus %in% protectedFlags,.N] > 0,
     #               "All data in this series are official."))
      
      
      withProgress(message = 'Re-estimating series', value = 0.1, {
        
        # -- Auxiliary variable ----
        
        macro_ind <- series2est$macro_ind
        
        yield <- series2est$yield
        
        # GDP <- macro_ind[,.(geographicAreaM49, timePointYears, ValueGDP)]
        # VA <- macro_ind[,.(geographicAreaM49, timePointYears, ValueVA)]
        # 
        # GDP[!is.na(ValueGDP) , LogGDP := log(ValueGDP)]
        # VA[!is.na(ValueVA) , LogVA := log(ValueVA)]
        
        GDPapproach <- copy(macro_ind)
        GDPapproach[!is.na(ValueVA), GDP2use := ValueVA]
        GDPapproach[is.na(ValueVA), GDP2use := ValueGDP]
        GDPapproach[,c('ValueGDP', 'ValueVA')] <- NULL
        
        prep12 <- merge(prep_price_exp, macro_ind, by = c("geographicAreaM49","timePointYears"),
                        all.x = T)
        
        prep12[!is.na(ValueGDP) , LogGDP := log(ValueGDP)]
        prep12[!is.na(ValueVA) , LogVA := log(ValueVA)]
        
        
        if(nrow(yield) > 0){
          yield1 <- copy(yield)
          setnames(yield1, 'Value', 'ValueYield')
          yield1[, c("measuredElement", "flagObservationStatus", "flagMethod")] <- NULL
          prep123 <- merge(prep12, yield1, 
                           by = c("geographicAreaM49", 
                                  "timePointYears",
                                  "measuredItemCPC"), all.x = T)
        } else {
          prep123 <- prep12[,ValueYield:= NA]
        }
        # TOI
        country2sel <- paste("geographicaream49 = '", sel_country, "'", sep = '')
        toi <- ReadDatatable('toi_data', where = country2sel)
        toi <- toi[timepointyears >= startyearevaluation]
        
        prepcov <- merge(prep123, toi, by.x = c("geographicAreaM49", 
                                                "timePointYears"),
                         by.y = c("geographicaream49", 
                                  "timepointyears"), all.x = T)
        
        # -- Missing value ----
        prepcov <- prepcov[order(timePointYears)]
        prepcov[!is.na(Value), LogValue := log(Value)]
        
        Sys.sleep(0.1)
        incProgress(0.1) 
        # -- Applying Price ratios (Value_PR) ----
        
        tcf <- series2est$tcf
        
        prepcov <- merge(prepcov, tcf[,c('geographicAreaM49', 'measuredItemCPC',
                                         "cpc_reference", 'tcf'), with = F],
                         by = c('geographicAreaM49', 'measuredItemCPC'), all.x = T)
        
        tcfneed <- prepcov[!is.na(tcf) & timePointYears %in% sel_years]
        refneed <- unique(tcfneed[,.(geographicAreaM49, cpc_reference, timePointYears)])
        
        reference_comm <- dataPP$prepSLC[geographicAreaM49 == sel_country &
                                           timePointYears %in% sel_years &
                                           measuredItemCPC == tcfneed$cpc_reference]
        
        refneed <- merge(refneed, reference_comm[,.(geographicAreaM49,
                                                    measuredItemCPC,
                                                    timePointYears, Value)], 
                         by.x = c('geographicAreaM49', 'cpc_reference', 'timePointYears'), 
                         by.y = c('geographicAreaM49', 'measuredItemCPC', 'timePointYears'),
                         all.x = T)
        refneed <- refneed[!is.na(Value)]
        convprices <- merge(prepcov, refneed, 
                            by =  c('geographicAreaM49', 'cpc_reference', 'timePointYears'),
                            all.x = T, suffixes = c('', '_ref'))
        
        # convprices[is.na(Value) & !is.na(tcf) & !is.na(Value_ref) & flagObservationStatus == 'M' & flagMethod == 'u', 
        #            c('Value_pr', 'flagObservationStatus', 'flagMethod',
        #              'LogValue') := list(tcf*Value_ref, 'I', 'i', log(tcf*Value_ref))]
        
        convprices[, PriceRatio := NA]
        
        convprices[!is.na(tcf) & 
                     !is.na(Value_ref),
                   PriceRatio := tcf*Value_ref]
        
        names(convprices)
        convprices[,c("cpc_reference", "tcf", "Value_ref")] <- NULL
        
        # Dataset with covariates ready to be manipulated
        prepcov <- copy(convprices)
        Sys.sleep(0.1)
        incProgress(0.2)
        prepcov[timePointYears %in% sel_years & !flagObservationStatus %in% protectedFlags, Value := NA]
        # Series for lm model: numeric year and series until btn_start_year
        series_comm_lm <- copy(prepcov)
        series_comm <- copy(prepcov)
  
        arimaxImputation <- arimaxRevision(series_comm = series_comm, series2forecastComplete = series2forecastComplete, 
                       minyear = min(as.numeric(sel_years)), maxyear = max(as.numeric(sel_years)))
        
        
        lmImputation <- linearRevision(series_comm_lm = series_comm_lm, minyear = min(as.numeric(sel_years)), 
                                       maxyear = max(as.numeric(sel_years)),
                                       series2forecastComplete = series2forecastComplete)
        
        Sys.sleep(0.1)
        incProgress(0.4)
        print('Linear ok')
        # -- Ensemble approach ----
        
        impPar <- defaultImputationParameters()
        impPar$imputationValueColumn="Value"
        impPar$imputationFlagColumn="flagObservationStatus"
        impPar$imputationMethodColumn="flagMethod"
        impPar$byKey=c("geographicAreaM49", "measuredItemCPC")
        impPar$estimateNoData=FALSE
        
        # If the data series contains only zero and missing value then it is considered to contain no information for imputation.
        
        series_ensemble <- prepcov[geographicAreaM49 == sel_country &
                                     measuredItemCPC == sel_commodity &
                                     measuredElement == '5531']
        
        series_ensemble <- series_ensemble[timePointYears %in% sel_years &
                                             !flagObservationStatus %in% protectedFlags, 
                                           c('Value',
                                             'flagObservationStatus',
                                             'flagMethod') := list(NA, 'M', 'u')]
        
        pp_ensemble_sub <- series_ensemble[,c("geographicAreaM49",
                                              "timePointYears",
                                              "measuredItemCPC",
                                              "Value",
                                              "flagObservationStatus",
                                              "flagMethod"), with = F]
        
        pp_ensemble_sub <- pp_ensemble_sub[order(timePointYears)]
        # If no missing data the commodityDB does not change
        
        pp_ensemble_imp <- imputeVariable(data = pp_ensemble_sub,
                                          imputationParameters = impPar)
        
        
        predEns <- pp_ensemble_imp$Value
        
        Sys.sleep(0.1)
        incProgress(0.5)
        print('Ensemble ok')
        # -- Commodity group information ----
        
        cpchierarchy <- cpchier
        
        hierarchyComm <- cpchierarchy[apply(cpchierarchy, 1, function(r) any(r %in% sel_commodity))]
        
        colComm <-  names(hierarchyComm)[
          names(hierarchyComm)== 
            names(hierarchyComm)[(apply(cpchierarchy, 2, 
                                        function(r) any(r %in% sel_commodity)))]]
        
        if(colComm == "code_l4"){
          hier1 <- unique(hierarchyComm[ , "code_l3", with = F]) 
          codes2compare <- c(cpchierarchy[code_l3 == hier1$code_l3 & !is.na(code_l4), ]$code_l4,
                             cpchierarchy[code_l3 == hier1$code_l3 & !is.na(code_l5), ]$code_l5,
                             cpchierarchy[code_l3 == hier1$code_l3 & !is.na(code_l6), ]$code_l6)
        } else {
          hier1 <- unique(hierarchyComm[ , "code_l4", with = F])
          codes2compare <- c(cpchierarchy[code_l4 == hier1$code_l4 & !is.na(code_l5), ]$code_l5,
                             cpchierarchy[code_l4 == hier1$code_l4 & !is.na(code_l6), ]$code_l6)
        }
        
        commoditygroup <- dataPP$prepSLC[!is.na(Value) &
                                           measuredItemCPC %in% codes2compare]
        
        # If there are data ok otherwise hierarchy up (if code_l4)
        if(nrow(commoditygroup[timePointYears %in% sel_years]) == 0
           & names(hier1) == 'code_l4'){
          
          hier1 <- unique(hierarchyComm[ , "code_l3", with = F]) 
          codes2compare <- c(cpchierarchy[code_l3 == hier1$code_l3 & !is.na(code_l4), ]$code_l4,
                             cpchierarchy[code_l3 == hier1$code_l3 & !is.na(code_l5), ]$code_l5,
                             cpchierarchy[code_l3 == hier1$code_l3 & !is.na(code_l6), ]$code_l6)
          
          commoditygroup <- dataPP$prepSLC[!is.na(Value) &
                                             measuredItemCPC %in% codes2compare]
          
        }else if(nrow(commoditygroup[timePointYears %in% 
                                     sel_years]) == 0
                 & names(hier1) == 'code_l3'){
          print('Commodity group approch not applicable') #Should be an NA in the final file just to have all results 
        }
        
        # calculate growth rate and the mean or median applied to product
        commoditygroup$timePointYears <- as.numeric(commoditygroup$timePointYears)
        commoditygroup <- commoditygroup[order(timePointYears)]
        # commoditygroup[, cgGR := diff(Value)/shift(Value)]
        
        commoditygroup[ , cgGR := c(NA, diff(Value))/shift(Value),
                        by = c("geographicAreaM49",
                               "measuredElement",
                               "measuredItemCPC")]
        
        commoditygroup[ , c('meanbyyear',
                            'medianbyyear'):= list(mean(cgGR, na.rm = T),
                                                   median(cgGR, na.rm = T)),
                        by = 'timePointYears']
        
        commoditygroup2merge <- unique(commoditygroup[,.(timePointYears, medianbyyear)])
        commoditygroup2merge$timePointYears <- as.character(commoditygroup2merge$timePointYears)
        # commoditygroup2merge[timePointYears == '2018', medianbyyear := 0.2]
        
        years <- sel_years
        cgmethod <- copy(prepcov)
        
        cgmethod <- merge(cgmethod, commoditygroup2merge,
                          by = 'timePointYears', all.x = T)
        cgmethod <- cgmethod[order(timePointYears)]
        
        cgmethod[!timePointYears %in% sel_years, ValueCG := Value]
        cgmethod[flagObservationStatus %in% protectedFlags, Value := exp(LogValue)]
        
        for(i in 2:nrow(cgmethod)){
          newValue <- (1 + cgmethod[i,]$medianbyyear) * cgmethod[i-1]$Value
          cgmethod[i , ValueCG := newValue]
          cgmethod[i & timePointYears %in% sel_years, Value := ValueCG]  
        }
        
        cgmethod[timePointYears %in% sel_years, Value := ValueCG]               
        cgmethod[flagObservationStatus %in% protectedFlags &
                   timePointYears %in% sel_years, Value := exp(LogValue)]
        
        Sys.sleep(0.1)
        incProgress(0.6)
        print('Comm group ok')
        # -- CPI ----
        
        cpi0 <- series2est$cpi
        if(cpi0[,.N] > 0){
          cpi0 <- cpi0[order(as.numeric(timePointYears))]
          cpi0[ , GR := c(NA, diff(Value))/shift(Value),
                by = c('geographicAreaM49', 'measuredElement')]
          cpi <- dcast(cpi0, geographicAreaM49 + timePointYears ~ measuredElement, value.var = 'GR')
          setnames(cpi, c("23012",  "23013"), c("GeneralCPI", "FoodCPI"))
          
          cpi[!is.na(FoodCPI) , GR2use := FoodCPI]
          cpi[is.na(FoodCPI) , GR2use := GeneralCPI]
          # cpi <- cpi[timePointYears != min(sel_years)]
          
          # 2014 CPI data???
          
          cpimethod <- copy(prepcov)
          
          
          cpimethod <- merge(cpimethod, cpi[,.(geographicAreaM49, 
                                               timePointYears,
                                               GR2use)],
                             by = c('geographicAreaM49', 'timePointYears'), all.x = T)
          
          cpimethod[order(timePointYears)]
          
          cpimethod[!timePointYears %in% sel_years, ValueCPI := Value]
          
          for(i in 2:nrow(cpimethod)){
            newValue <- (1 + cpimethod[i,]$GR2use) * cpimethod[i-1]$Value
            cpimethod[i , ValueCPI := newValue]
            cpimethod[i & timePointYears %in% sel_years, Value := ValueCPI]  
          }
          
          cpimethod[timePointYears %in% sel_years, Value := ValueCPI]               
          cpimethod[flagObservationStatus %in% protectedFlags &
                      timePointYears %in% sel_years, Value := exp(LogValue)]
          
        } else {
          
          cpimethod <- data.table(Value = NA)
        }
        
        Sys.sleep(0.1)
        incProgress(0.7)
        print('CPI ok')
        
        # -- GDP or AgGDP ----
        gdpmethod <- copy(prepcov)
        GDPapproach[ , GRgdp := c(NA, diff(GDP2use))/shift(GDP2use)]
        
        gdpmethod <- merge(gdpmethod, GDPapproach, 
                           by = c('geographicAreaM49', 'timePointYears'), 
                           all.x = T)
        
        gdpmethod[order(timePointYears)]
        
        gdpmethod[!timePointYears %in% sel_years, ValueGDPdefl := Value]
        
        for(i in 2:nrow(gdpmethod)){
          newValue <- (1 + gdpmethod[i,]$GRgdp) * gdpmethod[i-1]$Value
          gdpmethod[i , ValueGDPdefl := newValue]
          gdpmethod[i & timePointYears %in% sel_years, Value := ValueGDPdefl]  
          
        }
        
        gdpmethod[timePointYears %in% sel_years, Value := ValueGDPdefl]               
        gdpmethod[flagObservationStatus %in% protectedFlags &
                    timePointYears %in% sel_years, Value := exp(LogValue)]
        
        #gdpmethod[flagMethod == 'q', ValueGDPdefl := Value]
        #gdpmethod[timePointYears %in% sel_years, Value := ValueGDPdefl]  
        print('GDP ok')
        Sys.sleep(0.1)
        incProgress(0.9)
        
      })
      
      # -- End of imputation methods ----
      
      newimputation <- prepcov[, unique(c(names(prep_price_slc), 'PriceRatio')), with = F]
      
      newimputation[ , c('ARIMAX',
                         'Ensemble',
                         'LM',
                         'Comm_Group',
                         'CPI',
                         'GDP') 
                     := list(arimaxImputation$Value,  #c(exp(as.vector(ppseries[!is.na(ppseries) ])), # prep_price_slc[!timePointYears %in% sel_years & timePointYears < min(missingyear)][order(timePointYears)]$Value, 
                             #   exp(pred), 
                             #   prep_price_slc[!timePointYears %in% sel_years & 
                             #                   timePointYears > max(missingyear)][order(timePointYears)]$Value
                             # ), 
                             #c(newimputation[!timePointYears %in% sel_years,]$Value, exp(pred)),
                             predEns,
                             lmImputation,
                             # c(exp(as.vector(ppseries[!is.na(ppseries)])),#prep_price_slc[!timePointYears %in% sel_years & timePointYears < min(missingyear)][order(timePointYears)]$Value, # prep_price_slc[!timePointYears %in% sel_years & timePointYears < min(missingyear)][order(timePointYears)]$Value,
                             #   exp(predlm), 
                             #   prep_price_slc[!timePointYears %in% sel_years & 
                             #                   timePointYears > max(missingyear)][order(timePointYears)]$Value
                             #   ),
                             cgmethod$Value,
                             cpimethod$Value,
                             gdpmethod$Value
                     )] 
      
      
      
      # newimputation[ is.na(Value), 
      #                c('Value', 'flagObservationStatus', 'flagMethod') := 
      #                  list(Imputed, 'P', 'e')]
      
      imp2save <- copy(newimputation)
      #setnames(imp2save, 'Value', 'Original series')
      
      names(newimputation)
      imp2save <- melt(imp2save,
                       id.vars = c("geographicAreaM49",
                                   "geographicAreaM49_description",
                                   "measuredElement",
                                   "measuredElement_description",
                                   "measuredItemCPC",
                                   "measuredItemCPC_description",
                                   "timePointYears",
                                   "Value",
                                   "flagObservationStatus",
                                   "flagMethod"),
                       measure.vars = names(newimputation)[!names(newimputation) %in% c("geographicAreaM49",
                                                                                        "geographicAreaM49_description",
                                                                                        "measuredElement",
                                                                                        "measuredElement_description",
                                                                                        "measuredItemCPC",
                                                                                        "measuredItemCPC_description",
                                                                                        "timePointYears",
                                                                                        "Value",
                                                                                        "flagObservationStatus",
                                                                                        "flagMethod")],
                       variable.name = 'Approach',
                       value.name = 'Estimate')
      imp2save[, Flag := paste(flagObservationStatus, flagMethod, sep = ';')]
      
    }
  }
  return(imp2save)
  
})

output$plot_approach <- renderPlotly({
  
  req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity)
  
  input$btn_country
  
  if(is.null(input$btn_country) |
     is.null(input$btn_year) | is.null(input$btn_start_year) |
     is.null(input$btn_commodity) ) return(NULL)
  
  if(is.null(seriesEstimation())) return(NULL)
  
  sel_country <- country_input[country_input$label == input$btn_country, code]
  
  if(length(sel_country) == 0) return(NULL)
  
  imp2save <- seriesEstimation()
  sel_country <- country_input[country_input$label == input$btn_country, code]
  
  sel_years <- input$btn_start_year:input$btn_year
  startyearevaluation <- as.character(as.numeric(input$btn_start_year)-10)
  sel_commodity <- cpc_list[label == input$btn_commodity, code]
  
  chackIfSeries2impute <- imp2save[timePointYears %in% sel_years]
  
 # validate(need(chackIfSeries2impute[!flagObservationStatus %in% protectedFlags,.N] > 0,
  #              "All data in this series are official."))
  
  pricevalKey = DatasetKey(
    domain = domainPP,
    dataset = datasetPrep,
    dimensions = list(
      Dimension(name = "geographicAreaM49",
                keys = GetCodeList(domainPP, datasetPrep, 'geographicAreaM49')[code == sel_country, code]),
      Dimension(name = "measuredElement", 
                keys = GetCodeList(domainPP, datasetPrep, 'measuredElement')[code %in% '5530', code]),
      Dimension(name = "measuredItemCPC",
                keys = GetCodeList(domainPP, datasetPrep, 'measuredItemCPC')[code == sel_commodity, code]),
      Dimension(name = "timePointYears", 
                keys = GetCodeList(domainPP, datasetPrep, 'timePointYears')[code >= startyearevaluation & code <= max(sel_years), code])))
  
  priceValMD <- tryCatch( GetMetadata(pricevalKey), error = function(priceMDkey) NA)
  
  if(length(priceValMD) == 1){
    if(is.na(priceValMD)){
      series2est$metadata <- priceValMD}
    
  } else {
    priceValMD <- priceValMD[Metadata_Element == 'COMMENT']
    priceValMD <- priceValMD[,c("geographicAreaM49",# "measuredElement",
                                "measuredItemCPC", "timePointYears", "Metadata_Value"), with = F]
    #setnames(priceValMD, "Metadata_Value", "Metadata")
    
    series2est$metadata <- priceValMD
  }
  
  #md2attach <- copy(series2est$metadata)
  md2attach <- copy(priceValMD)
  
  if(length(md2attach) > 1){
    #browser()
    imp2save <- merge(imp2save, md2attach, by = c("geographicAreaM49", "measuredItemCPC",
                                                  "timePointYears" #,  "measuredElement"
    ), all.x = T)
  } else {
    imp2save[,Metadata_Value:= 'No metadata']
  }
  
  if(unique(imp2save$geographicAreaM49) != sel_country) return(NULL)
  
  # group.colors <- c(`Original series` = 'grey31',
  #                   ARIMAX = '#F8766D',
  #                   Ensemble = '#39B600',
  #                   CPI = '#00ABFD',
  #                   LM = '#DC71FA',
  #                   Comm_Group = '#BB9D00',
  #                   PriceRatio = '#00C1AA',
  #                   GDP = )
  
  if(!is.null(input$btn_sel_approach)){
    if(!'All' %in% input$btn_sel_approach){ 
      imp2save <- imp2save[Approach %in% input$btn_sel_approach]
      
    } # else, i.e. if it is equal to All nothing happens all are included
    
    imp2save$timePointYears <- as.character(imp2save$timePointYears)
    imp2save$timePointYears <- format(as.Date(imp2save$timePointYears, format = "%Y"),"%Y")
    
    imp2save$Estimate <- round(imp2save$Estimate, 2)
    imp2save$Value <- round(imp2save$Value, 2)
    
    ggplotly(ggplot(imp2save, aes(x = timePointYears, label = Flag, 
                                  label2 = Metadata_Value))+
               # scale_color_manual(values=group.colors) +
               geom_line(aes(y = Estimate, 
                             group = 1, col = Approach)) +
               geom_point(aes(y = Estimate, col = Approach))+
               geom_line(aes(y = Value, group = 1), col = 'grey31') + #'Original series'+
               geom_point(aes(y =  Value), col = 'grey31') + #'Original series'+
               scale_color_brewer(palette="Set1") +
               ggtitle(paste(unique(imp2save$geographicAreaM49_description),
                             unique(imp2save$measuredItemCPC_description),
                             sep = ' - ' )))
    
    
  } else { # If is null
    
    imp2save$timePointYears <- as.character(imp2save$timePointYears)
    imp2save$timePointYears <- format(as.Date(imp2save$timePointYears, format = "%Y"),"%Y")
    imp2save$Value <- round(imp2save$Value, 2)
    ggplotly(ggplot(imp2save, aes(x = timePointYears, label = Flag, label2 = Metadata_Value))+
               # geom_line(aes(y = Estimate, group = 1, col = Approach)) +
               # geom_point(aes(y = Estimate, col = Approach))+
               geom_line(aes(y =  Value, group = 1), col = 'grey31') + #'Original series'+
               geom_point(aes(y =  Value), col = 'grey31') + #'Original series'+
               scale_color_manual(values=group.colors) +
               ggtitle(paste(unique(imp2save$geographicAreaM49_description),
                             unique(imp2save$measuredItemCPC_description),
                             sep = ' - ' )))
  }
  
})

# -- Buttons to impute ----

output$btn_series_approach <- renderUI({
  
  input$btn_country
  
  req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity)
  
  if(is.null(input$btn_country) | 
     is.null(input$btn_year) | is.null(input$btn_start_year) |
     is.null(input$btn_commodity) ) return(NULL)
  
  sel_country <- country_input[country_input$label == input$btn_country, code]
  
  if(length(sel_country) == 0) return(NULL)
  
  #if(is.null(seriesEstimation())) return(NULL)
  
  imp2save <- seriesEstimation()
  sel_country <- country_input[country_input$label == input$btn_country, code]
  
  if(unique(imp2save$geographicAreaM49) != sel_country) return(NULL)
  
  
  if(!is.null(imp2save)){
    if(nrow(imp2save) >0){
      app_names <- as.character(unique(imp2save$Approach)) 
    } else {
      app_names <- 'Original series'
    }
    
    btn_radio_app <- radioGroupButtons(
      inputId = "btn_series_app",
      individual = FALSE,
      label = "Approach",
      selected = 1,
      choiceNames = app_names, 
      choiceValues = app_names, #1:6,
      status = "primary",
      justified = FALSE,
      direction = "vertical",
      checkIcon = list(
        yes = icon("ok", 
                   lib = "glyphicon"),
        no = icon("remove",
                  lib = "glyphicon"))
    )
    
    btn_radio_app
  }
  
})


# If the value has been detected as outlier a tabel with country and commodity data is shown
output$series_est_tab <- DT::renderDataTable(server = FALSE, {
  req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity)
  
  if(is.null(input$btn_country) | 
     is.null(input$btn_year) | is.null(input$btn_start_year) |
     is.null(input$btn_commodity) ) return(NULL)
  
  sel_country <- country_input[country_input$label == input$btn_country, code]
  
  if(length(sel_country) == 0) return(NULL)
  
  
  ppVal0 <- copy(dataPP$validated) #prepUSD)
  
  sel_country <- country_input[country_input$label == input$btn_country, code]
  sel_years <- input$btn_start_year:input$btn_year
  sel_commodity <- cpc_list[label == input$btn_commodity, code]
  
  ppVal <- ppVal0[geographicAreaM49 == sel_country |
                    measuredItemCPC == sel_commodity]
  
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


observeEvent(input$btn_accept_series ,{
  
  validate(need(!is.null(input$btn_series_app), 
                "No approach chosen, please choose an approach before validating the series."))
  
  approachsel <- input$btn_series_app
  
  sel_years <- input$btn_start_year:input$btn_year
  
  imp2save <- copy(seriesEstimation())
  imp2save <-as.data.table(imp2save)
  imp2save <- imp2save[Approach == approachsel & timePointYears %in% sel_years & !flagObservationStatus %in% protectedFlags]
  imp2save[, c(names(imp2save)[grepl('_description', names(imp2save))], 'Value', 'Flag')] <- NULL
  
  setnames(imp2save, c('Approach', 'Estimate'), c("Metadata_Value", "Value"))
  
  includemetadata <- copy(imp2save[,c("geographicAreaM49", "measuredItemCPC", "timePointYears", "Metadata_Value"), with = F])
  includemetadata[,Metadata:="GENERAL"]
  includemetadata[,Metadata_Element:="COMMENT"]
  includemetadata[,Metadata_Language:="en"]
  
  includemetadata[, measuredElement := '5530']
  includemetadata1 <- copy(includemetadata)[, measuredElement := '5531']
  includemetadata2 <- copy(includemetadata)[, measuredElement := '5532']
  
  includemetadata <- rbind(includemetadata, includemetadata1, includemetadata2)
  
  
  imp2save[, Metadata_Value := NULL]
 
  if(any(imp2save$geographicAreaM49 == '275')){
    
    erKey = DatasetKey(
      domain = 'common',
      dataset = 'exchange_rates_annual',
      dimensions = list(
        Dimension(name = 'geographicAreaM49',
                  keys = GetCodeList('common', 'exchange_rates_annual', 'geographicAreaM49')[code %in% c(unique(imp2save$geographicAreaM49), '376') , code]),
        Dimension(name = "from_currency",
                  keys = GetCodeList('common', 'exchange_rates_annual', 'from_currency')[code != 'ECU' & endDate >= '1991-01-01' | 
                                                                                           is.na(endDate), code]),
        Dimension(name = "to_currency", 
                  keys = GetCodeList('common', 'exchange_rates_annual', 'to_currency')[code == 'USD', code]),
        Dimension(name = 'measuredElement',
                  keys = c('LCU')),
        Dimension(name = "timePointYears", 
                  keys = GetCodeList('common', 'exchange_rates_annual', 'timePointYears')[code %in% unique(imp2save$timePointYears), code]))
      
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
                  keys = GetCodeList('common', 'exchange_rates_annual', 'geographicAreaM49')[code %in% unique(imp2save$geographicAreaM49), code]),
        Dimension(name = "from_currency",
                  keys = GetCodeList('common', 'exchange_rates_annual', 'from_currency')[code != 'ECU' & endDate >= '1991-01-01' | 
                                                                                           is.na(endDate), code]),
        Dimension(name = "to_currency", 
                  keys = GetCodeList('common', 'exchange_rates_annual', 'to_currency')[code == 'USD', code]),
        Dimension(name = 'measuredElement',
                  keys = c('LCU')),
        Dimension(name = "timePointYears", 
                  keys = GetCodeList('common', 'exchange_rates_annual', 'timePointYears')[code %in% unique(imp2save$timePointYears), code]))
      
    )
    
    erdt <- GetData(erKey, flags = F) 
    
  }
  
  erdt[,c('to_currency', 'measuredElement')] <- NULL
  
  message('pp_Conversion: Starting computations')
  
  priceConverted <- convert_currency(priceData = imp2save, erdt = erdt, sessionElement = 'SLC')
  
  
  pper3 <- priceConverted[ !is.na(Value)]
  pper3[, flagObservationStatus := 'I']
  pper3[, flagMethod := 'e']
  
  includemetadata <- includemetadata[pper3, on = c('geographicAreaM49', 'measuredElement', 'timePointYears', 'measuredItemCPC')]
  
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
           data = pper3,
           metadata = includemetadata,
           waitTimeout = Inf)
  
  showModal(modalDialog(
    title = "Data saved!" ,
    sprintf("Please check and save the SWS session data to the dataset.")
  ))
  
})