tagList(
  # shinythemes::themeSelector(),
  navbarPage(
    # theme = "flatly",  # <--- To use a theme, uncomment this
    "Producer Prices",
    #-- Outlier validation single ----
    tabPanel("Outlier validation",
             column(12,
                    column(2,
                           uiOutput('btn_country_out')                   
                    ),
                    #column(2, uiOutput('btn_year_out')),
                     column(2,
                            uiOutput('btn_outlier')),
                    column(2,
                           br(),
                           uiOutput('save_out_sws')),
                    column(2,
                           uiOutput('btn_tokenOut')
                    ),
                    column(2,
                           br(),
                           actionBttn("btn_upd_token", label = "Update token",
                                      color = "primary",
                                      style = "jelly"))
             ),
             
             tabsetPanel(
               tabPanel("Outlier validation", fluid = TRUE,
                        column(12,
                               column(8,
                                      h4("Detected outlier"),
                                      radioButtons(inputId = "slcORusd", 
                                                   label = "Currency type",
                                                   inline = TRUE,
                                                   choices = list("SLC" = 1, 
                                                                  "USD" = 2)),
                                      plotlyOutput('gg_plot_out_comp', width = '80%')
                               ),
                               column(4,
                                      DT::dataTableOutput('out_tab_slc'))),
                        column(12,
                               column(2,
                                      # br(),
                                      # br(),
                                      actionBttn('btn_not_out', label = 'Normal',
                                                 style = "gradient",
                                                 color = "success")),
                               # br(),
                               # br(),
                               column(2,
                                      actionBttn('btn_is_out', label = 'Outlier',
                                                 style = "gradient",
                                                 color = "danger")),
                               # br(),
                               # br(),
                               column(5,
                                      conditionalPanel("input.btn_is_out",
                                                       column(3,
                                                              uiOutput('btn_outlier_manual')),
                                                       column(2,
                                                              actionBttn('outlier_impute', label = 'Impute',
                                                                         style = "gradient"#,
                                                                         #color = ""
                                                              )
                                                       )))
                        ),
                        column(12,
                               h4("Data from the country and for the selected commodity"),
                               DT::dataTableOutput('out_tab_comp'))
               ),
               #-- Outlier validation bulk ----
               tabPanel("Bulk acceptance",
                        column(12,
                               selectInput(inputId = "btn_country_accept",
                                             label = 'Country',
                                             choices = c('', 'All',country_input$label),
                                             multiple = TRUE),
                               column(2,
                                      actionBttn('btn_bulk_country', label = 'Validate',
                                                 style = "gradient",
                                                 color = "success"))
                               # uiOutput('btn_price_ratio')
                               #  rHandsontableOutput('dt_suggested_est'),
                               # DT::dataTableOutput('ppr_dt')
                        )
               )
               
             )
             # )
    ),
    
    #-- Missing data imputation ----
    tabPanel('Missing data imputation',
             column(12,
                    column(3,
                           uiOutput('btn_validate_country')
                    ),
                     column(3,
                            uiOutput('btn_validate_product'))
             ),
             tabsetPanel(
               tabPanel("Plugin results",
                        column(12,
                               column(8,
                                      h4("Estimated results"),
                                      plotlyOutput('gg_plot_est', width = '80%')
                               ),
                               column(4,
                                      DT::dataTableOutput('est_tab_slc'))),
                        column(12,
                               # br(),
                               # br(),
                               column(3,
                                   #   DT::dataTableOutput('tbl')),
                                      uiOutput('btn_summary_approach')),
                               # br(),
                               # br(),
                               column(2,
                                      actionBttn('btn_accept', label = 'Validate',
                                                 style = "gradient",
                                                 color = "success")),
                               #br(),
                               #br(),
                               column(2,
                                      actionBttn('btn_refuse', label = 'Refuse',
                                                 style = "gradient",
                                                 color = "danger")),
                               # br(),
                               # br(),
                               column(5,
                                      conditionalPanel("input.btn_refuse",
                                                       column(3,
                                                              uiOutput('out_btn_manual')),
                                                       column(2,
                                                              actionBttn('btn_impute', label = 'Impute',
                                                                         style = "gradient"#,
                                                                         #color = ""
                                                              ))))
                        ),
                        column(12,
                               h4("data from the country and for the selected commodity"),
                               DT::dataTableOutput('est_tab'))
               ),
               tabPanel("Bulk imputation",
                       column(12,
                              br(),
                              selectInput(inputId = "btn_country_bulk",
                                          label = 'Country',
                                          choices = c('', 'All', country_input$label),
                                          multiple = TRUE
                                          #  selected = 'Afghanistan - 4'
                              ),
                              selectInput(inputId = "btn_comm_group",
                                          label = 'Commodity group',
                                          choices = c('All', comm_groups$name_en_l3), 
                                          multiple = TRUE
                                          #  selected = 'Afghanistan - 4'
                              ),
                              selectInput(inputId = "btn_method_bulk",
                                          label = 'Method',
                                          choices = approachList#, 
                                          #  selected = 'Afghanistan - 4'
                              ),
                              column(2,
                                     actionBttn('btn_accept_bulk', label = 'Validate',
                                                style = "gradient",
                                                color = "success"))
                             # uiOutput('btn_price_ratio')
                          #  rHandsontableOutput('dt_suggested_est'),
                          # DT::dataTableOutput('ppr_dt')
                   )
               )
             )
    ),
    #-- Data interpolation ----
    tabPanel('Interpolation',
             column(12,
                    column(3,
                           uiOutput('btn_interp_country')),
                    column(3, 
                           uiOutput('btn_interp_product'))
             ),
             tabsetPanel(
               tabPanel("Interpolation validation", fluid = TRUE,
                        column(12,
                               column(8,
                                      h4("Interpolation proposed"),
                                      plotlyOutput('gg_plot_int_comp', width = '80%')
                               ),
                               column(4,
                                      DT::dataTableOutput('int_tab_slc'))),
                        column(12,
                               column(2,
                                      # br(),
                                      # br(),
                                      actionBttn('btn_accept_interp', label = 'Accept interpolation',
                                                 style = "gradient",
                                                 color = "success")),
                               # br(),
                               # br(),
                               column(2,
                                      actionBttn('btn_no_interp', label = 'Refuse interpolation',
                                                 style = "gradient",
                                                 color = "danger"))
                        ),
                        column(12,
                               h4("Data from the country and for the selected commodity"),
                               DT::dataTableOutput('interp_tab'))
               )
               
             )
    ),
    #-- Series revision ----
    tabPanel("Series revision", 
             column(12,
                    column(2,
                           selectInput(inputId = "btn_country",
                                       label = 'Country',
                                       choices = c('', country_input$label)#, 
                                       #  selected = 'Afghanistan - 4'
                           )                    
                    ),
                    column(2,
                           uiOutput('btn_year')),
                    column(2,
                           uiOutput('btn_start_year')),
                    column(2,
                           uiOutput('btn_commodity'))#,
                   # column(2,
                   #        uiOutput('btn_tokenSeries')
                   # ),
                   # column(2,
                   #        br(),
                   #        actionBttn("btn_upd_token_series", label = "Update token",
                   #                   color = "primary",
                   #                   style = "jelly"))
             ),
             tabsetPanel(
               tabPanel('Whole series',
               column(12,
                      column(2,
                             uiOutput('btn_sel_approach'),
                             br(),
                             br(),
                             uiOutput('btn_series_approach'),
                             br(),
                             br(),
                             actionBttn('btn_accept_series', label = 'Validate',
                                        style = "gradient",
                                        color = "success")
                      ),
                      column(10,
                             plotlyOutput('plot_approach', width = '80%'),
                             DT::dataTableOutput('series_est_tab'))
               )
               )
             )
    ),
    tabPanel("Bulk revision check",
           column(12,
                  column(4,
                    fileInput("comparison", "Upload comparison.txt file after running bulk imputation plugin.",
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv"))),
                  column(3,
                    selectInput(inputId = "btn_country_compare",
                                label = 'Country',
                                choices = c('', country_input$label))),
           column(3,uiOutput('btn_commodity_compare'))),
           tabsetPanel(
             tabPanel('Series plot',
                      
                      column(12,

                    fillPage(plotlyOutput('comparison_plot', width = 'auto', height = 'auto')))
             
             ))),
    
    tabPanel("PPIs visualization",
             column(12,
                    column(2,
                           uiOutput('btn_dataset_ppi')),
                    column(2,
                           uiOutput('btn_country_ppi')
                    ),
                     column(2,
                            uiOutput('btn_year_ppi')),
                     column(2,
                            uiOutput('btn_start_year_ppi')),
                     column(2,
                            uiOutput('btn_commodity_ppi'))
        
             ),
             tabsetPanel(
               tabPanel('Series plot',
                        
                        column(12,
                        
                               fillPage(
                                 plotlyOutput('gg_plot_ppi', width="auto", height = 'auto'))
                        )
               )
             )
    ),
    
    
    tabPanel("Update local data", 
             column(12,
                    h4("The shiny contains a local file with data up to 2017 to speed up data loading.
                       Please update this file by clicking on the button if data before 2017 have been modified."),
                    actionBttn('btn_update_data', label = 'Update data',
                               style = "gradient",
                               color = "danger")
                    )
  )
)
)