tagList(
  # shinythemes::themeSelector(),
  navbarPage(
    # theme = "flatly",  # <--- To use a theme, uncomment this
    "Producer Prices",
    #-- Overview ----
    tabPanel("Outlier validation",
             column(12,
                    column(3,
                           uiOutput('btn_outlier')),
                    column(3,
                           br(),
                           uiOutput('save_out_sws')),
                    column(3,
                           uiOutput('btn_tokenOut')
                    ),
                    column(3,
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
               )#,
               # tabPanel()
             )
             # )
    ),
    
    #-- Missing data imputation ----
    tabPanel('Missing data imputation',
             column(12,
                    column(3,
                           uiOutput('btn_validate')
                    )#,
                    # column(3,
                    #        br(),
                    #        uiOutput('save_est_sws'))
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
                              selectInput(inputId = "btn_country_bulk",
                                          label = 'Country',
                                          choices = c('', country_input$label)#, 
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
                                          choices = c('', 'ARIMAX', 'LM', 'GDP', '...')#, 
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
    #-- Other ----
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
                           uiOutput('btn_commodity')),
                    column(2,
                           uiOutput('btn_tokenSeries')
                    ),
                    column(2,
                           br(),
                           actionBttn("btn_upd_token_series", label = "Update token",
                                      color = "primary",
                                      style = "jelly"))
             ),
             tabsetPanel(
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
  )
)