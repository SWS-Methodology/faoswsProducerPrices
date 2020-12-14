tagList(
 # shinythemes::themeSelector(),
  navbarPage(
     # theme = "flatly",  # <--- To use a theme, uncomment this
     "Producer Prices Tool",
#-- Overview ----
    tabPanel("Overview",
             column(12,
                    column(3,
                           selectInput(inputId = "btn_country",
                                       label = 'Country',
                                       choices = c('', country_input$label), 
                                       selected = 'Iran (Islamic Republic of) - 364')
                           ),
                    column(2,
                           uiOutput('btn_year')
                           ),
                    column(2,
                           uiOutput('btn_start_year')
                           )
                    ),
               tabsetPanel(
#-- Compare data ----
                 tabPanel("Compare data", fluid = TRUE,
                          column(12,
                                 
                                         # textInput("txt", "Text input:", "general"),
                                          #sliderInput("slider", "Slider input:", 1, 100, 30),
                                         # tags$h5("Deafult actionButton:"),
                                         # actionButton("action", "Search"),

                                        # tags$h5("actionButton with CSS class:"),
                                        #  actionButton("action2", "Action button", class = "btn-primary"),
                                      
                                        h4("Plot comparing validated and questionnaire data "),
                                        plotOutput('gg_plot_out_comp', width = '80%'),
                                        h4("Table comparing validated and questionnaire data "),
                                        DT::dataTableOutput('out_tab_comp')
                                 )
                          ),
#-- Check data ----
                 tabPanel("Check data", 
                          column(12,
                                 column(2,
                                        radioButtons(inputId = "btn_break",
                                                     label = 'Change of currency',
                                                     choices = c(Yes = 'y', No = 'n'),
                                                     selected = 'n',
                                                     inline = T), # To do is to put the B flag at the country values!
                                        
                                        conditionalPanel(condition = "input.btn_break == 'y' ",
                                                         textInput(inputId = 'btn_manual_break', 
                                                                   label = 'Add metadata to change of currency', 
                                                                   value = NA),
                                                         selectInput(inputId = "btn_breakyear",
                                                                     label = 'Year of change',
                                                                     choices = c("", years_input),
                                                                     selected = max(as.numeric(years_input)))
                                                         ),
                                        radioButtons(inputId = "btn_conversion",
                                                     label = 'Conversion needed',
                                                     choices = c(Yes = 'y', No = 'n'),
                                                     selected = 'n',
                                                     inline = T), # To do is to put the B flag at the country values!
                                        
                                        actionBttn(inputId = 'validateQuest',
                                                   label = 'Validate series',
                                                   style = "unite", 
                                                   color = "default"
                                                   )
                                        ),
                                 column(10,
                                        DT::dataTableOutput("rawData"))
                          )),
#-- Product insight ----
                 tabPanel("Product insight", 
                          column(12,
                                 column(3,
                                        uiOutput('btn_missing'),
                                        uiOutput('btn_product'),
                                        
                                        h5('LCU/t'),
                                        DT::dataTableOutput('YPtab')),
                                 column(9,
                                        plotOutput('gg_plot_YP'),
                                        br(),
                                        rHandsontableOutput('tcf_filtered'),
                                        br(),
                                        br(),
                                        actionBttn(inputId = 'show_tcftot',
                                                   label = 'Show whole TCF table',
                                                   style = "unite", 
                                                   color = "default"
                                        ),
                                        br(),
                                        conditionalPanel(condition = "input.show_tcftot",
                                                         DT::dataTableOutput("TCFtot"))
                                        )
                                 )
                          
                          )
               )
            # )
    ),
#-- Missing data imputation ----
    tabPanel('Missing data imputation',
             tabsetPanel(
#-- Commodity Group Information ----
               tabPanel("Commodity Group Information",
                        column(12,
                               column(3,
                                      uiOutput('btn_group_info'),
                                      actionBttn(inputId = 'validate_group',
                                                 label = 'Impute',
                                                 style = "unite", 
                                                 color = "default")
                               ),
                               column(9,
                                      DT::dataTableOutput('groupInfo_tab'),
                                      uiOutput('summary_growth_rate'),
                                      conditionalPanel("input.btn_gR == '5'",
                                                       uiOutput('out_btn_manual')))
                        )),
#-- Auxiliary Information ----
               tabPanel('Auxiliary Information',
             
                    column(4,
                           fileInput("file1", "Choose CSV File",
                                     multiple = FALSE,
                                     accept = c("text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv")),
                           uiOutput('aux_var')),
                    column(2, 
                           checkboxInput("header", "Header", TRUE)
                            ),
                    column(4,
                           selectInput(inputId = 'auxDat',
                                       label = 'Type of data',
                                       choices = c()
                                       )
                           ),
             column(12,
                    tableOutput("contents")
                    )
               ),
#-- Price Ratios ----
             tabPanel('Price Ratios')
             )
             ),
#-- Other ----
    tabPanel("Navbar 3", "This panel is intentionally left blank")
  )
)