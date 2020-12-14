
# I have created an easy example how You can use coupled events from plotly with some sample data that is close to Your needs:
  
  library(shiny)
library(plotly)
library(DT)
set.seed(100)
data <- data.frame(A=sample(c('a1','a2','a3'),10,replace=T),
                   B=1:10,
                   C=11:20,
                   D=21:30)
shinyApp(
  ui = fluidPage(
    plotlyOutput("trace_plot"),
    DT::dataTableOutput('tbl')),
  server = function(input, output) {
    
    output$trace_plot <- renderPlotly({
      plot_ly(data, x=~A,y=~B,z=~C, source = "subset") %>% add_histogram2d()})
    
    output$tbl <- renderDataTable({
      event.data <- event_data("plotly_click", source = "subset")
      
      if(is.null(event.data) == T) return(NULL)
      print(event.data[ ,c(3:4)])
    })
    
  }
)