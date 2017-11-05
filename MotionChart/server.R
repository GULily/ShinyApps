# Requires googleVis version 0.4.0 or higher and shiny 0.4.0 or higher
# server.R
library(googleVis)

data = read.csv("MotionChart.csv")
data = na.omit(data)

library(shiny)
shinyServer(function(input, output) {
  # dataInput <- reactive({
  #   switch(input$data,
  #          "Africa" = Africa,
  #          "Asia" = Asia,
  #          "Eastern Europe" = Europe,
  #          "Latin Ameria" = Latin,
  #          "West" = West)
  # })
  
  output$view <- renderGvis({
    gvisMotionChart(data,    # subset data?
                    idvar = "Country",
                    timevar = "Year", 
                    xvar = "Dim1",
                    yvar = "Dim2",
                    colorvar = "Region",
                    options = list(width = 800, height = 600))
    
  })
})


