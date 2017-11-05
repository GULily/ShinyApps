# ui.R
shinyUI(fluidPage(
  headerPanel("Motion Chart"),
  # sidebarPanel(
  #   selectInput("data", "Choose a region:", 
  #               choices = c("Africa", "Asia", "Eastern Europe",
  #                           "Latin America", "West"))
  # ),
  mainPanel(
    htmlOutput("view")
  )
))

