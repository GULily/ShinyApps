# ui.R
# This script builds user interface of data product

## IMPORTANT
## use the following code to install the dev version of plotly
# devtools::install_github('ropensci/plotly')
## The dev version makes sure the hover info appears on the map
## When you update this application, you need to reinstall the dev version of plotly


library(shiny)
library(plotly)

shinyUI(fluidPage(
    
    # Application title
    titlePanel("United Nations General Assembly Voting Analyzer (Alpha)"),
    
    # Sidebar Layout includes sidebar Panel and Main Panel
    sidebarLayout(
        sidebarPanel(
            # Data range selector
            dateRangeInput("DateRange",
                           "Resolution date range:",
                           min = min(sessionNew$date),
                           max = max(sessionNew$date),
                           start = "2015-12-01 UTC",
                           end = max(sessionNew$date),
                           startview = "decade"),
            
            # Key word input
            textInput("keywords",
                      "Keywords of interest:"),
            
             # Vote title selector
            htmlOutput("TitleSelectUI"),
            tableOutput("data"),
            
            # Issue area selector
            selectInput("variable", "Variable:",
                        c("Idealpoint" = "Idealpoint",
                          "Democracy" = "polity2",
                          "GDPPerCapita" = "GDPPerCapita",
                          "US Aid/GDP" = "USAIDGDP")),
            
            # Download button (session data)
            downloadButton('downloadSession', 'Session Data'),
            br(),
            # Download button (voting data)
            downloadButton('downloadVoting', 'Voting Data'),
            br(),
            br(),
            br(),
            # Official document link
            htmlOutput("OfficialDoc"),
            # Georgetown Logo. inside "www" folder
            #img(src = "logo.gif",height = 50, width = 235),
            img(src = "logo.gif", width = "90%"), width = 3),
        
        # Main panel
        mainPanel(
            # Tab panel
            tabsetPanel(
                # Map tab panel
                tabPanel("Map", plotlyOutput("map")),
                
                # Analysis tab panel
                tabPanel("Analysis" ,fluidRow(
                  br(),
                  splitLayout(cellWidths = c("33%", "33%", "33%"),
                              plotlyOutput("analysis1"), 
                              plotlyOutput("analysis2"),
                              plotlyOutput("analysis3")),
                
                  HTML('<center><img src="legend.png" height="40"></center>'),
                
                  splitLayout(cellWidths = c("33%", "33%", "33%"),
                              plotlyOutput("analysis4"),
                              plotlyOutput("analysis5"),
                              plotlyOutput("analysis6")))),
                
                # Session data table tab panel
                tabPanel("Session Data", dataTableOutput("sessionTable")),
                
                # Help tab panel
                tabPanel(
                    "Help",
                    h3("UN Vote Analyzer"),
                    p("Locating an UNGA resolution is straightforward."),
                    p("First, specify date range of the resolution. This data viewer currently support resulution from the first UNGA voting to 2014-9-9."),
                    p("Then, you may want to input keywords to search resolution description. You can separate different keywords by comma (upcoming)."),
                    p("Finally, select UNGA resolution title to locate vote.")
                )
            ),
            
            # Footnotes
            p("This graph plots observed votes within regions by a variable of the user's choice",
              a("Based on United Nations General Assembly Voting Data.", href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=hdl:1902.1/12379"))

        )
    )
))