# ui.R
# This script builds user interface of data product

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
                           min = min(session$date),
                           max = max(session$date),
                           start = "2015-10-01 UTC",
                           end = max(session$date),
                           startview = "decade"
            ),
            
            # Key word input
            textInput("keywords",
                      "Keywords of interest:"
            ),
            
             # Vote title selector
            htmlOutput("TitleSelectUI"
            ),
            tableOutput("data"),
            
            # Issue area selector
            selectInput("variable", "Variable:",
                        c("Idealpoint" = "Idealpoint",
                          "Democracy" = "polity2",
                          "GDPPerCapita" = "GDPPerCapita",
                          "US Aid/GDP" = "USAIDGDP"
                        )),
            
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
            img(src = "logo.gif", width = "90%"),
            width = 3),
        
        # Main panel
        mainPanel(
            # Tab panel
            tabsetPanel(
                ##############################
                ###### layout changes ########
                ##############################
                tabPanel("Analysis" ,fluidRow(
                  br(),
                  splitLayout(cellWidths = c("33%", "33%", "33%"),
                  #cellWidths = 200,
                  #cellArgs = list(style = "padding: 6px"),
                                                          plotlyOutput("analysis1"),
                                                          plotlyOutput("analysis2"),
                                                          plotlyOutput("analysis3")
                ),
                
                HTML('<center><img src="legend.png" height="40"></center>'),
                
                splitLayout(cellWidths = c("33%", "33%", "33%"),
                  #cellWidths = 200,
                  #cellArgs = list(style = "padding: 6px"),
                                                          plotlyOutput("analysis4"),
                                                          plotlyOutput("analysis5"),
                                                          plotlyOutput("analysis6")
                                                          ))
                         ),
                
                # Map tab panel
                tabPanel("Map", plotlyOutput("map")),
                
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