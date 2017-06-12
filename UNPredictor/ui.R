# ui.R
# This script builds user interface of data product

library(shiny)
library(plotly)

shinyUI(fluidPage(
    
    # Application title
    titlePanel("United Nations General Assembly Voting Analyzer (Predictor)"),
    
    # Sidebar Layout includes sidebar Panel and Main Panel
    sidebarLayout(
        sidebarPanel(
          # upload file
          fileInput('file1', 'Choose CSV File',
                    accept=c('text/csv', 
                             'text/comma-separated-values,text/plain', 
                             '.csv')),
            
            # Issue area selector
            selectInput("region", "Region:",
                        c("Africa" = "Africa",
                          "Asia" = "Asia",
                          "Eastern Europe" = "Eastern Europe",
                          "West" = "West",
                          "Latin America" = "Latin America",
                          "Middle East and North Africa" = "Middle East and North Africa"
                        )),
            
            # Issue area selector
            selectInput("ideal.year", "IdealData Year:",
                        c("2015" = "2015",
                          "2014" = "2014"
                        )),

            # Issue area selector
            selectInput("response", "Response:",
                        c("vote.yes" = "vote.yes",
                          "vote.abstain" = "vote.abstain",
                          "vote.no" = "vote.no"
                        )),

            # Issue area selector
          selectInput("predictor1", "Predictor1:",
                      c("Idealpoint" = "idealpoint",
                        "Democracy" = "polity2",
                        "GDPPerCapita" = "gdppercapita",
                        "US Aid/GDP" = "usaidgdp",
                        "Total China Aid" = "chinaaid",
                        "Total US Aid" = "usaid",
                        "Exports from China" = "exports",
                        "Imports to China" = "imports",
                        "Percentage Country's Exports to US" = "usexpper",
                        "Percentage Country's Imports from US" = "usimpper",
                        "Percentage Country's Exports to China" = "chinaexpper",
                        "Percentage Country's Imports from China" = "chinaimpper"
                      )),
          
          # Issue area selector
          selectInput("predictor2", "Predictor2:",
                      c("Idealpoint" = "Idealpoint",
                        "Democracy" = "polity2",
                        "GDPPerCapita" = "gdppercapita",
                        "US Aid/GDP" = "usaidgdp",
                        "Total China Aid" = "chinaaid",
                        "Total US Aid" = "usaid",
                        "Exports from China" = "exports",
                        "Imports to China" = "imports",
                        "Percentage Country's Exports to US" = "usexpper",
                        "Percentage Country's Imports from US" = "usimpper",
                        "Percentage Country's Exports to China" = "chinaexpper",
                        "Percentage Country's Imports from China" = "chinaimpper"
                      )),
          
          # Issue area selector
          selectInput("predictor3", "Predictor3:",
                      c("Idealpoint" = "Idealpoint",
                        "Democracy" = "polity2",
                        "GDPPerCapita" = "gdppercapita",
                        "US Aid/GDP" = "usaidgdp",
                        "Total China Aid" = "chinaaid",
                        "Total US Aid" = "usaid",
                        "Exports from China" = "exports",
                        "Imports to China" = "imports",
                        "Percentage Country's Exports to US" = "usexpper",
                        "Percentage Country's Imports from US" = "usimpper",
                        "Percentage Country's Exports to China" = "chinaexpper",
                        "Percentage Country's Imports from China" = "chinaimpper"
                      )),

            # Download button (session data)
            # downloadButton('downloadSession', 'Session Data'),
            br(),
            # Download button (voting data)
            downloadButton('downloadData', 'Download Data'),
            # br(),
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
                # table of upload file
                tabPanel("upload file", 
                         dataTableOutput('contents'),
                         br(),
                         uiOutput("hint"),
                         br(),br(),br(),br(),br(),
                         br(),br(),br(),br(),br(),
                         br()),
                #tabPanel("contents2", tableOutput("contents2")),
                # Prediction Analysis tab panel
                tabPanel("Prediction Analysis", 
                         br(),
                         plotlyOutput("prediction"),
                         br(),
                         # summary of statistics
                         uiOutput("summary"),
                         br(),
                         plotOutput("coefplot"),
                         br(),
                         uiOutput("summary2"),
                         verbatimTextOutput("ConfusionMatrix")
                         ),
                
                # Confusion Matrix
                tabPanel("Confusion Matirx", verbatimTextOutput("ConfusionMatrix2")),
                # Map tab panel
                tabPanel("Map", br(), br(), plotlyOutput("map")),
                
                # Session data table tab panel
                # tabPanel("Session Data", dataTableOutput("sessionTable")),
                
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