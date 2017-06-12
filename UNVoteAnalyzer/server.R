# server.R
# This script runs in the server (background).

# Load packages
library(shiny)
library(ggthemes)
library(plotly)
library(MASS)
library(arm)
library(maps)
#library(ggplot2)
#library(coefplot)
#library(rMaps)



# Main server function
shinyServer(function(input, output) {
    

  
    # Session selector 
    Select_session <- reactive({
        df <- session
        
        # Search function
        # TODO: change from "or" to "and"
        if (length(input$keywords!=0)){
            kwd_pattern <- gsub(",", "|", input$keywords)
            df <- subset(df, grepl(kwd_pattern, df$descr, ignore.case = T))
        }
        
        # Date selector
        if (!is.null(input$DateRange)){
            df <- subset(df, 
                         as.Date(df$date)>=input$DateRange[1] & as.Date(df$date)<=input$DateRange[2])
        }
        
        return(df)
    })
    
    # Session table output 
    output$sessionTable <- renderDataTable({
        df <- Select_session()
        if (length(input$keywords!=0)){
            kwd_pattern <- gsub(",", "|", input$keywords)
            df <- subset(df, grepl(kwd_pattern, df$descr, ignore.case = T))
        }
        
        # Vote title selector
        if (!is.null(input$voteTitle)){
            df <- subset(df, df$unres_title==input$voteTitle)
        }
        if (is.null(input$voteTitle)){
           df <- subset(df, df$unres_title=="R/70/235Oceans and the law of the sea")
        }
        
        
        # Date selector
        df <- subset(df, 
                     as.Date(df$date)>=input$DateRange[1] & as.Date(df$date)<=input$DateRange[2])
        
        # Select only following variables in data table and downloaded csv file
        vars <- c("date","session","unres_title", "descr", "yes", "no", "abstain")
        df <- df[,vars]
        return(df)}, 
        # Turn off searching function
        # List length, 3 options: 5/25/all
        # Page Length, default = 5
        options = list(searching = FALSE,
                       lengthMenu = list(c(5, 25, -1), c('5', '25', 'All')),
                       pageLength = 5)
    )
    
    # Voting selector
    Select_voting <- reactive({
        df <- vote
        
        # Link session data and vote data by rcid
        if (length(Select_session()$rcid)!=0){
            df <- subset(df, df$rcid %in% unique(Select_session()$rcid))
        }
        
        # Vote title selector
        if (!is.null(input$voteTitle)){
            df <- subset(df, df$unres_title==input$voteTitle)
        }
        
        return(df)

    })
    
    
    # Analysis output
  
    output$analysis1 <- renderPlotly({
      if (length(unique(Select_voting()$rcid))==1){
        df <- vote
        # Select voting data based on rcid and merge with idealpoints data
        df <- subset(df, df$rcid %in% min(Select_voting()$rcid))
        ideal <- subset(idealpoints, idealpoints$session %in% min(Select_voting()$session))
        new.data <- merge(ideal,df,by="ccode")
        new.data$xvar <- new.data[[input$variable]]
  
        new.data$my_text=paste(new.data$countryname)
        plot_ly(data = subset(new.data, regionnew == 'Latin America'), 
                x = ~xvar, y = ~reorder(CountryAbb, xvar), 
                type = "scatter", mode = "markers",
                color = ~ordvote, colors = c("#2ecc71", "orange", "red", "grey"),
                text = ~my_text, hoverinfo = "text") %>%
          layout(title = "Latin America", showlegend=F,
                 yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                 xaxis = list(title = input$variable))
      }
      else{ggplot()+theme_void()+ggtitle("Select the vote title")}
    })

    
    
    output$analysis2 <- renderPlotly({
      if (length(unique(Select_voting()$rcid))==1){
        df <- vote
        # Select voting data based on rcid and merge with idealpoints data
        df <- subset(df, df$rcid %in% min(Select_voting()$rcid))
        ideal <- subset(idealpoints, idealpoints$session %in% min(Select_voting()$session))
        new.data <- merge(ideal,df,by="ccode")
        new.data$xvar <- new.data[[input$variable]]
  
        new.data$my_text=paste(new.data$countryname)
        plot_ly(data = subset(new.data, regionnew == 'Africa'), 
                x = ~xvar, y = ~reorder(CountryAbb, xvar), 
                type = "scatter", mode = "markers",
                color = ~ordvote, colors = c("#2ecc71", "orange", "red", "grey"),
                text = ~my_text, hoverinfo = "text") %>%
          layout(title = "Africa", showlegend=F,
                 yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                 xaxis = list(title = input$variable))
      }
      else{ggplot()+theme_void()}
    })
    
    output$analysis3 <- renderPlotly({
      if (length(unique(Select_voting()$rcid))==1){
        df <- vote
        # Select voting data based on rcid and merge with idealpoints data
        df <- subset(df, df$rcid %in% min(Select_voting()$rcid))
        ideal <- subset(idealpoints, idealpoints$session %in% min(Select_voting()$session))
        new.data <- merge(ideal,df,by="ccode")
        new.data$xvar <- new.data[[input$variable]]
  
        new.data$my_text=paste(new.data$countryname)
        plot_ly(data = subset(new.data, regionnew == 'Asia'), 
                x = ~xvar, y = ~reorder(CountryAbb, xvar), 
                type = "scatter", mode = "markers",
                color = ~ordvote, colors = c("#2ecc71", "orange", "red", "grey"),
                text = ~my_text, hoverinfo = "text") %>%
          layout(title = "Asia", showlegend=F,
                 yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                 xaxis = list(title = input$variable))
      }
      else{ggplot()+theme_void()}
    })
    
    output$analysis4 <- renderPlotly({
      if (length(unique(Select_voting()$rcid))==1){
        df <- vote
        # Select voting data based on rcid and merge with idealpoints data
        df <- subset(df, df$rcid %in% min(Select_voting()$rcid))
        ideal <- subset(idealpoints, idealpoints$session %in% min(Select_voting()$session))
        new.data <- merge(ideal,df,by="ccode")
        new.data$xvar <- new.data[[input$variable]]
  
        new.data$my_text=paste(new.data$countryname)
        plot_ly(data = subset(new.data, regionnew == 'Middle East and North Africa'), 
                x = ~xvar, y = ~reorder(CountryAbb, xvar), 
                type = "scatter", mode = "markers",
                color = ~ordvote, colors = c("#2ecc71", "orange", "red", "grey"),
                text = ~my_text, hoverinfo = "text") %>%
          layout(title = "Middle East and North Africa", showlegend=F,
                 yaxis = list(title=F, autotick=F),
                 xaxis = list(title = input$variable))
      }
      else{ggplot()+theme_void()}
    })
    
    output$analysis5 <- renderPlotly({
      if (length(unique(Select_voting()$rcid))==1){
        df <- vote
        # Select voting data based on rcid and merge with idealpoints data
        df <- subset(df, df$rcid %in% min(Select_voting()$rcid))
        ideal <- subset(idealpoints, idealpoints$session %in% min(Select_voting()$session))
        new.data <- merge(ideal,df,by="ccode")
        new.data$xvar <- new.data[[input$variable]]
  
        new.data$my_text=paste(new.data$countryname)
        plot_ly(data = subset(new.data, regionnew == 'West'), 
                x = ~xvar, y = ~reorder(CountryAbb, xvar), 
                type = "scatter", mode = "markers",
                color = ~ordvote, colors = c("#2ecc71", "orange", "red", "grey"),
                text = ~my_text, hoverinfo = "text") %>%
          layout(title = "West", showlegend=F,
                 yaxis = list(title=F, autotick=F),
                 xaxis = list(title = input$variable))
      }
      else{ggplot()+theme_void()}
    })
    
    output$analysis6 <- renderPlotly({
      if (length(unique(Select_voting()$rcid))==1){
        df <- vote
        # Select voting data based on rcid and merge with idealpoints data
        df <- subset(df, df$rcid %in% min(Select_voting()$rcid))
        ideal <- subset(idealpoints, idealpoints$session %in% min(Select_voting()$session))
        new.data <- merge(ideal,df,by="ccode")
        new.data$xvar <- new.data[[input$variable]]
  
        new.data$my_text=paste(new.data$countryname)
        plot_ly(data = subset(new.data, regionnew == 'Eastern Europe'), 
                x = ~xvar, y = ~reorder(CountryAbb, xvar), 
                type = "scatter", mode = "markers",
                color = ~ordvote, colors = c("#2ecc71", "orange", "red", "grey"),
                text = ~my_text, hoverinfo = "text") %>%
          layout(title = "Eastern Europe", showlegend=F,
                 yaxis = list(title=F, autotick=F),
                 xaxis = list(title = input$variable))
      }
      else{ggplot()+theme_void()}
    })
    
    
    
    
    ##Hide the legend from all but one graph, add titles, adjust footnote new data, make long countryname a factor, expand space

    # Session data downloader
    output$downloadSession <- downloadHandler(
        filename = "UN_Session.csv",
        content = function(file) {
          vars <- c("date","session","unres_title", "descr", "yes", "no", "abstain")
            df <- Select_session()[,vars]
            write.csv(df, file)
        }
    )
    
    # Voting data downloader
    output$downloadVoting <- downloadHandler(
        filename = "UN_Voting.csv",
        content = function(file) {
            vars <- c("unres", "ccode", "CountryAbb", "CountryName", "Vote", "g77",
                      "PctAgreeRUSSIA", "PctAgreeBrazil", "PctAgreeChina", "PctAgreeIndia", "PctAgreeIsrael",
                      "Idealpoint", "GDPPerCapita", "GDP", "polity2", "USAid", "USAIDGDP", "regionnew")
            ########################################################
            ########## need merge vote and idealpoints #############
            ########################################################
            df <- Select_voting()
            ideal <- subset(idealpoints, idealpoints$session %in% min(Select_voting()$session))
            new.data <- merge(ideal,df,by="ccode")
            df <- new.data[, vars]
            write.csv(df, file)
        }
    )
    
    # Title selection
    output$TitleSelectUI <- renderUI({ 
        selectizeInput("voteTitle", "Select the vote title", 
                       choices = Select_session()$unres_title,
                       multiple = T,
                       options = list(maxItems = 1))
    })
    
    
    #######################################################
    # Map visualization
    output$map <- renderPlotly({
      if (length(unique(Select_voting()$rcid))==1){
        world_map<-map_data("world")
        df <- Select_voting()
        mydf <- merge(ccode,df,by="ccode")
        
        # colormatrix$colors[colormatrix$breaksvalue==8] <- "grey"
        # colormatrix$breakslabel[colormatrix$breaksvalue==8] <-"DNV"
        # colormatrix$colors[colormatrix$breaksvalue==2] <- "orange"
        # mydf <- merge(dfnew, colormatrix, by.x = "vote", by.y = "breaksvalue")
        
        mydf$hover <- with(mydf, paste(ordvote,"<br>",Country))
        mydf$vote[mydf$Vote=="Absent"] <- 1.5
        
        # light grey boundaries
        l <- list(color = toRGB("white"), width = 0.5)
        
        # specify map projection/options
        g <- list(
          showframe = F,
          showcoastlines = T,
          coastlinecolor = toRGB('white'),
          showland = T,
          landcolor = toRGB("grey90"),
          projection = list(type = 'azequalarea')
        )
        
        p <- plot_geo(mydf,locationmode = 'country names') %>%
          add_trace(
            z = ~vote, color = ~vote, 
            colors = c("#2ecc71","grey","orange", "grey","red"),
            text = ~hover, hoverinfo = "text",
            locations = ~Country, marker = list(line = l),
            showscale = F
          ) %>%    #colorbar() %>%
          layout(geo = g)   #title = 'vote',
        p
      }
      
      else{ggplot()+theme_void()+ggtitle("Select the vote title")}
    })
    ##############################################
    
    
    # Official document link
    official_doc_link <- reactive({
        df <- Select_session()
        if (!is.null(input$voteTitle)){
                df <- subset(df, df$unres_title==input$voteTitle)
                if (df$session>30){
                # UN official document URL
                url_string <- paste("http://www.un.org/en/ga/search/view_doc.asp?symbol=%20A/RES/",
                                    gsub("\\D", "",strsplit(df$unres[1], split = "/")[[1]][2]),
                                    "/",
                                    gsub("\\D", "",strsplit(df$unres[1], split = "/")[[1]][3]),
                                    sep = "")
            }
            else {url_string = NULL}
        }
        else {
            url_string = NULL
        }
        return(url_string)
    })
    
    # Official document hyperlink
    output$OfficialDoc <- renderUI({
        h5(a("Click to view official document at un.org",href = official_doc_link(), target="_blank"))
    })
    
    
    
})
