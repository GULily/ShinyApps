# server.R
# This script runs in the server (background).

## IMPORTANT
## use the following code to install the dev version of plotly
# devtools::install_github('ropensci/plotly')
## The dev version makes sure the hover info appears on the map
## When you update this application, you need to reinstall the dev version of plotly

# Load packages
library(shiny)
library(ggthemes)
library(plotly)
#library(maps)
#library(ggplot2)


# Main server function
shinyServer(function(input, output) {
  
    # Session selector 
    Select_session <- reactive({
        df <- sessionNew
        
        # Search function
        # TODO: change from "or" to "and"
        if (length(input$keywords!=0)){
            kwd_pattern <- gsub(",", "|", input$keywords)
            df <- subset(df, grepl(kwd_pattern, df$descr, ignore.case = T))
        }
        
        # Date selector
        if (!is.null(input$DateRange)){
            df <- subset(df, as.Date(df$date)>=input$DateRange[1] & as.Date(df$date)<=input$DateRange[2])}
        return(df)
    })
    
    # Session table output tab panel
    output$sessionTable <- renderDataTable({
        df <- Select_session()
        if (length(input$keywords!=0)){
            kwd_pattern <- gsub(",", "|", input$keywords)
            df <- subset(df, grepl(kwd_pattern, df$descr, ignore.case = T))
        }
        
        # Vote title selector
        if(!is.null(input$voteTitle)){
            df <- subset(df, df$unres_title==input$voteTitle)}
        else if(is.null(input$voteTitle)){
            df <- subset(df, df$unres_title == "A/RES/ES-10/19 Status of Jerusalem : resolution / adopted by the General Assembly")}
        
        # Date selector
        df <- subset(df, as.Date(df$date)>=input$DateRange[1] & as.Date(df$date)<=input$DateRange[2])
        
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
        df <- Votesnew
        
        # Link session data and vote data by rcid
        if (length(Select_session()$rcid)!=0){
            df <- subset(df, df$rcid %in% unique(Select_session()$rcid))
        }
        
        # Vote title selector
        if (!is.null(input$voteTitle)){
            df <- subset(df, df$unres_title == input$voteTitle)}
        #### set initial unres title ####
        if(is.null(input$voteTitle) | length(Select_session()$rcid)==0){
          df <- subset(df, df$unres_title == "A/RES/ES-10/19 Status of Jerusalem : resolution / adopted by the General Assembly")
          }
        return(df)

    })
    
    # Title selection
    output$TitleSelectUI <- renderUI({ 
      selectizeInput("voteTitle", "Select the vote title", 
                     choices = Select_session()$unres_title,
                     multiple = T,
                     options = list(maxItems = 1))
    })
    
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
        ## Merge vote and idealpoints
        df <- Select_voting()
        ideal <- subset(idealpoints, idealpoints$session %in% min(Select_voting()$session))
        new.data <- merge(ideal,df,by="ccode")
        df <- new.data[, vars]
        write.csv(df, file)
      }
    )
    
    # Official document hyperlink
    output$OfficialDoc <- renderUI({
      h5(a("Click to view official document at un.org", href = official_doc_link(), target="_blank"))
    })
    
   
    ##############################
    #### Map output tab panel ####
    ##############################
    output$map <- renderPlotly({
      
      # if (!is.null(input$voteTitle)){
      #   df <- subset(df, df$unres_title==input$voteTitle)}
      # #### set initial unres title ####
      # else if(is.null(input$voteTitle)){
      #   df <- subset(df, df$unres_title == "A/RES/ES-10/19 Status of Jerusalem : resolution / adopted by the General Assembly")}
      
      
      
      if (length(unique(Select_voting()$rcid))==1){
        ###world_map<-map_data("world")
        df <- Select_voting()
        df$hover <- with(df, paste(Vote,"<br>",Countryname))
        df$vote[df$vote==8 | df$vote==9] <- 1.5
        
        # light white boundaries
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
        # Case: Yes, Abstain, Nay
        # Case: Yes, Nay
        if(max(df$vote)==3){
          plot_geo(df, locationmode = 'country names') %>%
            add_trace(
              z = ~factor(vote), color = ~vote, 
              colors = c(I("#2ecc71"),I("grey"),I("orange"), I("grey"),I("red")),
              text = ~hover, hoverinfo = "text",
              locations = ~Countryname, marker = list(line = l),
              showscale = F
            ) %>%    #colorbar() %>%
            layout(geo = g)   
        } 
        # Case: Yes, Absent, Abstain
        # Case: Yes, Abstain
        else if(max(df$vote)==2){
          plot_geo(df, locationmode = 'country names') %>%
            add_trace(
              z = ~factor(vote), color = ~vote, 
              colors = c(I("#2ecc71"),I("grey"),I("orange")),
              text = ~hover, hoverinfo = "text",
              locations = ~Countryname, marker = list(line = l),
              showscale = F
            ) %>%    #colorbar() %>%
            layout(geo = g)   
        }
        
      }
      else if (length(input$voteTitle)==0){
        #ggplot()+theme_void()+ggtitle("Select the vote title to view map")
        
      }
      else{ggplot()+theme_void()+ggtitle("Voting data unavailable")}
    })
    #### Map output tab panel END ####
    
    
    ###################################
    #### Analysis output tab panel ####
    ###################################
    
    ## Latin America
    output$analysis1 <- renderPlotly({
      if (length(unique(Select_voting()$rcid))==1){
        name = "Latin America"
        df <- Votesnew
        # Select voting data based on rcid and merge with idealpoints data
        df <- subset(df, df$rcid %in% min(Select_voting()$rcid))
        ideal <- subset(idealpoints, idealpoints$session %in% min(Select_voting()$session))
        new.data <- merge(ideal,df,by="ccode")
        new.data$xvar <- new.data[[input$variable]]
        
        new.data$vote[new.data$vote==8 | new.data$vote==9] <- 1.5
        
        new.data$my_text = paste(new.data$Vote,"<br>",new.data$countryname)
        # Case: Yes, Absent, Abstain, Nay
        if(length(unique(new.data$vote[new.data$regionnew == name]))==4){
          plot_ly(data = subset(new.data, regionnew == name), 
                  x = ~xvar, y = ~reorder(CountryAbb, xvar), 
                  type = "scatter", mode = "markers",
                  color = ~factor(vote), 
                  colors = c(I("#2ecc71"),I("grey"),I("orange"),I("red")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes, Abstain, Nay
        # Case: Yes, Nay
        else if(max(new.data$vote[new.data$regionnew == name])==3){
          plot_ly(data = subset(new.data, regionnew == name), 
                  x = ~xvar, y = ~reorder(CountryAbb, xvar), 
                  type = "scatter", mode = "markers",
                  color = ~factor(vote), 
                  colors = c(I("#2ecc71"),I("grey"),I("orange"),I("grey"),I("red")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes, Absent, Abstain
        # Case: Yes, Abstain
        else if(max(new.data$vote[new.data$regionnew == name])==2){
          plot_ly(data = subset(new.data, regionnew == name),
                  x = ~xvar, y = ~reorder(CountryAbb, xvar),
                  type = "scatter", mode = "markers",
                  color = ~factor(vote),
                  colors = c(I("#2ecc71"),I("grey"),I("orange")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes, Absent
        else if(max(new.data$vote[new.data$regionnew == name])==1.5){
          plot_ly(data = subset(new.data, regionnew == name),
                  x = ~xvar, y = ~reorder(CountryAbb, xvar),
                  type = "scatter", mode = "markers",
                  color = ~factor(vote),
                  colors = c(I("#2ecc71"),I("grey")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes
        else if(max(new.data$vote[new.data$regionnew == name])==1){
          plot_ly(data = subset(new.data, regionnew == name),
                  x = ~xvar, y = ~reorder(CountryAbb, xvar),
                  type = "scatter", mode = "markers",
                  color = ~factor(vote),
                  colors = "#2ecc71",
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        
      }
      else{ggplot()+theme_void()+ggtitle("Select one vote title")}
    })
    
    ## Africa
    output$analysis2 <- renderPlotly({
      if (length(unique(Select_voting()$rcid))==1){
        name = "Africa"
        df <- Votesnew
        # Select voting data based on rcid and merge with idealpoints data
        df <- subset(df, df$rcid %in% min(Select_voting()$rcid))
        ideal <- subset(idealpoints, idealpoints$session %in% min(Select_voting()$session))
        new.data <- merge(ideal,df,by="ccode")
        new.data$xvar <- new.data[[input$variable]]
        
        new.data$vote[new.data$vote==8 | new.data$vote==9] <- 1.5
        
        new.data$my_text=paste(new.data$Vote,"<br>",new.data$countryname)
        # Case: Yes, Absent, Abstain, Nay
        if(length(unique(new.data$vote[new.data$regionnew == name]))==4){
          plot_ly(data = subset(new.data, regionnew == name), 
                  x = ~xvar, y = ~reorder(CountryAbb, xvar), 
                  type = "scatter", mode = "markers",
                  color = ~factor(vote), 
                  colors = c(I("#2ecc71"),I("grey"),I("orange"),I("red")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes, Abstain, Nay
        # Case: Yes, Nay
        else if(max(new.data$vote[new.data$regionnew == name])==3){
          plot_ly(data = subset(new.data, regionnew == name), 
                  x = ~xvar, y = ~reorder(CountryAbb, xvar), 
                  type = "scatter", mode = "markers",
                  color = ~factor(vote), 
                  colors = c(I("#2ecc71"),I("grey"),I("orange"),I("grey"),I("red")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes, Absent, Abstain
        # Case: Yes, Abstain
        else if(max(new.data$vote[new.data$regionnew == name])==2){
          plot_ly(data = subset(new.data, regionnew == name),
                  x = ~xvar, y = ~reorder(CountryAbb, xvar),
                  type = "scatter", mode = "markers",
                  color = ~factor(vote),
                  colors = c(I("#2ecc71"),I("grey"),I("orange")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes, Absent
        else if(max(new.data$vote[new.data$regionnew == name])==1.5){
          plot_ly(data = subset(new.data, regionnew == name),
                  x = ~xvar, y = ~reorder(CountryAbb, xvar),
                  type = "scatter", mode = "markers",
                  color = ~factor(vote),
                  colors = c(I("#2ecc71"),I("grey")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes
        else if(max(new.data$vote[new.data$regionnew == name])==1){
          plot_ly(data = subset(new.data, regionnew == name),
                  x = ~xvar, y = ~reorder(CountryAbb, xvar),
                  type = "scatter", mode = "markers",
                  color = ~factor(vote),
                  colors = "#2ecc71",
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
      }
      else{ggplot()+theme_void()}
    })
    
    ## Asia
    output$analysis3 <- renderPlotly({
      if (length(unique(Select_voting()$rcid))==1){
        name = "Asia"
        df <- Votesnew
        # Select voting data based on rcid and merge with idealpoints data
        df <- subset(df, df$rcid %in% min(Select_voting()$rcid))
        ideal <- subset(idealpoints, idealpoints$session %in% min(Select_voting()$session))
        new.data <- merge(ideal,df,by="ccode")
        new.data$xvar <- new.data[[input$variable]]
        
        new.data$vote[new.data$vote==8 | new.data$vote==9] <- 1.5
        
        new.data$my_text=paste(new.data$Vote,"<br>",new.data$countryname)
        # Case: Yes, Absent, Abstain, Nay
        if(length(unique(new.data$vote[new.data$regionnew == name]))==4){
          plot_ly(data = subset(new.data, regionnew == name), 
                  x = ~xvar, y = ~reorder(CountryAbb, xvar), 
                  type = "scatter", mode = "markers",
                  color = ~factor(vote), 
                  colors = c(I("#2ecc71"),I("grey"),I("orange"),I("red")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes, Abstain, Nay
        # Case: Yes, Nay
        else if(max(new.data$vote[new.data$regionnew == name])==3){
          plot_ly(data = subset(new.data, regionnew == name), 
                  x = ~xvar, y = ~reorder(CountryAbb, xvar), 
                  type = "scatter", mode = "markers",
                  color = ~factor(vote), 
                  colors = c(I("#2ecc71"),I("grey"),I("orange"),I("grey"),I("red")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes, Absent, Abstain
        # Case: Yes, Abstain
        else if(max(new.data$vote[new.data$regionnew == name])==2){
          plot_ly(data = subset(new.data, regionnew == name),
                  x = ~xvar, y = ~reorder(CountryAbb, xvar),
                  type = "scatter", mode = "markers",
                  color = ~factor(vote),
                  colors = c(I("#2ecc71"),I("grey"),I("orange")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes, Absent
        else if(max(new.data$vote[new.data$regionnew == name])==1.5){
          plot_ly(data = subset(new.data, regionnew == name),
                  x = ~xvar, y = ~reorder(CountryAbb, xvar),
                  type = "scatter", mode = "markers",
                  color = ~factor(vote),
                  colors = c(I("#2ecc71"),I("grey")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes
        else if(max(new.data$vote[new.data$regionnew == name])==1){
          plot_ly(data = subset(new.data, regionnew == name),
                  x = ~xvar, y = ~reorder(CountryAbb, xvar),
                  type = "scatter", mode = "markers",
                  color = ~factor(vote),
                  colors = "#2ecc71",
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
      }
      else{ggplot()+theme_void()}
    })
    
    ## Middle East and North Africa
    output$analysis4 <- renderPlotly({
      if (length(unique(Select_voting()$rcid))==1){
        name = "Middle East and North Africa"
        df <- Votesnew
        # Select voting data based on rcid and merge with idealpoints data
        df <- subset(df, df$rcid %in% min(Select_voting()$rcid))
        ideal <- subset(idealpoints, idealpoints$session %in% min(Select_voting()$session))
        new.data <- merge(ideal,df,by="ccode")
        new.data$xvar <- new.data[[input$variable]]
        
        new.data$vote[new.data$vote==8 | new.data$vote==9] <- 1.5
        
        new.data$my_text=paste(new.data$Vote,"<br>",new.data$countryname)
        # Case: Yes, Absent, Abstain, Nay
        if(length(unique(new.data$vote[new.data$regionnew == name]))==4){
          plot_ly(data = subset(new.data, regionnew == name), 
                  x = ~xvar, y = ~reorder(CountryAbb, xvar), 
                  type = "scatter", mode = "markers",
                  color = ~factor(vote), 
                  colors = c(I("#2ecc71"),I("grey"),I("orange"),I("red")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes, Abstain, Nay
        # Case: Yes, Nay
        else if(max(new.data$vote[new.data$regionnew == name])==3){
          plot_ly(data = subset(new.data, regionnew == name), 
                  x = ~xvar, y = ~reorder(CountryAbb, xvar), 
                  type = "scatter", mode = "markers",
                  color = ~factor(vote), 
                  colors = c(I("#2ecc71"),I("grey"),I("orange"),I("grey"),I("red")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes, Absent, Abstain
        # Case: Yes, Abstain
        else if(max(new.data$vote[new.data$regionnew == name])==2){
          plot_ly(data = subset(new.data, regionnew == name),
                  x = ~xvar, y = ~reorder(CountryAbb, xvar),
                  type = "scatter", mode = "markers",
                  color = ~factor(vote),
                  colors = c(I("#2ecc71"),I("grey"),I("orange")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes, Absent
        else if(max(new.data$vote[new.data$regionnew == name])==1.5){
          plot_ly(data = subset(new.data, regionnew == name),
                  x = ~xvar, y = ~reorder(CountryAbb, xvar),
                  type = "scatter", mode = "markers",
                  color = ~factor(vote),
                  colors = c(I("#2ecc71"),I("grey")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes
        else if(max(new.data$vote[new.data$regionnew == name])==1){
          plot_ly(data = subset(new.data, regionnew == name),
                  x = ~xvar, y = ~reorder(CountryAbb, xvar),
                  type = "scatter", mode = "markers",
                  color = ~factor(vote),
                  colors = "#2ecc71",
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
      }
      else{ggplot()+theme_void()}
    })
    
    ## West
    output$analysis5 <- renderPlotly({
      if (length(unique(Select_voting()$rcid))==1){
        name = "West"
        df <- Votesnew
        # Select voting data based on rcid and merge with idealpoints data
        df <- subset(df, df$rcid %in% min(Select_voting()$rcid))
        ideal <- subset(idealpoints, idealpoints$session %in% min(Select_voting()$session))
        new.data <- merge(ideal,df,by="ccode")
        new.data$xvar <- new.data[[input$variable]]
        
        new.data$vote[new.data$vote==8 | new.data$vote==9] <- 1.5
        
        new.data$my_text=paste(new.data$Vote,"<br>",new.data$countryname)
        # Case: Yes, Absent, Abstain, Nay
        if(length(unique(new.data$vote[new.data$regionnew == name]))==4){
          plot_ly(data = subset(new.data, regionnew == name), 
                  x = ~xvar, y = ~reorder(CountryAbb, xvar), 
                  type = "scatter", mode = "markers",
                  color = ~factor(vote), 
                  colors = c(I("#2ecc71"),I("grey"),I("orange"),I("red")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes, Abstain, Nay
        # Case: Yes, Nay
        else if(max(new.data$vote[new.data$regionnew == name])==3){
          plot_ly(data = subset(new.data, regionnew == name), 
                  x = ~xvar, y = ~reorder(CountryAbb, xvar), 
                  type = "scatter", mode = "markers",
                  color = ~factor(vote), 
                  colors = c(I("#2ecc71"),I("grey"),I("orange"),I("grey"),I("red")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes, Absent, Abstain
        # Case: Yes, Abstain
        else if(max(new.data$vote[new.data$regionnew == name])==2){
          plot_ly(data = subset(new.data, regionnew == name),
                  x = ~xvar, y = ~reorder(CountryAbb, xvar),
                  type = "scatter", mode = "markers",
                  color = ~factor(vote),
                  colors = c(I("#2ecc71"),I("grey"),I("orange")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes, Absent
        else if(max(new.data$vote[new.data$regionnew == name])==1.5){
          plot_ly(data = subset(new.data, regionnew == name),
                  x = ~xvar, y = ~reorder(CountryAbb, xvar),
                  type = "scatter", mode = "markers",
                  color = ~factor(vote),
                  colors = c(I("#2ecc71"),I("grey")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes
        else if(max(new.data$vote[new.data$regionnew == name])==1){
          plot_ly(data = subset(new.data, regionnew == name),
                  x = ~xvar, y = ~reorder(CountryAbb, xvar),
                  type = "scatter", mode = "markers",
                  color = ~factor(vote),
                  colors = "#2ecc71",
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
      }
      else{ggplot()+theme_void()}
    })
    
    ## Eastern Europe
    output$analysis6 <- renderPlotly({
      if (length(unique(Select_voting()$rcid))==1){
        name = "Eastern Europe"
        df <- Votesnew
        # Select voting data based on rcid and merge with idealpoints data
        df <- subset(df, df$rcid %in% min(Select_voting()$rcid))
        ideal <- subset(idealpoints, idealpoints$session %in% min(Select_voting()$session))
        new.data <- merge(ideal,df,by="ccode")
        new.data$xvar <- new.data[[input$variable]]
        
        new.data$vote[new.data$vote==8 | new.data$vote==9] <- 1.5
        
        new.data$my_text=paste(new.data$Vote,"<br>",new.data$countryname)
        # Case: Yes, Absent, Abstain, Nay
        if(length(unique(new.data$vote[new.data$regionnew == name]))==4){
          plot_ly(data = subset(new.data, regionnew == name), 
                  x = ~xvar, y = ~reorder(CountryAbb, xvar), 
                  type = "scatter", mode = "markers",
                  color = ~factor(vote), 
                  colors = c(I("#2ecc71"),I("grey"),I("orange"),I("red")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes, Abstain, Nay
        # Case: Yes, Nay
        else if(max(new.data$vote[new.data$regionnew == name])==3){
          plot_ly(data = subset(new.data, regionnew == name), 
                  x = ~xvar, y = ~reorder(CountryAbb, xvar), 
                  type = "scatter", mode = "markers",
                  color = ~factor(vote), 
                  colors = c(I("#2ecc71"),I("grey"),I("orange"),I("grey"),I("red")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes, Absent, Abstain
        # Case: Yes, Abstain
        else if(max(new.data$vote[new.data$regionnew == name])==2){
          plot_ly(data = subset(new.data, regionnew == name),
                  x = ~xvar, y = ~reorder(CountryAbb, xvar),
                  type = "scatter", mode = "markers",
                  color = ~factor(vote),
                  colors = c(I("#2ecc71"),I("grey"),I("orange")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes, Absent
        else if(max(new.data$vote[new.data$regionnew == name])==1.5){
          plot_ly(data = subset(new.data, regionnew == name),
                  x = ~xvar, y = ~reorder(CountryAbb, xvar),
                  type = "scatter", mode = "markers",
                  color = ~factor(vote),
                  colors = c(I("#2ecc71"),I("grey")),
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
        # Case: Yes
        else if(max(new.data$vote[new.data$regionnew == name])==1){
          plot_ly(data = subset(new.data, regionnew == name),
                  x = ~xvar, y = ~reorder(CountryAbb, xvar),
                  type = "scatter", mode = "markers",
                  color = ~factor(vote),
                  colors = "#2ecc71",
                  text = ~my_text, hoverinfo = "text") %>%
            layout(title = name, showlegend=F,
                   yaxis = list(title=F, autotick=F, tickfont = list(size=9)),
                   xaxis = list(title = input$variable))
        }
      }
      else{ggplot()+theme_void()}
    })
    #### Analysis output tab panel END #### 
    
    
    # Official document link
    official_doc_link <- reactive({
        df <- Select_session()
        df$unres = gsub("A/RES", "R", df$unres)
        
        if (!is.null(input$voteTitle)){
                df <- subset(df, df$unres_title==input$voteTitle)
                if (df$session>30){
                # UN official document URL
                url_string <- paste("http://www.un.org/en/ga/search/view_doc.asp?symbol=%20A/RES/",
                                    gsub("\\D", "",strsplit(df$unres[1], split = "/")[[1]][2]), "/",
                                    gsub("\\D", "",strsplit(df$unres[1], split = "/")[[1]][3]), sep = "")
                }
                else {url_string = NULL}
        }
        else {url_string = NULL}
        return(url_string)
    })
    
})
