# server.R
# This script runs in the server (background).

# Load packages
library(shiny)
library(ggthemes)
library(plotly)
library(MASS)
library(arm)
library(gmodels)
#library(maps)
#library(ggplot2)


# Main server function
shinyServer(function(input, output){
  # read the upload csv file
  output$contents <- renderDataTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(output$hint <- renderText({paste("Please upload a csv file")}))
    inFile <- read.csv(inFile$datapath, header=T, sep=',', quote='')
                       
    # merge with idealpoints
    ideal <- subset(idealpoints, idealpoints$year == input$ideal.year) # input$ideal.year
    mydata <- merge(inFile, ideal, by="ccode")
    return(mydata)
  })
  
  # data downloader
  output$downloadData <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      inFile <- input$file1
      if(is.null(inFile))
        return(NULL)
      inFile <- read.csv(inFile$datapath, header = T, sep=',', quote = '')
      ########################################
      ########## need merge data #############
      ########################################
      ideal <- subset(idealpoints, idealpoints$year == input$ideal.year)
      mydata <- merge(inFile, ideal, by="ccode")
      write.csv(mydata, file)
    })
  
    ###################################################
    ############ Prediction Analysis ##################
    ###################################################
    output$prediction <- renderPlotly({
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        inFile <- read.csv(inFile$datapath, header=T, sep=',', quote='')
        
        # only use the newest data: 2015
        ideal <- subset(idealpoints, idealpoints$year == input$ideal.year)
        mydata <- merge(inFile, ideal, by="ccode")
        
        mydata$xvar1 <- mydata[[input$predictor1]]
        mydata$xvar2 <- mydata[[input$predictor2]]
        mydata$xvar3 <- mydata[[input$predictor3]]
        
        # remove rows that Vote = ""
        new.data <- mydata[-which(mydata$Vote==""),]
        new.data$xvar1 <- new.data[[input$predictor1]]
        new.data$xvar2 <- new.data[[input$predictor2]]
        new.data$xvar3 <- new.data[[input$predictor3]]
        
        # check: there is too little variation to estimate the model
        if(length(table(subset(new.data, new.data$regionnew == input$region)$Vote))==1){
          ggplot()+theme_void()+ggtitle("There is too little variation to estimate the model") 
        }
        else{
          # ordial probit model: 3 or more vote categories
          if(length(table(subset(new.data, new.data$regionnew == input$region)$Vote))>2){
            if (input$predictor2 == "Idealpoint" & input$predictor3 == "Idealpoint"){
              oprobit <- polr(as.factor(Vote) ~ xvar1, 
                              data = subset(new.data, new.data$regionnew == input$region), #, !is.na(xvar1)
                              Hess = T, model = T, method = "probit")
              output$summary <- renderText({
                paste("You have selected one predictor:", input$predictor1,
                      br()
                      )
              })
              # coefplot
              output$coefplot <- renderPlot(
                coefplot(oprobit, col.pts = "blue", frame.plot = T, na.rm = T)
                ) 
              output$summary2 <- renderText({
                paste("You have selected one predictor:", input$predictor1,
                      br(), br(), 
                      "The coefficient plot graph plots the confidence interval of each coefficient. If the confidence interval includes zero, we cannot reject with 95% confidence that the coefficient is not zero.", br(), br())
              })
            }
            else if (input$predictor3 == "Idealpoint"){
              oprobit <- polr(as.factor(Vote) ~ xvar1 + xvar2, 
                              data = subset(new.data, new.data$regionnew == input$region),
                              Hess = T, model = T, method = "probit")
              output$summary <- renderText({
                paste("You have selected two predictors:", input$predictor1, input$predictor2,
                      br(),
                      "Coefficients:", oprobit$coefficients[1], oprobit$coefficients[2])
              })
              output$coefplot <- renderPlot(
                coefplot(oprobit, col.pts = "blue", frame.plot = T)
              ) 
              output$summary2 <- renderText({
                paste("You have selected two predictors:", input$predictor1, input$predictor2,
                      br(), br(), 
                      "The coefficient plot graph plots the confidence interval of each coefficient. If the confidence interval includes zero, we cannot reject with 95% confidence that the coefficient is not zero.", br(), br())
              })
            }
            else{
              oprobit <- polr(as.factor(Vote) ~ xvar1 + xvar2 + xvar3, 
                              data = subset(new.data, new.data$regionnew == input$region),
                              Hess = T, model = T, method = "probit")
              output$summary <- renderText({
                paste("You have selected four predictors:", input$predictor1, input$predictor2, input$predictor3, input$predictor4,
                      br(),
                      "Coefficients:", oprobit$coefficients[1], oprobit$coefficients[2], oprobit$coefficients[3], oprobit$coefficients[4])
              })
              output$coefplot <- renderPlot(
                coefplot(oprobit, col.pts = "blue", frame.plot = T)
              ) 
              output$summary2 <- renderText({
                paste("You have selected four predictors:", input$predictor1, input$predictor2, input$predictor3, input$predictor4,
                      br(), br(), 
                      "The coefficient plot graph plots the confidence interval of each coefficient. If the confidence interval includes zero, we cannot reject with 95% confidence that the coefficient is not zero.", br(), br())
              })
            }
            # Predict the probability of a given outcome
            probs <- predict(oprobit, mydata, type = "probs")
            for(i in (1:dim(probs)[2])){
              if (colnames(probs)[i] == "Yes"){
                mydata$probs.yes <- as.vector(probs[,i])}
              if (colnames(probs)[i] == "Abstain"){
                mydata$probs.abstain <- as.vector(probs[,i])}
              if (colnames(probs)[i] == "No"){
                mydata$probs.no <- as.vector(probs[,i])}
            }
            
            # Response is vote.yes | vote.abstain | vote.no
            mydata$hover <- with(mydata, paste(Vote, "<br>", countryname))
            if (input$response == "vote.yes"){
              plot_ly (data = subset(mydata, mydata$regionnew == input$region),
                       x = ~probs.yes, y=~reorder(countryabb, probs.yes),
                       color = ~Vote, 
                       colors = c("grey", "orange", "red", "#2ecc71"),
                       text = ~hover,
                       type = "scatter", mode = "markers")%>%
                layout(title = paste(input$region, "-- Probability of yes-vote by actual vote"),
                       yaxis = list(title=F, autotick=F, tickfont = list(size=8)),
                       xaxis = list(title = "probability of a yes-vote"))
            }
            else if (input$response == "vote.abstain"){
              plot_ly (data = subset(mydata, mydata$regionnew == input$region),
                       x = ~probs.abstain, y=~reorder(countryabb, probs.abstain),
                       color = ~Vote, 
                       colors = c("grey", "orange", "red", "#2ecc71"),
                       text = ~hover,
                       type = "scatter", mode = "markers")%>%
                layout(title = paste(input$region, "-- Probability of abstain-vote by actual vote"),
                       yaxis = list(title=F, autotick=F, tickfont = list(size=8)),
                       xaxis = list(title = "probability of a abstain-vote"))
            }
            else if (input$response == "vote.no"){
              plot_ly (data = subset(mydata, mydata$regionnew == input$region),
                       x = ~probs.no, y=~reorder(countryabb, probs.no),
                       color = ~Vote, 
                       colors = c("grey", "orange", "red", "#2ecc71"),
                       text = ~hover,
                       type = "scatter", mode = "markers")%>%
                layout(title = paste(input$region, "-- Probability of no-vote by actual vote"),
                       yaxis = list(title=F, autotick=F, tickfont = list(size=8)),
                       xaxis = list(title = "probability of a no-vote"))
            }
          }
          
          # probit model: only two vote categories
          else{
            if (input$predictor2 == "Idealpoint" & input$predictor3 == "Idealpoint"){
              oprobit <- glm(as.factor(Vote) ~ xvar1,
                              data = subset(new.data, new.data$regionnew == input$region),
                              family = binomial(link = "probit"))
              output$summary <- renderText({
                paste("You have selected one predictor:", input$predictor1,
                      br(),
                      "Coefficient:", oprobit$coefficients[2]) # remove intercept
              })
              output$coefplot <- renderPlot(
                coefplot(oprobit, col.pts = "blue", frame.plot = TRUE)
              )
              output$summary2 <- renderText({
                paste("You have selected one predictor:", input$predictor1,
                      br(), br(), 
                      "The coefficient plot graph plots the confidence interval of each coefficient. If the confidence interval includes zero, we cannot reject with 95% confidence that the coefficient is not zero.", br(), br())
              })

            }
            else if (input$predictor3 == "Idealpoint"){
              oprobit <- glm(as.factor(Vote) ~ xvar1 + xvar2,
                              data = subset(new.data, new.data$regionnew == input$region),
                              family = binomial(link = "probit"))
              output$summary <- renderText({
                paste("You have selected two predictors:", input$predictor1, input$predictor2,
                      br(),
                      "Coefficients:", oprobit$coefficients[2], oprobit$coefficients[3]) # remove intercept
              })
              output$coefplot <- renderPlot(
                coefplot(oprobit, col.pts = "blue", frame.plot = TRUE)
              )
              output$summary2 <- renderText({
                paste("You have selected two predictors:", input$predictor1, input$predictor2,
                      br(), br(), 
                      "The coefficient plot graph plots the confidence interval of each coefficient. If the confidence interval includes zero, we cannot reject with 95% confidence that the coefficient is not zero.", br(), br())
              })
            }
            else{
              oprobit <- glm(as.factor(Vote) ~ xvar1 + xvar2 + xvar3,
                              data = subset(new.data, new.data$regionnew == input$region),
                              family = binomial(link = "probit"))
              output$summary <- renderText({
                paste("You have selected four predictors:", input$predictor1, input$predictor2, input$predictor3, input$predictor4,
                      br(),
                      "Coefficients:", oprobit$coefficients[2], oprobit$coefficients[3], oprobit$coefficients[4], oprobit$coefficients[5]) # remove intercept
              })
              output$coefplot <- renderPlot(
                coefplot(oprobit, col.pts = "blue", frame.plot = TRUE)
              )
              output$summary2 <- renderText({
                paste("You have selected four predictors:", input$predictor1, input$predictor2, input$predictor3, input$predictor4,
                      br(), br(), 
                      "The coefficient plot graph plots the confidence interval of each coefficient. If the confidence interval includes zero, we cannot reject with 95% confidence that the coefficient is not zero.", br(), br())
              })
            }
            # Predict the probability of a given outcome
            probs <- predict(oprobit, mydata, type = "response")
            mydata$probs.yes <- probs
            mydata$probs.no <- 1 - probs
            # for(i in (1:dim(probs)[2])){
            #   if (colnames(probs)[i] == "Yes"){
            #     new.data$probs.yes <- as.vector(probs[,i])}
            #   if (colnames(probs)[i] == "Abstain"){
            #     new.data$probs.abstain <- as.vector(probs[,i])}
            #   if (colnames(probs)[i] == "No"){
            #     new.data$probs.no <- as.vector(probs[,i])}
            # }

            # Response is vote.yes | vote.abstain | vote.no
            mydata$hover <- with(mydata, paste(year, "<br>", countryname))
            if (input$response == "vote.yes"){
              plot_ly (data = subset(mydata, mydata$regionnew == input$region),
                       x = ~probs.yes, y=~reorder(countryabb, probs.yes),
                       color = ~Vote,
                       colors = c("grey", "orange", "red", "#2ecc71"),
                       text = ~hover,
                       type = "scatter", mode = "markers")%>%
                layout(title = paste(input$region, "-- Probability of yes-vote by actual vote"),
                       yaxis = list(title=F, autotick=F, tickfont = list(size=8)),
                       xaxis = list(title = "probability of a yes-vote"))
            }
            # not consider abstain
            else if (input$response == "vote.abstain"){
              plot_ly (data = subset(mydata, mydata$regionnew == input$region),
                       x = ~probs.abstain, y=~reorder(countryabb, probs.abstain),
                       color = ~Vote,
                       colors = c("grey", "orange", "red", "#2ecc71"),
                       text = ~hover,
                       type = "scatter", mode = "markers")%>%
                layout(title = paste(input$region, "-- Probability of abstain-vote by actual vote"),
                       yaxis = list(title=F, autotick=F, tickfont = list(size=8)),
                       xaxis = list(title = "probability of a abstain-vote"))
            }
            # define prob.no = 1 - prob.yes
            else if (input$response == "vote.no"){
              plot_ly (data = subset(mydata, mydata$regionnew == input$region),
                       x = ~probs.no, y=~reorder(countryabb, probs.no),
                       color = ~Vote,
                       colors = c("grey", "orange", "red", "#2ecc71"),
                       text = ~hover,
                       type = "scatter", mode = "markers")%>%
                layout(title = paste(input$region, "-- Probability of no-vote by actual vote"),
                       yaxis = list(title=F, autotick=F, tickfont = list(size=8)),
                       xaxis = list(title = "probability of a no-vote"))
            }
          }
        }
      # else{ggplot()+theme_void()+ggtitle("Select one vote title other than R/70/5")}
      
    })
    
    ##############################################
    output$ConfusionMatrix2 <- renderPrint({
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      inFile <- read.csv(inFile$datapath, header=T, sep=',', quote='')
      #header=input$header,sep=input$sep, quote=input$quote)
      # only use the newest data: 2015
      ideal <- subset(idealpoints, idealpoints$year == input$ideal.year)
      mydata <- merge(inFile, ideal, by="ccode")
      
      mydata$xvar1 <- mydata[[input$predictor1]]
      mydata$xvar2 <- mydata[[input$predictor2]]
      mydata$xvar3 <- mydata[[input$predictor3]]
      
      # remove rows that Vote = ""
      new.data <- mydata[-which(mydata$Vote==""),]
      new.data$xvar1 <- new.data[[input$predictor1]]
      new.data$xvar2 <- new.data[[input$predictor2]]
      new.data$xvar3 <- new.data[[input$predictor3]]
    
      # ordial probit model: 3 or more vote categories
      if(length(table(subset(new.data, new.data$regionnew == input$region)$Vote))>2){
        if (input$predictor2 == "Idealpoint" & input$predictor3 == "Idealpoint"){
          oprobit <- polr(as.factor(Vote) ~ xvar1, 
                          data = new.data,
                          Hess = T, model = T, method = "probit")
        }
        else if (input$predictor3 == "Idealpoint"){
          oprobit <- polr(as.factor(Vote) ~ xvar1 + xvar2, 
                          data = new.data,
                          Hess = T, model = T, method = "probit")
        }
        else{
          oprobit <- polr(as.factor(Vote) ~ xvar1 + xvar2 + xvar3, 
                          data = new.data,
                          Hess = T, model = T, method = "probit")
        }
      }
      predvote <- predict(oprobit, mydata, type = "class")
      CrossTable(mydata$History_vote, predvote, format="SPSS")
    })
    
    
    ###############################
    ##### Map visualization #######
    ###############################
    output$map <- renderPlotly({
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        inFile <- read.csv(inFile$datapath, header=T, sep=',', quote='')
        #header=input$header,sep=input$sep, quote=input$quote)
        ideal <- subset(idealpoints, idealpoints$year == input$ideal.year)
        mydata <- merge(inFile, ideal, by="ccode")
        
        mydata$xvar1 <- mydata[[input$predictor1]]
        mydata$xvar2 <- mydata[[input$predictor2]]
        mydata$xvar3 <- mydata[[input$predictor3]]
        
        # remove rows that Vote = ""
        new.data <- mydata[-which(mydata$Vote==""),]
        new.data$xvar1 <- new.data[[input$predictor1]]
        new.data$xvar2 <- new.data[[input$predictor2]]
        new.data$xvar3 <- new.data[[input$predictor3]]
        
          # ordial probit model: 3 or more vote categories
          if(length(table(subset(new.data, new.data$regionnew == input$region)$Vote))>2){
            if (input$predictor2 == "Idealpoint" & input$predictor3 == "Idealpoint"){
              oprobit <- polr(as.factor(Vote) ~ xvar1, 
                              data = new.data,
                              Hess = T, model = T, method = "probit")
            }
            else if (input$predictor3 == "Idealpoint"){
              oprobit <- polr(as.factor(Vote) ~ xvar1 + xvar2, 
                              data = new.data,
                              Hess = T, model = T, method = "probit")
            }
            else{
              oprobit <- polr(as.factor(Vote) ~ xvar1 + xvar2 + xvar3, 
                              data = new.data,
                              Hess = T, model = T, method = "probit")
            }
            # Predict the probability of a given outcome
            probs <- predict(oprobit, mydata, type = "probs")
            for(i in (1:dim(probs)[2])){
              if (colnames(probs)[i] == "Yes"){
                mydata$probs.yes <- as.vector(probs[,i])}
              if (colnames(probs)[i] == "Abstain"){
                mydata$probs.abstain <- as.vector(probs[,i])}
              if (colnames(probs)[i] == "No"){
                mydata$probs.no <- as.vector(probs[,i])}
            }
          }
        mydata$hover <- with(mydata, paste(Vote,"<br>",round(probs.yes,4),"<br>",countryname))
        
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
        # probs.yes
        p <- plot_geo(mydata,locationmode = 'country names') %>%
          add_trace(
            z = ~probs.yes, color = ~probs.yes, 
            colors = c("red","orange","#2ecc71"),
            text = ~hover, hoverinfo = "text",
            locations = ~countryname, marker = list(line = l),
            showscale = T
          ) %>%    #colorbar() %>%
          layout(geo = g, title = 'Probability of Yes Vote by Country')
        p
        
      })
    ##############################################################
})
