# global.r
# Code inside this file apply both ui and server

## IMPORTANT
## use the following code to install the dev version of plotly
# devtools::install_github('ropensci/plotly')
## The dev version makes sure the hover info appears on the map
## When you update this application, you need to reinstall the dev version of plotly

# Load dataframes
###load("data/UN_vote.RData") # Old Vote data & Old Session data
load("data/UNVotes1-72full.RData") # New Vote data
Votesnew = Votesfull[,c(-9,-26)]
colnames(Votesnew)[3] = "session"
colnames(Votesnew)[8] = "year"
Votesnew$Vote = ifelse(Votesnew$vote == 1, "Yes",
                       ifelse(Votesnew$vote == 2, "Abstain",
                              ifelse(Votesnew$vote == 3, "No",
                                     ifelse(Votesnew$vote == 8, "Absent",
                                            "Data Not Available"))))
Votesnew$unres_title = paste(Votesnew$unres, Votesnew$short)


load("data/UNDescriptions1-72.RData")
sessionNew = Descriptions # New Session data
sessionNew$unres_title = paste(sessionNew$unres, sessionNew$short)


library(foreign)
idealpoints <- read.dta("data/countrydata.dta")
# TEMPORARY: use 2015's data to impute 2016's and 2017's data
idealpoints2016 = na.omit(idealpoints[idealpoints$year==2015,])
idealpoints2016$year = 2016
idealpoints2016$session = 71
idealpoints2017 = na.omit(idealpoints[idealpoints$year==2015,])
idealpoints2017$year = 2017
idealpoints2017$session = 72
idealpoints = rbind(idealpoints, idealpoints2016, idealpoints2017)


# map visualization
#ccode <- read.csv("data/ccode.csv") # use MapRegionName to link map






