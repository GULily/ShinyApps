# global.r
# Code inside this file apply both ui and server

# Load dataframes
load("data/UN_vote.RData") # Vote data

library(foreign)
idealpoints <- read.dta("data/countrydata.dta")

# map visualization
ccode <- read.csv("data/ccode.csv") # use MapRegionName to link map



#m1 <- merge(idealpoints,vote)

#vshapes <- c(19, 2, 4, 13, 14)
#vcolors <- c("blue", "orange", "red", "grey", "black")

#UN_vote$vote[UN_vote$vote==8] <- NA
#vote$ordvote <- factor(UN_vote$vote, ordered = TRUE, levels = c(1,2,3), labels = c("Yes", "Abstain", "No"))

# START NOT RUN
# Preprocessing vote and session data

# library(dplyr)
# library(lubridate)
# session <- read.csv("descriptions 9-29.csv", stringsAsFactors = F)
# vote <- read.csv("UNGAvotes.csv" , stringsAsFactors = F)
# ccode <- read.csv("ccode.csv", stringsAsFactors = F)
# 
# vote <- vote %>% left_join(ccode)
# 
# session$date <- mdy(session$date)
# session$unres_title <- paste(session$unres, session$short, sep = " ")
# vote$region <- tolower(vote$Country)
# vote$Vote <- ifelse(vote$vote==1, "Yes",
#                     ifelse(vote$vote==2, "Abstain",
#                            ifelse(vote$vote==3, "No",
#                                   ifelse(vote$vote==8, "Absent", "Not UN member"))))
# 
# vote <- vote %>% left_join(session)
# save(session, vote, file = "UN_vote.RData")
# 
# END NOT RUN