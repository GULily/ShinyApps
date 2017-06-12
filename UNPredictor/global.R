# global.r
# Code inside this file apply both ui and server

# Load dataframes
load("data/UN_vote.RData")# Vote data
# load("data/finaldata.Rdata")
library(foreign)
idealpoints <- read.dta("data/countrydatanew.dta")


sum(is.na(idealpoints$gdppercapita))


