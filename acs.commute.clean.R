
## C. McClintock 
## Shift Research Lab 
## Summer 2018 ## Updated: May 22, 2018 
## Cleaning Script: ACS 5-year Commute Data

# ..................................................................................................

# set up: wd, retrieve encrypted data
rm(list=ls())
getwd()
# if need be setwd("~/../../")
setwd("R/ShiftResearchLab2018/commute")

# set up: libraries
library(dplyr)
library(forcats)
library(tidyverse)
library(readxl)
library(tidyr)

# ..................................................................................................

# read in the average commute time data (ACS 2016)
avg <- read_csv("avg.commute.csv")
# select just the first row
avg <- avg[1,]

# move columns to rows
avg <- gather(avg, tract, avgcommute, 2:ncol(avg))
avg <- select(avg, -X1)

# read in commute distributions (ACS 2016)
travel <- read_csv("traveltime.csv") 
travel <- travel[1:10,] # select rows with values

# reshape the data to be useful
travel <- gather(travel, tract, n, 2:ncol(travel))
travel <- spread(travel, X1, n)

# rename the variables for ease of use
names(travel)
names(travel) <- c("tract", "10to19", "20to29", "30to39", "40to59", 
                   "60to89", "90plus", "total_outside", "less10", 
                   "workedhome", "total")
# reorder the columns
travel <- select(travel, tract, less10, everything())

# merge the data
commute <- left_join(travel, avg, by="tract")

# read in the tract information
info <- read_csv("acs.meta.csv")
info <- select(info, Geo_FIPS, Geo_NAME, Geo_TRACT)
names(info) <- c("geo_id", "tract", "tract_num")

# merge with commute info
commute <- left_join(info, commute, by="tract")

# add a zero in front of geo_id for ease of merge with tract/neighborhood matching dataa
commute$zero <- 0
commute <- unite(commute, geo_id, zero, geo_id, sep="")

# write the object as a csv for later use
write.csv(commute, "commute.clean.csv")
