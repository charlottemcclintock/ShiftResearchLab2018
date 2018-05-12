
## C. McClintock 
## Shift Research Lab 
## Summer 2018 ## Updated: May 12, 2018 
## Cleaning Script: Police Pedestrian and Vehicle Stop Data from the Denver Open Data Portal

# ..............................................................................................

# set up: wd, retrieve encrypted data
rm(list=ls())
getwd()
# /Users/charmed33
setwd("R/ShiftResearchLab2018/denver-data")
# "/Users/charmed33/R/ShiftResearchLab2018/denver"

# set up: libraries
library(dplyr)
library(forcats)
library(tidyverse)
library(readxl)
library(tidyr)
library(lubridate)
library(stringr)

# ..............................................................................................

# READING IN THE DATA

# read the data into R
police <- read_csv("police_stops.csv")

# dimensions
dim(police) # 871,696 rows, 14 columns 

# ..............................................................................................

# CLEANING THE DATA

# variable names to lower case
names(police)
names(police) <- tolower(names(police))

# rename for ease of use
police <- rename(police, 
                   "nbhd"="neighborhood_name",
                    "priority"="priority_description",
                    "disposition"="call_disposition")
# coerce to factors
police <- mutate(police, 
                 nbhd=as.factor(nbhd),
                 priority=as.factor(priority), 
                 problem=as.factor(problem),
                 call_class=as.factor(call_class),
                 disposition=as.character(disposition))

# TOTAL CALLS

# count incidents by neighborhood
plc <- police %>% 
  group_by(nbhd) %>% 
  count()
plc <- rename(plc, "freq"="n")

# TYPE OF PROBLEM

# count incidents by neighborhood and type of problem
prob <- police %>% 
  group_by(nbhd) %>% 
  count(problem)
prob <- spread(prob, problem, n)

# merge together
plc <- left_join(plc, prob, by="nbhd")

# DISPOSITION

# select disposition 
disp <- select(police, disposition, nbhd)
# coerce to character vector
disp <- mutate(disp, disposition=as.character(disposition))
# split at delimiter
disp <- separate(disp, disposition, c("disposition", "b"), sep = ",", extra="drop")
# select only the first column
disp <- select(disp, disposition, nbhd)

# check to see levels now
disp <- mutate(disp, disposition = as.factor(disposition))
levels(disp$disposition)

# count by disposition
disp <- disp %>% 
  group_by(nbhd) %>% 
  count(disposition)
disp <- spread(disp, disposition, n)

# merge with other police 
plc <- left_join(plc, disp, by="nbhd")

names(plc)
names(plc) <- c("nbhd", "freq", "subject.stop", "vehicle.stop", "alarm.rp", "alarm.false", 
                "alarm.good", "alarm.cancelled", "arrest", "backup", "cit", "detox", "evidence", "fileonly", 
                "goa", "homeless", "in.service", "street.check", "clearance", "message.left", 
                "no.police.needed", "none", "vehicle.release", "party.advised", "quit", 
                "report.made", "supervisor.cancelled", "citation", "unfounded", "vehicle.towed",
                "warning.issued", "exchanged.info", "broadcast", "test")

# ..............................................................................................

# EXPLORATORY ANALYSIS

# how many incidents do we have?
sum(plc$freq) # 871696

# create some new useful variables
plc <- mutate(plc, 
              pct.of.total = freq*100/871696,
              pct.subject = subject.stop*100/freq,
              pct.vehicle = vehicle.stop*100/freq,
              pct.arrest = arrest*100/freq,
              pct.warning = warning.issued*100/freq,
              pct.citation = citation*100/freq)

# reorder for convenience
plc <- select(plc, nbhd, freq, pct.of.total, subject.stop, pct.subject, vehicle.stop,
              pct.vehicle, arrest, pct.arrest, warning.issued, pct.warning, citation, 
              pct.citation, everything())

summary(plc)

# ..............................................................................................

# save new clean data
write.csv(plc, "police.clean.csv")



