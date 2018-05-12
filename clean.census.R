
## C. McClintock 
## Shift Research Lab 
## Summer 2018 ## Updated: May 12, 2018 
## Cleaning Script: Census Neighborhood Data from the Denver Open Data Portal

# ..................................................................................................

# SET UP

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

# ...............................................................................................

# READING IN THE DATA

# read the data into R
census10 <- read_csv("census2010.csv") # data from the 2010 Census
census00 <- read_csv("census2000.csv") # data from the 2000 Census 

# looking at the data structure
str(census00)
str(census10)

# dimensions
dim(census10) # 78 rows, 127 colums 
dim(census00) # 78 rows, 117 columns

# check to see that we have the same variables for both time periods 
intersect(names(census10), names(census00)) # same but has 2010 in the variable name

# ...............................................................................................

# CENSUS 2010

# change variable names to lower case
names(census00)
names(census00) <- tolower(names(census00))

# rename some variables for ease of use
census10 <- rename(census10, 
                   "nbhd"="nbrhd_name")


# ...............................................................................................

# CENSUS 2000

# change variable names to lower case
names(census10)
names(census10) <- tolower(names(census10))

# rename some variables for ease of use
census00 <- rename(census00, 
                   "nbhd"="nbrhd_name")

# ...............................................................................................

# save new clean data
write.csv(census10, "census10.clean.csv")
write.csv(census00, "census00.clean.csv")



