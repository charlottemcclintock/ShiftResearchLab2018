
## C. McClintock 
## Shift Research Lab 
## Summer 2018 ## Updated: May 12, 2018 
## Cleaning Script: ACS Neighborhood Data from the Denver Open Data Portal

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
acs14 <- read_csv("acs2010-2014.csv") # data from the American Community Survey 2010-2014
acs10 <- read_csv("acs2006-2010.csv") # data from the American Community Survey 2006-2010

# looking at the data structure
str(acs14)
str(acs10)

# dimensions
dim(acs14) # 78 rows, 184 columns 
dim(acs10) # 78 rows, 106 columns 

# check to see that we have the same variables for both time periods 
intersect(names(acs14), names(acs10)) # 97 shared 

# ...............................................................................................

# ACS 2010-2014

# change variable names to lower case
names(acs14)
names(acs14) <- tolower(names(acs14))

# rename some variables for ease of use
acs14 <- rename(acs14, 
                "nbhd"="nbhd_name",
                "population"="ttl_population_all")

# ...............................................................................................

# ACS 2006-2010

# change variable names to lower case
names(acs10)
names(acs10) <- tolower(names(acs10))

# rename some variables for ease of use
acs10 <- rename(acs10, 
                "nbhd"="nbhd_name",
                "population"="ttl_population_all")

# ...............................................................................................

acs14.mini <- select(acs14, nbhd, population, pct_white, pct_black, pct_nativeam, 
                     pct_asian, pct_hawaiianpi, pct_otherrace, pct_twoormore_races, 
                     male, female, pct_ageless18, pct_age65plus, median_age_all, 
                     median_age_male, median_age_female, less_than_hs_diploma_edu, 
                     hsgrad_or_equiv_edu, somecollege_or_aa_edu, bachelors_or_higher_edu, 
                     ttl_housing_units, occupied_hu, vacant_hu, owner_occupied_hu, 
                     renter_occupied_hu, ttl_households, family_households, med_hh_income, 
                     med_family_income, per_capita_income, avg_hh_inc, avg_fam_income, 
                     median_earnings, median_earn_male, median_earn_female, med_gross_rent, 
                     med_contract_rent, pct_poverty, pct_fam_poverty)

# ...............................................................................................

# save new clean data
write.csv(acs14, "acs14.clean.csv")
write.csv(acs10, "acs10.clean.csv")



