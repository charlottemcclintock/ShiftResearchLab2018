
## C. McClintock 
## Shift Research Lab 
## Summer 2018 ## Updated: May 22, 2018 
## Cleaning Script: Assessor Housing Data

# ..................................................................................................

# set up: wd, retrieve data
rm(list=ls())
getwd()
# if need be 
setwd("~/../../")
setwd("Users/charmed33/R/ShiftResearchLab2018/final-data/parcels")

# set up: libraries
library(dplyr)
library(forcats)
library(tidyverse)
library(readxl)
library(tidyr)

# ..................................................................................................

# ADAMS COUNTY

adams <- read_csv("sf_adams_nh.csv") %>% 
            select(buildings, totalacctval, nhname) %>% 
            filter(buildings<=3) %>% 
            select(totalacctval, nhname)
adams <- rename(adams, "homevalue"="totalacctval", "nbhd"="nhname")
adams$county <- "Adams County"

# ..................................................................................................

# ARAPAHOE COUNTY

arapahoe <- read_csv("sf_arapahoe_nh.csv") %>% 
              select(Classifica, Appr_Value, nhname) %>% 
              filter(Classifica == "Single Family" | Classifica == "Residential Condos") %>% 
              select(Appr_Value, nhname)
arapahoe <- rename(arapahoe, "homevalue"="Appr_Value", "nbhd"="nhname")
arapahoe$county <- "Arapahoe County"

# ..................................................................................................

broomfield <- read.csv("sf_broomfield_nh.csv") %>%
                  select(finalactua, nhname)
broomfield <- rename(broomfield, "homevalue"="finalactua", "nbhd"="nhname")
broomfield$county <- "Broomfield County"

# ..................................................................................................

boulder <- read_csv("sf_boulder_nh.csv") %>% 
                select(totalactualval, nhname) 
boulder <- rename(boulder, "homevalue"="totalactualval", "nbhd"="nhname")
boulder$county <- "Boulder County"
# ..................................................................................................

denver <- read_csv("sf_denver_nh.csv") %>% 
  select(units, TOTAL_VALUE, nhname) %>% 
  mutate(perunit = TOTAL_VALUE/units)
denver <- denver[rep(1:nrow(denver), denver$units),] %>% select(perunit, nhname)
denver <- rename(denver, "homevalue"="perunit", "nbhd"="nhname")
denver$county <- "Denver County"

# ..................................................................................................

douglas <- read.csv("sf_douglas_nh.csv") %>% 
  select(accttyp, actual_value, nhname) %>% 
  filter(accttyp=="Residential") %>% 
  select(actual_value, nhname)
douglas <- rename(douglas, "homevalue"="actual_value", "nbhd"="nhname")
douglas$county <- "Douglas County"

# ..................................................................................................

jefferson <- read_csv("sf_jefferson_nh.csv") %>% 
  select(totactval, nhname) 
jefferson <- rename(jefferson, "homevalue"="totactval", "nbhd"="nhname")
jefferson$county <- "Jefferson County"

# ..................................................................................................

parcels <- full_join(adams, arapahoe, by = c("homevalue", "nbhd", "county"))
parcels <- full_join(parcels, broomfield, by =  c("homevalue", "nbhd", "county"))
parcels <- full_join(parcels, boulder, by =  c("homevalue", "nbhd", "county"))
parcels <- full_join(parcels, denver, by =  c("homevalue", "nbhd", "county"))
parcels <- full_join(parcels, douglas, by =  c("homevalue", "nbhd", "county"))
parcels <- full_join(parcels, jefferson, by =  c("homevalue", "nbhd", "county"))

unique <- as.data.frame(unique(parcels$nbhd))
# ..................................................................................................

write.csv(parcels, "parcels.csv")

# ..................................................................................................




