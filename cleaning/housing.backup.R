
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

adams1 <- read_csv("adams1_parcels.csv")

# get a sense of the data
str(adams1)
summary(adams1)

# select useful variables
adams1 <- select(adams1, parcelnb, accttype, legaldesc, site_address, buildings, 
                 totallandval, totalimpsval, totalacctval, nhname)

# explore possible factor variables
adams1$accttype <- as.factor(adams1$accttype) # all residential
adams1$site_address <- as.factor(adams1$site_address)

dim(adams1) # 128774, 8
n_distinct(adams1$parcelnb) # 128774
n_distinct(adams1$site_address) # 128589

table(adams1$buildings) # 125,742 have only 1 building

adams11 <- filter(adams1, buildings==1)

check <- mutate(adams11, 
                check = totallandval+totalimpsval-totalacctval) # no significant differences
# ok to use total acct value

# ..................................................................................................

load("wages.Rdata") # load the BLS data 

wages <- select(bls.spec, industry, occ_title, a_median) # select useful variables
wages <- mutate(wages, 
                housing = 2.6*a_median) # create a housing metric 

parcels <- select(adams11, totalacctval, nhname) # select useful parcel data

# compare each of the home values to an affordability metric specific to each occupation, 
# and populate a data frame with TRUE/FALSE
df <- map2(wages$occ_title, wages$a_median, ~ parcels %>% 
             transmute(!! .x :=  totalacctval < 2.6 * .y)) %>% bind_cols(parcels, .)
df$nhname <- as.factor(df$nhname) # neighborhood name as factor

# ..................................................................................................

# convert all the TRUE/FALSE values to 1/0s, TRUE = 1, FALSE = 0
df[,3:653] <- lapply(df[,3:653], as.numeric)

# sum every occupation by neighborhood, should give the count of true values
aff <- df %>%
  group_by(nhname) %>% 
  summarise_all(funs(sum))

# remove the total value column, we don't need this now
aff <- select(aff, - totalacctval)

# count the number of parcels in each neighborhood
df.nbhd <- df %>% 
  group_by(nhname) %>%
  count()

# add a neighborhood total column to the data
aff <- left_join(aff, df.nbhd, by="nhname")

# divide each column the values in the total column
aff <- with(aff, cbind(nhname, aff[!names(aff) %in% c("nhname", "n")]/n, n))

# library(data.table) # another way of doing it
# aff2 <- setDT(aff)[, lapply(.SD, `/`, aff$n), .SDcols = 
# `Chief Executives`:`Material Moving Workers, All Other`]

# gather the data from wide to long so occupation is a column for the shiny app
aff <- gather(aff, `Chief Executives`:`Material Moving Workers, All Other`, 
              key="occupation", value="affordability")
# multiply the data set by 100 to get percents
aff <- mutate(aff, 
              affordability = 100*affordability)

# 13 occupations with no median annual, why?
check <- filter(aff, is.na(aff$affordability))
n_distinct(check$occupation)