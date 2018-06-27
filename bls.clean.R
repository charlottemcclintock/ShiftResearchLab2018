
## C. McClintock 
## Shift Research Lab 
## Summer 2018 ## Updated: May 22, 2018 
## Cleaning Script: Bureau of Labor Statistics Occupational Data

# ..................................................................................................

# set up: wd, retrieve encrypted data
rm(list=ls())
getwd()
# if need be setwd("~/../../")
setwd("R/ShiftResearchLab2018/data-bls")

# set up: libraries
library(dplyr)
library(forcats)
library(tidyverse)
library(readxl)
library(tidyr)

# ..................................................................................................

# read in the data 
bls <- read_excel("MSA_M2017_dl.xlsx")

# make the variable names lower case
names(bls)
names(bls) <- tolower(names(bls))
bls <- rename(bls, "loc_quotient"="loc quotient")

# select only the denver region data
bls <- filter(bls, area_name=="Denver-Aurora-Lakewood, CO")

# recode asterisk values as NA
bls[bls=="*"] <- NA # code book says "* = indicates that a wage estimate is not available"

# recode hashtag values as 208,000
bls[bls=="#"] <- 208000 # code book says "# = indicates a wage that is equal to or greater than $100.00 per hour or $208,000 per year"
# NOTE: this is a lower limit

# quick overviews
dim(bls)
summary(bls)
str(bls)

# coerce to numeric class
bls[,6:23] <- lapply(bls[,6:23], as.numeric)

# split occupational code at -
bls <- separate(bls, occ_code, c("occ_code1", "occ_code2"), sep = "-")

# subset of generalized professions
bls.gen <- filter(bls, occ_code2=="0000") # seem to all end occ_code in "0000" 
bls.gen <- filter(bls.gen, !occ_title=="All Occupations") # remove the occupational sum

# subset of specific professions
bls.spec <- filter(bls, !occ_code2=="0000") # all except general

# the value summary for all occupations 
bls.all <- filter(bls, occ_title=="All Occupations")

sum(bls.spec$jobs_1000, na.rm=TRUE) # 976.82 # what's missing? # 23.16
sum(bls.gen$jobs_1000) # 999.99 # good

# check to see if occ_code1 matches 1-1 to general occupations
n_distinct(bls.spec$occ_code1) # 22
nrow(bls.gen) # 22

check <- filter(bls.spec, is.na(a_median))
n_distinct(check$occ_title)

# ..................................................................................................

# add a variable to account for spread
bls.gen <- mutate(bls.gen, 
                  quartilespread = a_pct75-a_pct25)
bls.spec <- mutate(bls.spec, 
                  quartilespread = a_pct75-a_pct25)

# what's the relationship between median annual wage and quartile spread?
ggplot(bls.gen, aes(a_median, quartilespread)) + geom_point() + geom_smooth(method="lm", se = FALSE)
ggplot(bls.spec, aes(a_median, quartilespread)) + geom_point() + geom_smooth(method="lm", se = FALSE)

# ..................................................................................................

# add gen to spec to get an industry variable

# create a matching table
bls.ind <- select(bls.gen, occ_code1, occ_title) 
bls.ind <- rename(bls.ind, "industry"="occ_title")

# join to data
bls.spec <- left_join(bls.spec, bls.ind, by="occ_code1")


# ..................................................................................................

# write objects as clean csv files
write.csv(bls.gen, "clean.bls.gen.csv")
write.csv(bls.spec, "clean.bls.spec.csv")

# ..................................................................................................

rm(list = ls(pattern = "all")) 
rm(list = ls(pattern = "ind")) 
rm(list = ls(pattern = "gen")) 

# save the data to use later
save.image("wages.Rdata")


