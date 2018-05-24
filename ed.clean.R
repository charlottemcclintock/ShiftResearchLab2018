## C. McClintock 
## Shift Research Lab 
## Summer 2018 ## Updated: May 22, 2018 
## Cleaning Script: CDE Education Data

# ..................................................................................................

# set up: wd, retrieve encrypted data
rm(list=ls())
getwd()
# if need be setwd("~/../../")
setwd("R/ShiftResearchLab2018/ed-data")

# set up: libraries
library(dplyr)
library(forcats)
library(tidyverse)
library(readxl)
library(tidyr)

# ..................................................................................................

# HIGH SCHOOL GRADUATION RATES

# read in the high school graduation data
hs <- read_excel("hsgrad.xlsx")
hs <- hs[-1,] # delete the first row of Excel formulas

hs <- select(hs, `County Name`, `Organization Code`, `Organization Name`, 
             `School Code`, `School Name`, `\"Class Of…\" Anticpated Year of Graduation Cohort`,
             `All Students Graduation Rate`)

# better names
names(hs)
names(hs) <- c("county", "distcode", "district", "schoolcode", "school", "cohort", "gradrate")

# limit to denver 7 metro 
# Adams, Arapahoe, Boulder, Broomfield, Denver, Douglas, Jefferson
denvermetro <- c("ADAMS","ARAPAHOE","BOULDER","BROOMFIELD","DENVER","DOUGLAS","JEFFERSON")
hs <- filter(hs, county %in% denvermetro)

# ..................................................................................................

# READING AND MATH PROFICIENCY

# read in 2017 CMAS data
cmas <- read_excel("cmas2017.xlsx", skip=4)

# recode asterisk values as NA
cmas[cmas=="*"] <- NA

# select useful measures
cmas <- select(cmas, `Level`, `District \r\nCode`, `District \r\nName`, `School \r\nCode`, 
               `School \r\nName`, `Content`, `Test`, `% Met or \r\nExceeded \r\nExpectations`)

# better names
names(cmas)
names(cmas) <- c("level", "distcode", "district", "schoolcode", "school", 
                 "content", "test", "proficiency")

# split into levels
cmas.state <- filter(cmas, cmas$level=="STATE")
cmas.district <- filter(cmas, cmas$level=="DISTRICT")
cmas.school <- filter(cmas, cmas$level=="SCHOOL")

# aggregate school proficiency by grade
cmas.school <- filter(cmas.school, !is.na(cmas.school$proficiency)) # remove observations with NA in proficiency score
cmas.school$proficiency <- as.numeric(cmas.school$proficiency) # coerce proficiency to numeric class
cmas.school <- aggregate(cmas.school$proficiency, by=list(district=cmas.school$district, school=cmas.school$school, 
                                                   content=cmas.school$content), data=cmas.school, FUN=mean)
cmas.school <- rename(cmas.school, "proficiency"="x")
# write the useful objects as CSVs

