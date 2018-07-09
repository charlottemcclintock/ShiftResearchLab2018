## C. McClintock 
## Shift Research Lab 
## Summer 2018 ## Updated: May 22, 2018 
## Cleaning Script: CDE Education Data

# ..................................................................................................

# set up: wd, retrieve data
rm(list=ls())
getwd()
# if need be setwd("~/../../")
setwd("R/ShiftResearchLab2018/data-ed")

# set up: libraries
library(dplyr)
library(forcats)
library(tidyverse)
library(readxl)
library(tidyr)

# ..................................................................................................

# HIGH SCHOOL GRADUATION RATES

# read in the high school graduation data
hs.og <- read_excel("hsgrad.xlsx")
hs.og <- hs.og[-1,] # delete the first row of Excel formulas
hs <- hs.og

hs <- select(hs, `County Name`, `Organization Code`, `Organization Name`, 
             `School Code`, `School Name`, `\"Class Of…\" Anticpated Year of Graduation Cohort`,
             `All Students Final Grad Base`, `All Students Graduation Rate`)

# better names
names(hs)
names(hs) <- c("county", "distcode", "district", "schoolcode", "school", "cohort", "gradbase", "gradrate")

# limit to denver 7 metro 
# Adams, Arapahoe, Boulder, Broomfield, Denver, Douglas, Jefferson
denvermetro <- c("ADAMS","ARAPAHOE","BOULDER","BROOMFIELD","DENVER","DOUGLAS","JEFFERSON")
hs <- filter(hs, county %in% denvermetro)

# coerce to numeric class
hs$gradrate <- as.numeric(hs$gradrate)
hs$gradbase <- as.numeric(hs$gradbase)

# exclude school name equal to district total
hs <- filter(hs, !school=="DISTRICT TOTAL")

# exclude schools with fewer than 10 students
hs <- subset(hs, gradbase>=9)

# aggregate grad rate over the last 4 years  
hs <- aggregate(hs$gradrate, by=list(school=hs$school), data=hs, FUN=mean) # average grad rate over the past 4 years (2014-2017)
hs <- rename(hs, "gradrate"="x")
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

# aggregate school proficiency by subject area
cmas.school <- filter(cmas.school, !is.na(proficiency)) # remove observations with NA in proficiency score
cmas.school$proficiency <- as.numeric(cmas.school$proficiency) # coerce proficiency to numeric class
school1 <- aggregate(cmas.school$proficiency, by=list(district=cmas.school$district, # average proficiency by subject
                                                      school=cmas.school$school, content=cmas.school$content), data=cmas.school, FUN=mean)
school1 <- rename(school1, "proficiency"="x") 

# aggregate total school proficiency 
school2 <- aggregate(cmas.school$proficiency, by=list(district=cmas.school$district, 
                                                      school=cmas.school$school), data=cmas.school, FUN=mean) # average proficiency across all measured subjects
school2 <- rename(school2, "cmasprof"="x")


# ..................................................................................................

# MERGE SCHOOL LIST WITH GEO INFO

# CMAS SCORES

# read in and rename the data
nbhd <- read_csv("schoolnbhd.csv")
nbhd <- select(nbhd, School_Name, nhname)
names(nbhd) <- c("school", "nhname")
n_distinct(nbhd$nhname) # 229 distinct denver neighborhoods in the CMAS data

# merge nbhd list with school quality data 
ed <- full_join(nbhd, school2, by="school")
ed <- filter(ed, !is.na(cmasprof)) # remove observations with no proficiency scores # many are ECE or charters

# aggregate results by neighborhood
ed.nbhd <- aggregate(ed$cmasprof, by=list(nbhd=ed$nhname), data=ed, FUN=mean) # average proficiency across all measured subjects
ed.nbhd <- rename(ed.nbhd, "cmasprof"="x")

# visualizing the data
ggplot(ed.nbhd, aes(x=cmasprof)) + geom_density()

# HS GRAD RATES

# school names to upper for merge
hs$school <- str_to_upper(hs$school)

# merge nbhd list with hs grad rate data
hs.nbhd <- left_join(hs, nbhd, by="school")

# aggregate results by neighborhood
hs.nbhd <- aggregate(hs.nbhd$gradrate, by=list(nbhd=hs.nbhd$nhname), data=hs.nbhd, FUN=mean) 
# average proficiency across all measured subjects
hs.nbhd <- rename(hs.nbhd, "gradrate"="x")

# visualizing the data
ggplot(hs.nbhd, aes(x=gradrate)) + geom_density()

# ..................................................................................................

# percentile rank for graduation rates and for CMAS scores in order to combine them into a useful stat
perc.rank <- function(x) trunc(rank(x))/length(x)

# for CMAS data
ed.nbhd <- mutate(ed.nbhd, 
                  cmas.perc = perc.rank(ed.nbhd$cmasprof))

# for grad rate data
hs.nbhd <- mutate(hs.nbhd, 
                  grad.perc = perc.rank(hs.nbhd$gradrate))
# ..................................................................................................

# DECIDING HOW TO MERGE GRAD RATE AND PROFICIENCY DATA

arb1 <- anti_join(hs.nbhd, ed.nbhd, by="nbhd") # 3 obsevations in hs data but not CMAS data
arb2 <- anti_join(ed.nbhd, hs.nbhd, by="nbhd") # 119 in CMAS data but not in HS data
arb3 <- inner_join(ed.nbhd, hs.nbhd, by="nbhd") # 101 observations in both
# total of 223 neighborhoods represented

# combine into one useful table
all <- full_join(hs.nbhd, ed.nbhd, by="nbhd")

# visualizing the data 
ggplot(all, aes(x=gradrate, y = cmasprof)) + geom_point() + geom_smooth(method = "lm")
ggplot(all, aes(x=grad.perc, y = cmas.perc)) + geom_point() + geom_smooth(method = "lm")

# if grad rate and CMAS proficiency are both given, make school quality the weighted average.
all <- mutate(all, 
              schoolquality = ifelse(!is.na(grad.perc)&!is.na(cmas.perc), 
                                     (grad.perc+cmas.perc)/2, NA))

# if we have grad rate but no CMAS, make school quality the grad rate percentile rank
all$schoolquality <- ifelse(is.na(all$schoolquality)&is.na(all$cmas.perc), 
                            all$grad.perc, all$schoolquality)

# if we have grad rate but no CMAS, make school quality the grad rate percentile rank
all$schoolquality <- ifelse(is.na(all$schoolquality)&is.na(all$grad.perc), 
                            all$cmas.perc, all$schoolquality)