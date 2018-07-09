## C. McClintock 
## Shift Research Lab 
## Summer 2018 ## Updated: May 22, 2018 
## Cleaning Script: CDE Education Data

# ..................................................................................................

# set up: wd, retrieve data
rm(list=ls())
getwd()
# if need be # setwd("~/../../") setwd("Users/charmed33/R")
setwd("ShiftResearchLab2018/cleaning/data-ed")

# set up: libraries
library(dplyr)
library(forcats)
library(tidyverse)
library(readxl)

# ..................................................................................................

# get a matching table for nhids and nh names 

match <- read_csv("matchingtracts.csv")
match <- select(match, nhid, nhname)
match <- match[!duplicated(match), ]
match <- separate(match, nhname, c("nhname", "num2"), sep = "\"")
match$nhname <- ifelse(match$nhname=="'", match$num2, match$nhname)
match <- select(match, -num2)

es <- read_csv("nh_eleschl.csv")
ms <- read_csv("nh_midschl.csv")
hs <- read_csv("nh_highschl.csv")

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
cmas.school <- aggregate(cmas.school$proficiency, by=list(district=cmas.school$district, 
                   school=cmas.school$school, content=cmas.school$content), data=cmas.school, FUN=mean)
cmas.school <- rename(cmas.school, "proficiency"="x") 

# aggregate total school proficiency 
cmas.school <- aggregate(cmas.school$proficiency, by=list(district=cmas.school$district, 
                school=cmas.school$school), data=cmas.school, FUN=mean) 
cmas.school <- rename(cmas.school, "cmasprof"="x")
cmas.school <- select(cmas.school, -district)


# ..................................................................................................

es <- select(es, nhid, School_Name) # 792
names(es) <- c("nhid", "school")

es <- left_join(es, cmas.school, by = "school") # 902
check <- filter(es, is.na(es$cmasprof)) # 31 of 902
es <- filter(es, !is.na(es$cmasprof))

es <- left_join(es, match, by = "nhid")
n_distinct(es$nhname) # 263

es <- aggregate(es$cmasprof, by=list(nhname=es$nhname, 
                    nhid=es$nhid), data=es, FUN=mean) 
es <- rename(es, "elementary"="x")

# ..................................................................................................

ms <- select(ms, nhid, School_Name) # 792
names(ms) <- c("nhid", "school")

ms <- left_join(ms, cmas.school, by = "school") # 812
check <- filter(ms, is.na(ms$cmasprof)) # 92 of 812
ms <- filter(ms, !is.na(ms$cmasprof))

ms <- left_join(ms, match, by = "nhid")
n_distinct(ms$nhname) # 261

ms <- aggregate(ms$cmasprof, by=list(nhname=ms$nhname, 
                                     nhid=ms$nhid), data=ms, FUN=mean) 
ms <- rename(ms, "middle"="x")

# ..................................................................................................

hs <- select(hs, nhid, School_Name) # 792
names(hs) <- c("nhid", "school")

hs <- left_join(hs, cmas.school, by = "school") # 811
check <- filter(hs, is.na(hs$cmasprof)) # 224 of 902
hs <- filter(hs, !is.na(hs$cmasprof))

hs <- left_join(hs, match, by = "nhid")
n_distinct(hs$nhname) # 261

hs <- aggregate(hs$cmasprof, by=list(nhname=hs$nhname, 
                                     nhid=hs$nhid), data=hs, FUN=mean) 
hs <- rename(hs, "highschool"="x")

# ..................................................................................................

ed <- left_join(es, ms, c("nhname", "nhid"))
ed <- left_join(ed, hs, c("nhname", "nhid"))

ed <- gather(ed, 3:5, key="grades", value="proficiency")

ed <- aggregate(ed$proficiency, by=list(nhname=ed$nhname, 
                   nhid=ed$nhid), data=ed, FUN=mean, na.rm=TRUE) 
ed <- rename(ed, "schoolquality"="x")

# ..................................................................................................

rm(list = ls(pattern = "s")) 
rm(list = ls(pattern = "k")) 

save.image("ed.Rdata")

# FIGURE OUT WHAT'S GOING ON WITH NEIGHBORHOOD NAMES

es.check <- as.data.frame(unique(es$nhname))
names(es.check) <- "nbhd"
data.check <- read_csv("data.csv")
parcels.check <- read_csv("parcels.csv") %>% select(x)
names(parcels.check) <- "nbhd"

arb1 <- anti_join(es.check, data.check, "nbhd")
arb2 <- anti_join(data.check, es.check, "nbhd")
arb3 <- anti_join(es.check, parcels.check, "nbhd")
arb4 <- anti_join(parcels.check, es.check, "nbhd")
arb5 <- anti_join(data.check, parcels.check, "nbhd")
arb6 <- anti_join(parcels.check, data.check, "nbhd")

arbi1 <- inner_join(es.check, data.check, "nbhd")
arbi2 <- inner_join(parcels.check, data.check, "nbhd")
arbi3 <- inner_join(es.check, parcels.check, "nbhd")

all3 <- inner_join(arbi1, arbi2, "nbhd") %>% select(nbhd)
