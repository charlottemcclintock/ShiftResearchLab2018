
## C. McClintock 
## Shift Research Lab 
## Summer 2018 ## Updated: May 22, 2018 
## Cleaning Script: ACS 5-year Commute Data

# ..................................................................................................

# set up: wd, retrieve data
rm(list=ls())
getwd()
# if need be 
setwd("~/../../")
setwd("Users/charmed33/R/ShiftResearchLab2018/cleaning/data-commute")

# set up: libraries
library(dplyr)
library(forcats)
library(tidyverse)
library(readxl)
library(tidyr)

# ..................................................................................................

# AVERAGE COMMUTE TIME

# read in the average commute time data (ACS 2016)
avg <- read_csv("avg.commute.csv")
# select just the first row
avg <- avg[1,]

# move columns to rows
avg <- gather(avg, tract, avgcommute, 2:ncol(avg))
avg <- select(avg, -X1)
# ..................................................................................................

# read in the tract information
info <- read_csv("acs.meta.csv")
info <- select(info, Geo_FIPS, Geo_NAME, Geo_TRACT)
names(info) <- c("geo_id", "tract", "tract_num")

# merge with commute info
commute <- left_join(info, avg, by="tract")

# ..................................................................................................

# MATCHING TRACTS TO NEIGHBORHOODS

# read in matching tracts data
match <- read_csv("matchingtracts.csv")
match <- rename(match, "geo_id"="geoid10")
unique.check <- as.data.frame(unique(match$nhname))
unique.check <- rename(unique.check, "nhname" = "unique(match$nhname)")

# merge with commute data
nbhd <- full_join(match, commute, by="geo_id")
unique.check2 <- as.data.frame(unique(nbhd$nhname))

# break up the big tract variable
nbhd <- separate(nbhd, tract, c("tract", "county", "state"), sep = ",")
nbhd <- select(nbhd, -c(tract, state))

# ..................................................................................................

# EXPLORING

n_distinct(nbhd$nhid) # 265
n_distinct(nbhd$nhname) # 264 # why is this one less?

# n occurences of each neighborhood name
nhname.freq <- nbhd %>% 
  count(nhname) # 576 NAs, tracts that don't match a neighborhood

# n occurences of each neighborhood id
nhid.freq <- nbhd %>% 
  count(nhid) # 576 NAs, tracts that don't match a neighborhood

arb <- filter(nbhd, is.na(nhname)&!is.na(nhid)) # no values so nhname is not blank 


str(nbhd) # check the class of the variables
nbhd$avgcommute <- as.numeric(nbhd$avgcommute) # coerce avg commute to numeric class
nbhd$nhid <- as.numeric(nbhd$nhid)

# what's the average denver metro commute time? 
mean(nbhd$avgcommute, na.rm=TRUE) # 26.57
median(nbhd$avgcommute, na.rm = TRUE) # 26

# compute neighborhood average commute time
nbhd.avg <- aggregate(nbhd$avgcommute,by=list(name=nbhd$nhname, nhid=nbhd$nhid), data=nbhd, FUN=mean)
nbhd.avg <- rename(nbhd.avg, "avgcommute"="x")
nrow(nbhd.avg) # 264
# 7 NA values, why?
na.check <- filter(nbhd.avg, is.na(avgcommute))

# check with NAs in avg commute removed
nbhd2 <- filter(nbhd, !is.na(nbhd$avgcommute)) # remove observations with NA in avg commute
nbhd.avg2 <- aggregate(nbhd2$avgcommute,by=list(name=nbhd2$nhname, nhid=nbhd2$nhid), data=nbhd2, FUN=mean)
nbhd.avg2 <- rename(nbhd.avg2, "avgcommute"="x")
nrow(nbhd.avg2) # 262 # 2 missing

# check which two are missing 
arb <- anti_join(nbhd.avg, nbhd.avg2, by="name")
# Federal Center # Rocky Mountain Arsenal
# one tract each with a missing avg commute and missing travel time, will be excluded

nbhd.avg3 <- aggregate(nbhd2$avgcommute,by=list(name=nbhd2$nhname, nhid=nbhd2$nhid), data=nbhd2, FUN=median)
nbhd.avg3 <- rename(nbhd.avg3, "medcommute"="x")

check <- left_join(nbhd.avg2, nbhd.avg3, by = c("name", "nhid"))

check <- mutate(check, delt = avgcommute-medcommute) 
# difference between average and median always less than 2 minutes, usually 0 

# rename for ease of use
avgcmt <- nbhd.avg2

# number of distinct neighborhoods
n_distinct(avgcmt$name) # 261

# ..................................................................................................

# LOOK AT DATA WITH MISSING COMMUTE IN THE APP DATA

missing <- read.csv("missing.commute.csv") # 41 neighborhoods
missing <- rename(missing, "nhname"="nbhd")
arb1 <- inner_join(missing, unique.check, by = "nhname") # Rocky Mountain Arsenal is commute missing data 
arb2 <- anti_join(missing, unique.check, by = "nhname") # 40 neighborhoods in missing but not commute 
arb3 <- anti_join(unique.check, missing, by = "nhname") # 263 neighborhoods in commute but not missing

avgcmt$name <- as.factor(avgcmt$name)

avgcmt <- mutate(avgcmt, 
                 name=fct_recode(name, 
                                 "Benedict Park, Central Brighton" = "Benedict Park",                       
                                 "Chautauqua, Table Mesa, and Devil's Thumb" = "Chautauqua",                
                                 "Delmar Parkway, Jewell Heights, and Hoffman Heights" = "Delmar Parkway",     
                                 "East central Englewood" = "Englewood",                               
                                 "Glover, Plum Creek Fairway and Crystal Valley Ranch" = "Glover",     
                                 "Green Valley Ranch" = "Gateway - Green Valley Ranch",                                      
                                 "Gunbarrel Area" = "Gunbarrel",                                       
                                 "Heritage Hills and Stonegate" = "Stonegate",                         
                                 "Hine Lake, Powderhorn Park and Westbury Park" = "Hine Lake",           
                                 "Lakota Hills, Golden Proper" = "Lakota Hills",                            
                                 "Louisville and Southeast Boulder Valley" = "Louisville",                   
                                 "Northwest Sheridan" = "Sheridan",                               
                                 "Okane Park, Washington Heights Park" = "Okane Park",                     
                                 "Twin Lakes, Berkely, and West North Washington" = "Twin Lakes",          
                                 "Walnut Grove, Westbrook, Countryside and Crownpoint" = "Walnut Grove",    
                                 "West Greenwood Village" = "Greenwood Village",                                    
                                 "West Niwot" = "Niwot",                                           
                                 "Whittier(Denver County)" = "Whittier",                                   
                                 "Wyco Park, Northeast Northglenn" = "Wyco Park",                        
                                 "Applewood, Echo Hill, Rolling Hills and Meadows" = "Applewood",          
                                 "Cherry Hills Village Area" = "Cherry Hills Village",                          
                                 "Continental Divide Raceway, Dawson Butte and True Mountain" = "Continental Divide Raceway",
                                 "Cory-Merrill" = "Cory - Merrill",                                        
                                 "Derby, Southeast Thornton, and Southwest Commerce City" = "Derby",   
                                 "Federal Heights Federal Heights and Sherrelwood" = "Federal Heights",        
                                 "Fruitdale, Kipling and I-70 & Clearcreek" = "Fruitdale",               
                                 "Harriman Lake, Weaver Hollow Park and Blue Heron Park" = "Harriman Lake",   
                                 "Highlands, Prkway, Wood Creek and Hackbrhl" = "Highlands",          
                                 "Mapleton Hill, Newlands, and Juniper / Kalmia" = "Mapleton Hill",          
                                 "Martin Acres, Majestic Heights, and Superior" = "Martin Acres",          
                                 "Northeast Parker" = "Parker",                                      
                                 "Oakhurst Park, Sherwood Park" = "Oakhurst Park",                       
                                 "Platte Park" = "Platt Park",                                          
                                 "Skiline, Arvada Plaza and Terrace Park" = "Skiline",                  
                                 "Sloans Lake" = "Sloan Lake",                                   
                                 "South Marston Lake" = "Marston",                                   
                                 "Southeast Lafayette" = "Southeast Lafayette and Rock Creek Farm Park",                                    
                                 "University Hills(Denver County)" = "University Hills",                           
                                 "Wonderland, Boulder Meadows, and Catalpa Park" = "Wonderland",           
                                 "Yankee Doodle Park, Ralston Vally and Apex" = "Yankee Doodle Park"  
                 ))
avgcmt <- rename(avgcmt, "nhname"="name")

arb4 <- inner_join(missing, avgcmt, by = "nhname") # Rocky Mountain Arsenal is commute missing data 


load("ed.Rdata")

arb <- inner_join(ed, avgcmt, by = c("nhid", "nhname"))



# ..................................................................................................

# write the object as a csv for later use
write.csv(nbhd, "clean.nbhdcommutes.csv")
write.csv(avgcmt, "clean.avgcommute.csv")

rm(list = ls(pattern = "arb")) 
rm(list = ls(pattern = "commute")) 
rm(list = ls(pattern = "info")) 
rm(list = ls(pattern = "match"))
rm(list = ls(pattern = "nbhd"))
rm(list = ls(pattern = "nh"))
rm(list = ls(pattern = "travel"))
rm(list = ls(pattern = "na.check"))

save.image("commute.Rdata")
# ..................................................................................................

# TO DO LIST:


