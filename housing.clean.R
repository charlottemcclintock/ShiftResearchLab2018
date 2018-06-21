
## C. McClintock 
## Shift Research Lab 
## Summer 2018 ## Updated: May 22, 2018 
## Cleaning Script: Assessor Housing Data

# ..................................................................................................

# set up: wd, retrieve data
rm(list=ls())
getwd()
# if need be setwd("~/../../")
setwd("Users/charmed33/R/ShiftResearchLab2018/final-data")

# set up: libraries
library(dplyr)
library(forcats)
library(tidyverse)
library(readxl)
library(tidyr)

# ..................................................................................................

adams <- read_csv("adams_parcels.csv")

# get a sense of the data
str(adams)
summary(adams)

# select useful variables
adams <- select(adams, parcelnb, accttype, legaldesc, site_address, buildings, 
                totallandval, totalimpsval, totalacctval, nhname)

# explore possible factor variables
adams$accttype <- as.factor(adams$accttype) # all residential
adams$site_address <- as.factor(adams$site_address)

dim(adams) # 128774, 8
n_distinct(adams$parcelnb) # 128774
n_distinct(adams$site_address) # 128589

table(adams$buildings) # 125,742 have only 1 building

adams1 <- filter(adams, buildings==1)

check <- mutate(adams1, 
                check = totallandval+totalimpsval-totalacctval) # no significant differences
# ok to use total acct value

# ..................................................................................................

load("wages.Rdata")

wages <- select(bls.spec, industry, occ_title, a_median)
wages <- mutate(wages, 
                housing = 2.6*a_median)

parcels <- select(adams1, totalacctval, nhname)

df <- map2(wages$occ_title, wages$a_median, ~ parcels %>% transmute(!! .x :=  totalacctval < 2.6 * .y)) %>% bind_cols(parcels, .)
df$nhname <- as.factor(df$nhname)

df.nbhd <- df %>% 
  group_by(nhname) %>%
  count()

df.check <- df %>% group_by(nhname) %>% summarise()

# ..................................................................................................

df[,3:653] <- lapply(df[,3:653], as.numeric)

df$nhname <- as.factor(df$nhname)

aff <- df %>%
  group_by(nhname) %>% 
  summarise_all(funs(sum))

aff <- select(aff, - totalacctval)

df.nbhd <- df %>% 
  group_by(nhname) %>%
  count()

aff <- left_join(aff, df.nbhd, by="nhname")

aff <- with(aff, cbind(nhname, aff[!names(aff) %in% c("nhname", "n")]/n, n))

# library(data.table) # another way of doing it
# aff2 <- setDT(aff)[, lapply(.SD, `/`, aff$n), .SDcols = `Chief Executives`:`Material Moving Workers, All Other`]

aff <- gather(aff, `Chief Executives`:`Material Moving Workers, All Other`, key="occupation", value="affordability")

aff <- mutate(aff, 
              affordability = 100*affordability)

check <- filter(aff, is.na(aff$affordability))
n_distinct(check$occupation)
# ..................................................................................................


rm(list = ls(pattern = "adams")) 
rm(list = ls(pattern = "bls")) 
rm(list = ls(pattern = "df")) 
rm(list = ls(pattern = "parcels")) 
rm(list = ls(pattern = "wages"))
rm(list = ls(pattern = "check"))

# ..................................................................................................

save.image("adams.Rdata")





