
## C. McClintock 
## Shift Research Lab 
## Summer 2018 ## Updated: May 22, 2018 
## Fetching and Cleaning Script: ACS Commute Time Data

# ..................................................................................................

# set up: wd, retrieve encrypted data
rm(list=ls())
getwd()

# set up: libraries
library(dplyr)
library(forcats)
library(tidyverse)
library(readxl)
library(tidyr)
library(acs)

# ..................................................................................................

denver.metro <- geo.make(state="WA", tract =c())
