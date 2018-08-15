
## C. McClintock 
## Shift Research Lab 
## Summer 2018 ## Updated: August 15th, 2018
## FamilyHOMES App: UI & Server

# ..................................................................................................

# set up: wd, retrieve data
rm(list=ls())
getwd()
# if need be 
# setwd("~/../../")
# setwd("Users/charmed33/R/")
# setwd("ShiftResearchLab2018/app/HOMEtool")

library(knitr)
library(tidyverse)
library(shiny)
library(plotly)
library(DT)
library(shinythemes)
library(markdown)
library(rsconnect)

# ..................................................................................................

# SET UP: MERGE DATA & FINAL CLEANING

load("wages.Rdata")
load("ed.Rdata")
load("commute.Rdata")

# load in the data
avgcommute <- select(avgcmt, nhname, avgcommute) # commute data
avgcommute <- rename(avgcommute, "nbhd"="nhname") # rename for merge
schools <- select(ed, nhname, schoolquality) # clean school data
schools <- rename(schools, "nbhd"="nhname") # rename for merge

# join school and commute time data
data <- left_join(schools, avgcommute, by="nbhd") 

# read in the parcel data
parcels <- read_csv("parcels.csv") %>% select(homevalue, nbhd, county) %>% filter(!nbhd=="Kennedy")

# count number of parcels per neighborhood
parcels.nbhd <- parcels %>% 
  group_by(nbhd) %>%
  count()
parcels.nbhd <- rename(parcels.nbhd, "ntotal"="n")

# get a df of nbhds matched to counties
parcels.county <- parcels %>% 
  group_by(county, nbhd) %>%
  count() %>% select(-n)

# count parcels by nbhd
county.count <- parcels %>% 
  group_by(county) %>%
  count() 
county.count <- rename(county.count, "ntotal"="n")

# read in a better df of nbhds matched to counties
n2c <- read.csv("nbhd2county.csv") %>% 
  group_by(county, nbhd)

# recode factors from the school and commute data to match the housing data
data <- mutate(data, 
               nbhd = fct_recode(nbhd, 
                                 "Apel Bacher Park, Koch Sub and Coulehan Grange" = "Apel Bacher Park",
                                 "Applewood, Echo Hill, Rolling Hills and Meadows" = "Applewood", 
                                 "Benedict Park, Central Brighton" = "Benedict Park",
                                 "Castle Pines Villige, Metes and Bounds" = "Castle Pines Villige", 
                                 "Chautauqua, Table Mesa, and Devil''s Thumb" = "Chautauqua", 
                                 "Cherry Hills Village Area" = "Cherry Hills Village", 
                                 "Continental Divide Raceway, Dawson Butte and True Mountain" = "Continental Divide Raceway", 
                                 "Cory-Merrill" = "Cory - Merrill",
                                 "Delmar Parkway, Jewell Heights, and Hoffman Heights" = "Delmar Parkway", 
                                 "Derby, Southeast Thornton, and Southwest Commerce City"  = "Derby", 
                                 "Federal Heights Federal Heights and Sherrelwood" = "Federal Heights", 
                                 "Fruitdale, Kipling and I-70 & Clearcreek" = "Fruitdale", 
                                 "Glendale Area" = "Glendale",
                                 "Glover, Plum Creek Fairway and Crystal Valley Ranch" = "Glover", 
                                 "Green Valley Ranch"  = "Gateway - Green Valley Ranch",
                                 "Gunbarrel Area" = "Gunbarrel", 
                                 "Harriman Lake, Weaver Hollow Park and Blue Heron Park" = "Harriman Lake", 
                                 "Heritage Hills and Stonegate" = "Stonegate", 
                                 "Highlands, Prkway, Wood Creek and Hackbrhl" = "Highlands", 
                                 "Hine Lake, Powderhorn Park and Westbury Park" = "Hine Lake", 
                                 "Kings Ridge, Noble Park, and East Boulder" = "Kings Ridge", 
                                 "Lakota Hills, Golden Proper" = "Lakota Hills", 
                                 "Louisville and Southeast Boulder Valley" = "Louisville", 
                                 "Mapleton Hill, Newlands, and Juniper / Kalmia" = "Mapleton Hill", 
                                 "Martin Acres, Majestic Heights, and Superior" = "Martin Acres", 
                                 "Okane Park, Washington Heights Park" = "Okane Park", 
                                 "Platte Park" = "Platt Park", 
                                 "Oakhurst Park, Sherwood Park" = "Oakhurst Park", 
                                 "Skiline, Arvada Plaza and Terrace Park" = "Skiline", 
                                 "Sloans Lake" = "Sloan Lake", 
                                 "Twin Lakes, Berkely, and West North Washington" = "Twin Lakes",
                                 "University Hills(Denver County)" = "University Hills", 
                                 "Walnut Grove, Westbrook, Countryside and Crownpoint" = "Walnut Grove", 
                                 "West Roxborough Park" = "Roxborough Park", 
                                 "Whittier(Denver County)" = "Whittier", 
                                 "Wonderland, Boulder Meadows, and Catalpa Park" = "Wonderland", 
                                 "Wyco Park, Northeast Northglenn" = "Wyco Park", 
                                 "Yankee Doodle Park, Ralston Vally and Apex" = "Yankee Doodle Park", 
                                 "Northwest Sheridan" = "Sheridan", 
                                 "South Marston Lake" = "Marston", 
                                 "Denver Airport" = "DIA", 
                                 "West Greenwood Village" = "Greenwood Village", 
                                 "East central Englewood" = "Englewood", 
                                 "Northeast Parker" = "Parker", 
                                 "West Niwot" = "Niwot", 
                                 "Highlands" = "Highlands Ranch West"
               ))

# list of nbhds
nbhd <- select(data, nbhd)

# check the fct_recode
check <- full_join(parcels.county, data, by="nbhd")

# wage and labor data
bls.spec <- arrange(bls.spec, industry, occ_title) %>% select(occ_title, a_pct10, a_pct25, a_median, a_pct75, a_pct90, industry)

bls.all <- select(bls.gen, industry, a_median) %>% rename("ind_median"="a_median")
bls.spec <- left_join(bls.spec, bls.all, by = "industry")


arb <- filter(check, is.na(county)) %>% select(county, nbhd)
arb$county[arb$nbhd == 'Athmar Park'] <- 'Denver County'
arb$county[arb$nbhd == 'Auraria'] <- 'Denver County'
arb$county[arb$nbhd == 'Baker'] <- 'Denver County'
arb$county[arb$nbhd == 'Belcaro'] <- 'Denver County'
arb$county[arb$nbhd == 'CBD'] <- 'Denver County'
arb$county[arb$nbhd == 'City Park'] <- 'Denver County'
arb$county[arb$nbhd == 'City Park West'] <- 'Denver County'
arb$county[arb$nbhd == 'Kennedy'] <- 'Denver County'
arb$county[arb$nbhd == 'North Capitol Hill'] <- 'Denver County'
arb$county[arb$nbhd == 'Valverde'] <- 'Denver County'
arb$county[arb$nbhd == 'S Federal Blvd and W Belleview Ave'] <- 'Arapahoe County'
arb$county[arb$nbhd == 'Lafayette'] <- 'Boulder County'
arb$county[arb$nbhd == 'Bow Mar'] <- 'Jefferson County'
arb$county[arb$nbhd == 'Federal Center'] <- 'Jefferson County'
arb$county[arb$nbhd == 'Rocky Mountain Arsenal'] <- 'Adams County'

parcels.county <- full_join(parcels.county, arb, by = c("nbhd", "county"))


# ..................................................................................................

# UI

ui <- shinyUI(fluidPage(theme = "bootstrap.css",
                        tags$p(" "),
                        
                        sidebarPanel(
                          tags$img(height = 85, src = "tool.png"),
                          br(),
                          tags$p("This tool allows users to explore housing affordability, school quality, 
                                 and  average commute time in the Denver metro area by household income. 
                                 For more information, or to see the code that produced this application, click", 
                                 tags$a(href = "https://github.com/charlottemcclintock/ShiftResearchLab2018", "here.")), 
                          htmlOutput("income_selector"),
  #                        htmlOutput("clt_deduction"),
                          tags$p("To use the version with industry and occupation inputs, click",
                                 tags$a(href = "https://charlottemccclintock.shinyapps.io/hometool", "here.")),
                          tags$a(href = "https://www.shiftresearchlab.org", tags$img(height = 90, src = "logo.png"))
                        ),
                        
                        mainPanel(tabsetPanel(type = "tabs",
                                              
                                              tabPanel("Plot", 
                                                       tags$h3("Housing Affordability and Access to Quality Education by Occupation"),
                                                       plotlyOutput("plot")), 
                                              tabPanel("About", 
                                                       tags$h3("About This Project"),
                                                       tags$p("This tool allows users to explore housing affordability, school quality, 
                                                              and  average commute time in the Denver metro area by industry and occupation. 
                                                              For more information, or to see the code that produced this application, click", 
                                                              tags$a(href = "https://github.com/charlottemcclintock/ShiftResearchLab2018", "here.")),  
                                                       tags$h5("Housing Affordability"),
                                                       tags$p("Parcel data was retrieved from the Adams, Arapahoe, Broomfield, Boulder, Denver, Douglas, 
                                                              and Jefferson County assessor's offices. The tool currently only uses parcel data for single 
                                                              family homes, but will be expanded soon. The housing affordability measure 
                                                              displays the percent of homes in each neighborhood for which the parcel value."),
                                                       tags$h5("School Quality"),
                                                       tags$p("School quality is determined by CMAS proficiency in reading and math for the three closest 
                                                              elementary, middle, and high schools. CMAS data was aggregated by school level 
                                                              (elementary, middle, high) for each neighborhood, then by neighborhood as a whole to create 
                                                              an average proficiency score for each school."),
                                                       tags$h5("Average Commute Time"),
                                                       tags$p("Average commute time is based on 5 year American Community Survey tract level data for 
                                                              average reported commute time. Tracts were then matched to neighborhoods with a matching table. 
                                                              Neighborhood averages were computed as the average of all matched tracts.")),
                                              tabPanel("Table",
                                                       tags$h3("Neighborhoods: Housing, Schools, and Commute Time"),
                                                       br(),
                                                       dataTableOutput("table"))
                                                       )
                                                       ) # close main panel 
                                                       ) # close fluid page
                        ) # close shinyUI

# ..................................................................................................

# SERVER

server <- shinyServer(function(input, output) {
  
  # ..................................................................................................
  
  # REACTIVE INPUT SELECTORS
  
  # INCOME SELECTOR
  output$income_selector <- renderUI({
    
    sliderInput(inputId = "income", 
                label = "Total Household Income:",
                min = 10000, 
                max = 250000, 
                value = 50000, 
                pre = "$")
  })
  
  output$clt_deduction <- renderUI({
    
    checkboxInput(inputId = "check", 
                  label = "CLT", 
                  value = FALSE,
                  width = NULL)
  })
  

  
  # ..................................................................................................
  
  # USEFUL REACTIVE FILTERED DATA SETS  
  
  # CREATE DATA FRAME FOR PLOT
  df <- reactive({
#    if (input$check==FALSE) {
            df <- parcels %>%
              filter(homevalue < 3*input$income) %>% 
              group_by(nbhd) %>%
              count() %>% 
              ungroup() %>% 
              rename("nless"="n") %>% left_join(parcels.nbhd, by = "nbhd") %>% 
              mutate(affordable = 100*nless/ntotal) %>% right_join(nbhd, by="nbhd") %>% 
              left_join(parcels.county, by = "nbhd")
            df$affordable <- ifelse(is.na(df$affordable), 0, df$affordable)
            df <- right_join(df, data, by = "nbhd")  
#            }
#    else {
#      parcels <- mutate(parcels,
#                        homevalue = homevalue-75000)
#      df <- parcels %>%
#        filter(homevalue < 3*input$income) %>% 
#        group_by(nbhd) %>%
#        count() %>% 
#        ungroup() %>% 
#        rename("nless"="n") %>% left_join(parcels.nbhd, by = "nbhd") %>% 
#        mutate(affordable = 100*nless/ntotal) %>% right_join(nbhd, by="nbhd") %>% 
#        left_join(parcels.county, by = "nbhd")
#      df$affordable <- ifelse(is.na(df$affordable), 0, df$affordable)
#      df <- right_join(df, data, by = "nbhd") 
#    }
  })
  
  # ..................................................................................................
  
  # OUTPUTS  
  
  # OUTPUT FULL DATA FRAME
  output$table <- renderDataTable({
    df <- as.data.frame(df()) %>% select(nbhd, county, schoolquality, affordable, avgcommute) %>% 
      arrange(nbhd)
    df$schoolquality <- round(df$schoolquality, 1)
    df$affordable <- round(df$affordable, 1)
    df$avgcommute <- round(df$avgcommute, 1)
    df <- rename(df, "Neighborhood" = "nbhd", "County" = "county", "School Quality" = "schoolquality", 
                 "Percent Affordable Housing" = "affordable", "Average Commute" = "avgcommute")
  })
  
  # ..................................................................................................
  
  # PLOT OUTPUT  
  output$plot <- renderPlotly({
      plot_ly(data = as.data.frame(df()), 
              x = ~schoolquality, 
              y = ~affordable, 
              type = 'scatter', 
              symbol = ~county,
              symbols = c(1:8),
              color = ~avgcommute, 
              colors = 'Blues', 
              mode = 'markers', 
              marker = list(
                size = 20
              ), 
              hoverinfo = 'text',
              text = ~paste(nbhd, '<br>', county,'<br>', round(affordable, 1), 
                            'percent of homes are affordable', 
                            '<br>', round(schoolquality,1), 
                            'percent grade level proficiency', 
                            '<br>', round(avgcommute,0), 
                            "minute average commute time"), width = 850) %>%
        layout(autosize = F, 
               xaxis = list(title = 'School Quality',   zeroline = FALSE),
               yaxis = list(title = 'Percent Affordable Housing'))  %>% 
        colorbar(title = "Average Commute", limits = c(5,45)) 
    
  })
  
})

# add duplex/condo housing data
# check school quality measure 

# ..................................................................................................

shinyApp(ui = ui, server = server)

# deploy to shinyapps.io
# deployApp()

