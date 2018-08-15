
## C. McClintock 
## Shift Research Lab 
## Summer 2018 ## Updated: August 15th, 2018
## HOME Tool App: UI & Server

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
          tags$img(height = 130, src = "tool.png"),
          br(),
          htmlOutput("indicator"),
          tags$p("To begin, select an industry and occupation:"),
          htmlOutput("industry_selector"),
          htmlOutput("occupation_selector"), 
          htmlOutput("income_selector"),
          tags$a(href = "https://www.shiftresearchlab.org", tags$img(height = 90, src = "logo.png"))
        ),
        
        mainPanel(tabsetPanel(type = "tabs",
                              
          tabPanel("Plot", 
                   tags$h3("Housing Affordability and Access to Quality Education by Occupation"),
                   plotlyOutput("plot")), 
          
          tabPanel("Statistics", 
                   tags$head(tags$style(" #container * { display: inline; }")),
                  # TITLES
                   tags$h2(textOutput("occ_name")), 
                   tags$h4(textOutput("ind_name")),
                  # HOUSING STATISTICS
                  tags$h3("Housing"),
                  tags$li(div(id="container", tags$h5(textOutput("percent")), 
                              tags$p("of all houses in the Denver Metro area are affordable for this occupation."))),
                  tags$li(div(id="container", tags$h5(textOutput("affcount")), tags$p(" is the most affordable with "),
                              tags$h5(textOutput("affcount_perc")), tags$p("percent affordable housing."))),
                  tags$li(div(id="container", tags$h5(textOutput("naffcount")), tags$p(" is the least affordable with "),
                              tags$h5(textOutput("naffcount_perc")), tags$p("percent affordable housing."))),
                  # WAGE STATISTICS
                   tags$h3("Wages"),
                   tags$li(div(id="container", tags$h5(textOutput("income")), 
                       tags$p(" median annual income for an individual in the given occupation."))),
                   tags$li(div(id="container", 
                               tags$p("The range between 25th and 75th percentiles for the given occupation is "), 
                               tags$h5(textOutput("range")), ".")),
                   tags$li(div(id="container", tags$p("The median income for this occupation is "), 
                               tags$h5(textOutput("occ_percent")), tags$p(" of the industry median wage."))),
                  # OCCUPATIONAL STRUCTURE STATISTICS
                   tags$h3("Occupational Structure"),
                   tags$li(div(id="container", tags$h5(textOutput("jobs")), 
                               tags$p(" jobs in the given occupation per 1,000 jobs in the Denver Metro area."))),
                   tags$li(div(id="container", tags$p("The median income for the given occupation is "), 
                               tags$h5(textOutput("rel_percent")), tags$p(" of the Denver Metro area median income."))),
                   tags$li(div(id="container", tags$h5(textOutput("loc")), tags$p(", the ratio of the given occupation’s share of employment 
                                  in the Denver area to that occupation’s"), tags$br(), 
                               tags$p("   share of employment in the U.S. as a whole")))
                   
                  ),
          tabPanel("About", 
                   tags$h3("About This Project"),
                   tags$p("This tool allows users to explore housing affordability, school quality, 
                          and  average commute time in the Denver metro area by industry and occupation. 
                          For more information, or to see the code that produced this application, click", 
                          tags$a(href = "https://github.com/charlottemcclintock/ShiftResearchLab2018", "here.")),  
                   tags$strong(tags$h5("Occupational Wages")),
                   tags$p("Median annual occupation wage data comes from the Bureau of Labor Statistics, from the 2017 
                          calendar year, narrowed to the 'Denver-Aurora-Lakewood, CO' statistical area. A user can select
                          either industry alone for a broad perspective, or industry and occupation for more narrowly 
                          tailored results. Occupational income defaults to the annual minimum, with the range between the 
                          25th and 75th percentiles."),
                   tags$h5("Housing Affordability"),
                   tags$p("Parcel data was retrieved from the Adams, Arapahoe, Broomfield, Boulder, Denver, Douglas, 
                          and Jefferson County assessor's offices. The tool currently only uses parcel data for single 
                          family homes, but will be expanded soon.  The housing affordability measure 
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
                     dataTableOutput("table")),
          tabPanel("Density",
                   tags$h3("Density Plot of Home Values by County"),
                   plotlyOutput("density"))
                   )
                 ) # close main panel 
             ) # close fluid page
        ) # close shinyUI

# ..................................................................................................

# SERVER

server <- shinyServer(function(input, output) {

# ..................................................................................................
  
# REACTIVE INPUT SELECTORS
  
  # INDICATOR SELECTOR
  output$indicator <- renderUI({
    
    selectInput(
      inputId = "indicator", 
      label = "Primary Indicator:",
      choices = c("School Quality", "Commute Time"),
      selected = "School Quality",
      multiple = F)
    
  })
  
  # INDUSTRY SELECTOR
  output$industry_selector <- renderUI({
    
    selectInput(
      inputId = "industry", 
      label = "Industry:",
      choices = as.character(unique(bls.spec$industry)), # add an empty string c(" ", data) for no default
      selected = bls.spec[1,7], 
      multiple = F)
    
  })
  
  # OCCUPATION SELECTOR
  output$occupation_selector <- renderUI({
    
    available <- bls.spec[bls.spec$industry == input$industry, "occ_title"]
    
    selectInput(
      inputId = "occupation", 
      label = "Occupation:",
      choices = c("Industry Median Wage", unique(available)),
      selected = "Industry Median Wage", 
      multiple = F)
    
  })
  
  # INCOME SELECTOR
  output$income_selector <- renderUI({
    
    sliderInput(inputId = "income", 
                label = "Occupational Income:",
                min = inc()$a_pct25, 
                max = inc()$a_pct75, 
                value = inc()$a_median, 
                pre = "$") 
  })
  
# ..................................................................................................

# USEFUL REACTIVE FILTERED DATA SETS  
  
# REACTIVE INDUSTRY AND OCCUPATION PAIR
  occ <- reactive({
    if (input$occupation=="Industry Median Wage") {
      bls.gen %>% 
        select(industry, a_median) %>%
        filter(bls.gen$industry==input$industry)
    } else {
      bls.spec %>% 
        select(occ_title, a_median) %>%
        filter(bls.spec$occ_title==input$occupation)
    }
  })  
  
# INCOME RANGE DATA
  inc <- reactive({
    if (input$occupation=="Industry Median Wage") {
      bls.gen %>% 
        filter(bls.gen$industry==input$industry)
    } else {
      bls.spec %>% 
        filter(bls.spec$occ_title==input$occupation)
    }
  })  
  

  # USE ALL BLS DATA
  jobs <- reactive({
    if (input$occupation=="Industry Median Wage") {
      bls %>% 
        filter(bls$occ_title==input$industry)
    } else {
      bls %>% 
        filter(bls$occ_title==input$occupation)
    }
  }) 
  
  # CALCULATING TOTAL PERCENT AFFORDABLE  
  perc.aff <- reactive({
    perc.aff <- parcels %>%
      filter(homevalue < 3*input$income) %>% 
      count()
    perc.aff$n = perc.aff$n*100/732527
  })
  
# CREATE DATA FRAME FOR PLOT
  df <- reactive({
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
  })

# MOST AFFORDABLE COUNTY
county <- reactive({
  county <- parcels %>%
    filter(homevalue < 3*input$income) %>% 
    group_by(county) %>%
    count() %>% 
    ungroup() %>% 
    rename("nless"="n") %>% left_join(county.count, by = "county") %>% 
    mutate(affordable = 100*nless/ntotal) %>% arrange(affordable)
})    
  
# ..................................................................................................

# OUTPUTS  
  
# INPUT INDUSTRY AND OCCUPATION
output$ind_name <- renderText({ 
  input$industry })

output$occ_name<- renderText({ 
  input$occupation })

# INPUT OCCUPATION INCOME
output$income <- renderText({ 
  paste("$", prettyNum(as.data.frame(occ())[1,2], big.mark=",", preserve.width = "none"))
})

# PERCENT AFFORDABLE
output$percent <- renderText({
  paste(round(as.data.frame(perc.aff())[1,1], 1),"%")
})

# OCC INCOME AS A PERCENT OF INDUSTRY 
output$occ_percent <- renderText({
  if (input$occupation == "Industry Median Wage") {
    paste("100%")
  }
  else {
    paste(round((100*inc()$a_median/inc()$ind_median),1), "%")
  }
})

output$rel_percent <- renderText({
  paste(round((100*inc()$a_median/69153),1), "%")
})

# INPUT OCCUPATION INCOME RANGE
output$range <- renderText({ 
  paste("$", prettyNum(inc()$a_pct75-inc()$a_pct25, big.mark=",", preserve.width = "none"))
})

# JOBS PER 1000
output$jobs <- renderText({
  paste(round(as.data.frame(jobs())[1,10],1))
})

# LOCATION QUOTIENT
output$loc <- renderText({
  paste(round(as.data.frame(jobs())[1,11],1))
})

# MOST AFFORDABLE COUNTY NAME
output$affcount <- renderText({
  paste(as.data.frame(county())[7,1])
})
# MOST AFFORDABLE COUNTY PERCENT
output$affcount_perc <- renderText({
  paste(round(as.data.frame(county())[7,4],1))
})

# LEAST AFFORDABLE COUNTY NAME
output$naffcount <- renderText({
  paste(as.data.frame(county())[1,1])
})
# LEAST AFFORDABLE COUNTY PERCENT
output$naffcount_perc <- renderText({
  paste(round(as.data.frame(county())[1,4],1))
})
  
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
    
    # SCHOOL QUALITY ON THE X
    if (input$indicator=="School Quality") {
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
        colorbar(title = "Average Commute", limits = c(5,45)) } 
    # AVERAGE COMMUTE ON THE X
    else {
      plot_ly(data = as.data.frame(df()), 
              x = ~avgcommute, 
              y = ~affordable, 
              type = 'scatter', 
              symbol = ~county,
              symbols = c(1:8),
              color = ~schoolquality, 
              colors = 'Blues', 
              mode = 'markers', 
              marker = list(
                size = 20
              ), 
              hoverinfo = 'text',
              text = ~paste(nbhd, '<br>', county,'<br>', round(affordable, 1), 
                            'percent of homes are affordable', 
                            '<br>', round(avgcommute,0), 
                            "minute average commute time", 
                            '<br>', round(schoolquality,1), 
                            'percent grade level proficiency'), width = 850) %>%
        layout(autosize = F, 
               xaxis = list(title = 'Average Commute Time (Minutes)',   zeroline = FALSE),
               yaxis = list(title = 'Percent Affordable Housing'))  %>% 
        colorbar(title = "School Quality", limits = c(5,45))
    }
    
  })
  

  output$density <- renderPlotly({
    ggplotly(ggplot(parcels, aes(x=homevalue, fill=county)) + 
               geom_density(aes(alpha=.5)) + xlim(0, 1000000)) %>%
      layout(xaxis = list(title = 'Home Value (US$)'),
             yaxis = list(title = 'Density'))
             })
})

# add duplex/condo housing data
# check school quality measure 

# ..................................................................................................

shinyApp(ui = ui, server = server)

# deploy to shinyapps.io
# deployApp()

