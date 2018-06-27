
## C. McClintock 
## Shift Research Lab 
## Summer 2018 ## Updated: June 21, 2018 
## Housing Affordability Tool App: UI & Server

# ..................................................................................................

# set up: wd, retrieve data
rm(list=ls())
getwd()
# if need be setwd("~/../../") setwd("Users/charmed33/R/")
# setwd("ShiftResearchLab2018/app/HousingAffordabilityTool")

library(knitr)
library(tidyverse)
library(shiny)
library(plotly)
library(DT)
library(shinythemes)
library(markdown)

load("wages.Rdata")
load("ed.Rdata")
load("commute.Rdata")
load("adams.Rdata")

avgcommute <- select(avgcmt, name, avgcommute) # load in the commute data
avgcommute <- rename(avgcommute, "nbhd"="name") # rename for merge
schools <- select(all, nbhd, schoolquality) # load in the clean school data
schools <- mutate(schools,
                  schoolquality=100*schoolquality)

affordable <- aff # load in the housing affordabiity data
affordable <- mutate(affordable,
                     affordability=100*affordability)
affordable <- rename(affordable, "nbhd"="nhname") # rename for merge

data <- left_join(schools, avgcommute, by="nbhd") # join schools and commute time 
data <- left_join(data, affordable, by="nbhd") # add affordable housing. 

ui <- shinyUI(fluidPage(
  sidebarPanel(
    tags$img(height = 110, src = "tool.png"),
    br(),
    tags$p("To begin, select an industry and occupation:"),
    htmlOutput("industry_selector"),
    htmlOutput("occupation_selector"), 
    tags$p("With this tool, you can explore Denver metro area neighborhoods by housing affordability, school quality, and commute time. When you select an occupation, the graph will display the percent of houses in each neighborhood that are affordable based on the median income in Denver for that occupation. Each neighborhood also has a school quality rating, determined based on the reading and math proficiency and graduation rate of the nearest schools. When you hover over a point, you’ll be able to see information about each neighborhood."),
    tags$a(href = "https://www.shiftresearchlab.org", tags$img(height = 80, src = "logo.png"))
  ),
  mainPanel(tags$h3("Housing Affordability and Access to Quality Education by Occupation"),
            plotlyOutput("plot")
  ) # close main panel 
) # close fluid page
) # close shinyUI

server <- shinyServer(function(input, output) {
  
  output$plot <- renderPlotly({
    plot_ly(data = subset(data, data$occupation==input$occupation), x = ~schoolquality, y = ~affordability, type = 'scatter', mode = 'markers',
            color = ~avgcommute, colors = 'Blues', marker = 
              list(size = ~avgcommute, opacity = 0.5), hoverinfo = 'text',
            text = ~paste(nbhd, '<br>', 'Based on your occupation,', round(affordability, 1), 'percent of houses are affordable',  '<br>Schools in this neighborhood are in the ', round(schoolquality,1), 'percentile for Denver area schools', '<br>Residents of this neighborhood have a ', round(avgcommute,0), "minutes average commute time")) %>%
      layout(autosize = F, width = 850, height = 550,
             xaxis = list(title = 'School Quality',   zeroline = FALSE),
             yaxis = list(title = 'Percent Affordable Housing'))
  })
  
  output$industry_selector <- renderUI({
    
    selectInput(
      inputId = "industry", 
      label = "Industry:",
      choices = as.character(unique(bls.spec$industry)),
      selected = "Management Occupations")
    
  })
  
  output$occupation_selector <- renderUI({
    
    available <- bls.spec[bls.spec$industry == input$industry, "occ_title"]
    
    selectInput(
      inputId = "occupation", 
      label = "Occupation:",
      choices = unique(available),
      selected = unique(available)[1])
    
  })
})
##
shinyApp(ui = ui, server = server)


