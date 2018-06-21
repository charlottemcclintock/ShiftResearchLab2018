
## C. McClintock 
## Shift Research Lab 
## Summer 2018 ## Updated: June 21, 2018 
## Housing Affordability Tool App: UI & Server

# ..................................................................................................

# set up: wd, retrieve data
rm(list=ls())
getwd()
# if need be setwd("~/../../") setwd("Users/charmed33/R/")
#
setwd("ShiftResearchLab2018/final-data")

library(knitr)
library(tidyverse)
library(shiny)
library(plotly)

load("wages.Rdata")
load("ed.Rdata")
load("commute.Rdata")
load("adams.Rdata")

avgcommute <- select(avgcmt, name, avgcommute)
avgcommute <- rename(avgcommute, "nbhd"="name")
schools <- select(all, nbhd, schoolquality)
affordable <- rename(aff, "nbhd"="nhname")

data <- left_join(schools, avgcommute, by="nbhd")
data <- left_join(data, affordable, by="nbhd")

ui <- shinyUI(fluidPage(
  titlePanel("Housing Affordability Tool"),
  
  sidebarPanel(
    htmlOutput("industry_selector"),
    htmlOutput("occupation_selector")
  ),
  mainPanel(plotlyOutput("plot"))
))

server <- shinyServer(function(input, output) {
  
  output$plot <- renderPlotly({
    plot_ly(data = subset(data, data$occupation==input$occupation), x = ~schoolquality, y = ~affordability, type = 'scatter', mode = 'markers',
            color = ~avgcommute, colors = 'Blues', marker = 
              list(size = ~avgcommute, opacity = 0.5), hoverinfo = 'text',
            text = ~paste('Neighborhood:', nbhd, '<br>Percent Affordable Housing', affordability,  '<br>School Quality (Percentile):', 
                          schoolquality)) %>%
      layout(title = 'Housing Affordability and School Quality by Occupation',
             xaxis = list(title = 'School Quality',
                          gridcolor = 'rgb(255, 255, 255)',
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwidth = 2),
             yaxis = list(title = 'Percent Affordable Housing',
                          gridcolor = 'rgb(255, 255, 255)',
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwidth = 2),
             paper_bgcolor = 'rgb(243, 243, 243)',
             plot_bgcolor = 'rgb(243, 243, 243)')
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


