
## C. McClintock 
## Shift Research Lab 
## Summer 2018 ## Updated: June 21, 2018 
## Housing Affordability Tool App: UI & Server

# ..................................................................................................

# set up: wd, retrieve data
rm(list=ls())
getwd()
# if need be setwd("~/../../") setwd("Users/charmed33/R/")
# setwd("ShiftResearchLab2018/app/FamilyHOMES")
library(knitr)
library(tidyverse)
library(shiny)
library(plotly)
library(DT)
library(shinythemes)
library(markdown)

load("ed.Rdata")
load("commute.Rdata")
load("adams.Rdata")

adams <- read_csv("adams_parcels.csv")
parcels <- select(adams, totalacctval, nhname)
parcels <- rename(parcels, "nbhd"="nhname")

avgcommute <- select(avgcmt, name, avgcommute) # load in the commute data
avgcommute <- rename(avgcommute, "nbhd"="name") # rename for merge
schools <- select(all, nbhd, schoolquality) # load in the clean school data
schools <- mutate(schools,
                  schoolquality=100*schoolquality)

data <- left_join(schools, avgcommute, by="nbhd") # join schools and commute time 

parcels.nbhd <- parcels %>% 
  group_by(nbhd) %>%
  count()
parcels.nbhd <- rename(parcels.nbhd, "ntotal"="n")

ui <- shinyUI(fluidPage(
                        sidebarPanel(
                          br(),
                          htmlOutput("income_selector") ),
                        mainPanel(mainPanel(tags$h3("Housing Affordability and Access to Quality Education by Occupation"),
                                            plotlyOutput("plot")
                        ))
                         # close main panel 
) # close fluid page
) # close shinyUI

server <- shinyServer(function(input, output) {
  
  output$income_selector <- renderUI({
    
    sliderInput(inputId = "income", 
                label = "Select Your Household Income:",
                min = 0, 
                max = 500000, 
                value = 25000)
    
  })
df <- reactive(
  df <- parcels %>%
    filter(totalacctval < 2.6*input$income) %>% 
    group_by(nbhd) %>%
    count() %>% 
    ungroup() %>% 
    rename("nless"="n") %>% left_join(parcels.nbhd, by = "nbhd")
    %>% mutate(affordable = nless/ntotal) %>% 
    left_join(data, by="nbhd")
)

  
  
  output$plot <- renderPlotly({
    plot_ly(data = as.data.frame(df()), x = ~schoolquality, y = ~affordable, type = 'scatter', mode = 'markers',
            color = ~avgcommute, colors = 'Blues', marker = 
              list(size = ~avgcommute, opacity = 0.5), hoverinfo = 'text',
            text = ~paste(nbhd, '<br>', round(affordable, 1), 'percent of homes are affordable',  '<br>', round(schoolquality,1), 'percentile for Denver area schools', '<br>', round(avgcommute,0), "minute average commute time")) %>%
      layout(autosize = F, width = 850, height = 550,
             xaxis = list(title = 'School Quality',   zeroline = FALSE),
             yaxis = list(title = 'Percent Affordable Housing'),
             colorbar = list(title = 'Average Commute Time'))  %>% colorbar(title = "Average Commute Time")
  })
})
##
shinyApp(ui = ui, server = server)


