
## C. McClintock 
## Shift Research Lab 
## Summer 2018 ## Updated: June 21, 2018 
## Housing Affordability Tool App: UI & Server

# ..................................................................................................

# set up: wd, retrieve data
rm(list=ls())
getwd()
# if need be 
#setwd("~/../../") 
#setwd("Users/charmed33/R/") 
#setwd("ShiftResearchLab2018/app/FamilyHOMES")

library(knitr)
library(tidyverse)
library(shiny)
library(plotly)
library(DT)
library(shinythemes)
library(markdown)
library(rsconnect)

load("ed.Rdata")
load("commute.Rdata")

parcels <- read_csv("parcels.csv") %>% select(homevalue, nbhd)

avgcommute <- select(avgcmt, nhname, avgcommute) # load in the commute data
avgcommute <- rename(avgcommute, "nbhd"="nhname") # rename for merge
schools <- select(all, nbhd, schoolquality) # load in the clean school data
schools <- mutate(schools,
                  schoolquality=100*schoolquality)

data <- left_join(schools, avgcommute, by="nbhd") # join schools and commute time 
# 
missing <- filter(data, is.na(data$avgcommute))
# write.csv(missing, "missing.commute.csv")

parcels.nbhd <- parcels %>% 
  group_by(nbhd) %>%
  count()
parcels.nbhd <- rename(parcels.nbhd, "ntotal"="n")

ui <- shinyUI(fluidPage(theme = "bootstrap.css",
                        tags$p(" "),
                        sidebarPanel(
                          tags$img(height = 87, src = "tool.png"),
                          br(),
                          tags$p("With this tool, you can explore Denver metro area neighborhoods by housing affordability, school quality, and commute time. When you select your annual median income, the graph will display the percent of houses in each neighborhood that are affordable based on the income. Each neighborhood also has a school quality rating, determined based on the reading and math proficiency and graduation rate of the nearest schools. When you hover over a point, you’ll be able to see information about each neighborhood."),
                          htmlOutput("income_selector"),
                          tags$a(href = "https://www.shiftresearchlab.org", tags$img(height = 80, src = "logo.png"))
                        ),
                        mainPanel(tags$h3("Denver Area Housing Affordability and Access to Quality Education"),
                                  plotlyOutput("plot")
                        ) # close main panel 
) # close fluid page
) # close shinyUI

server <- shinyServer(function(input, output) {
  
  output$income_selector <- renderUI({
    
    sliderInput(inputId = "income", 
                label = "Select Your Household Income:",
                min = 0, 
                max = 350000, 
                value = 25000)
    
  })
  df <- reactive(
    df <- parcels %>%
      filter(homevalue < 2.6*input$income) %>% 
      group_by(nbhd) %>%
      count() %>% 
      ungroup() %>% 
      rename("nless"="n") %>% left_join(parcels.nbhd, by = "nbhd")
    %>% mutate(affordable = 100*nless/ntotal) %>% 
      left_join(data, by="nbhd")
  )
  
  
  
  output$plot <- renderPlotly({
    plot_ly(data = as.data.frame(df()), 
            x = ~schoolquality, 
            y = ~affordable, 
            type = 'scatter', 
            size = ~avgcommute,
            color = ~avgcommute, 
            colors = 'Blues',
            mode = 'markers', 
            marker = list(
                        sizeref=0.2,
                        sizemode='area'), 
            hoverinfo = 'text',
            text = ~paste(nbhd, '<br>', round(affordable, 1), 'percent of homes are affordable',  '<br>', round(schoolquality,1), 'percentile for Denver area schools', '<br>', round(avgcommute,0), "minute average commute time")) %>%
      layout(autosize = F, width = 850, height = 550,
             xaxis = list(title = 'School Quality',   zeroline = FALSE),
             yaxis = list(title = 'Percent Affordable Housing'))  %>% colorbar(title = "Average Commute Time")
  })
})
##
shinyApp(ui = ui, server = server)

# deployApp()

