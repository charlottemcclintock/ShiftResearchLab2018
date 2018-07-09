
## C. McClintock 
## Shift Research Lab 
## Summer 2018 ## Updated: June 21, 2018 
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


load("wages.Rdata")
load("ed.Rdata")
load("commute.Rdata")

avgcommute <- select(avgcmt, nhname, avgcommute) # load in the commute data
avgcommute <- rename(avgcommute, "nbhd"="nhname") # rename for merge
schools <- select(ed, nhname, schoolquality) # load in the clean school data
schools <- rename(schools, "nbhd"="nhname") # rename for merge

data <- left_join(schools, avgcommute, by="nbhd") # join schools and commute time

parcels <- read_csv("parcels.csv") %>% select(homevalue, nbhd)

parcels.nbhd <- parcels %>% 
  group_by(nbhd) %>%
  count()
parcels.nbhd <- rename(parcels.nbhd, "ntotal"="n")

bls.spec <- arrange(bls.spec, industry, occ_title)

ui <- shinyUI(fluidPage(theme = "bootstrap.css",
                        tags$p(" "),
                        sidebarPanel(
                          tags$img(height = 130, src = "tool.png"),
                          br(),
                          tags$p("This tool allows users to explore housing affordability, school quality, 
                                 and  average commute time in the Denver metro area by industry and occupation. 
                                 For more information, or to see the code that produced this application, click", 
                                 tags$a(href = "https://github.com/charlottemcclintock/ShiftResearchLab2018", "here.")),
                          tags$p("To begin, select an industry and occupation:"),
                          htmlOutput("industry_selector"),
                          htmlOutput("occupation_selector"), 
                          tags$a(href = "https://www.shiftresearchlab.org", tags$img(height = 90, src = "logo.png"))
                        ),
                        mainPanel(tags$h3("Housing Affordability and Access to Quality Education by Occupation"), 
                                  plotlyOutput("plot") 
                        ) # close main panel 
                        ) # close fluid page
) # close shinyUI

server <- shinyServer(function(input, output) {
  
  
  output$industry_selector <- renderUI({
    
    selectInput(
      inputId = "industry", 
      label = "Industry:",
      choices = as.character(unique(bls.spec$industry)), # add an empty string c(" ", data) for no default
      selected = bls.spec[1,28], 
      multiple = F)
    
  })
  
  output$occupation_selector <- renderUI({
    
    available <- bls.spec[bls.spec$industry == input$industry, "occ_title"]
    
    selectInput(
      inputId = "occupation", 
      label = "Occupation:",
      choices = c("Industry Median Wage",unique(available)),
      selected = "Industry Median Wage", 
      multiple = F)
    
  })
  
  x <- reactive({input$occupation})
  
  
  occ <- reactive({
    if (x()=="Industry Median Wage") {
      bls.gen %>% 
        select(industry, a_median) %>%
        filter(bls.gen$industry==input$industry)
    } else {
      bls.spec %>% 
        select(occ_title, a_median) %>%
        filter(bls.spec$occ_title==input$occupation)
    }
  })  
  
  df <- reactive({
    df <- parcels %>%
      filter(homevalue < 2.6*as.data.frame(occ())[1,2]) %>% 
      group_by(nbhd) %>%
      count() %>% 
      ungroup() %>% 
      rename("nless"="n") %>% left_join(parcels.nbhd, by = "nbhd") %>% 
      mutate(affordable = 100*nless/ntotal) %>% right_join(data, by = "nbhd")
  })
  
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
            text = ~paste(nbhd, '<br>', round(affordable, 1), 'percent of homes are affordable',  
                          '<br>', round(schoolquality,1), 'percent grade level proficiency', 
                          '<br>', round(avgcommute,0), "minute average commute time"), width = 850) %>%
      layout(autosize = F, 
             xaxis = list(title = 'School Quality',   zeroline = FALSE),
             yaxis = list(title = 'Percent Affordable Housing'))  %>% 
      colorbar(title = "Average Commute Time")
  })
  
})



##
shinyApp(ui = ui, server = server)

# deployApp()

