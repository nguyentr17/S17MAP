library(shinydashboard)
library(ggplot2)
library(shiny)
library(dplyr)

dashboardPage(
  dashboardHeader(
    title = "Visualizing gender p-value",
    titleWidth = 350
  ),
  
  dashboardSidebar(
    fileInput('datafile', 'Choose CSV file',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    menuItemOutput("menu"),
    htmlOutput("text")
  ),
  
  dashboardBody(
    
  )
)