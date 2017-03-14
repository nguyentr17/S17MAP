#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(data.table)

ui <- dashboardPage(
  dashboardHeader(title = "Tangram Data"),
  dashboardSidebar(
    selectInput('id', "GroupName", choices = groupname ,selected = 'all'),
    selectInput('level', "Factor", choices = factorchoice ,selected = 'all')
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    )
  )
)