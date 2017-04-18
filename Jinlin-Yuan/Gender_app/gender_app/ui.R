library(shinydashboard)
library(ggplot2)
library(shiny)

dashboardPage(skin = "green",
  dashboardHeader(
    title = "Shapesplosion: two sample t-test",
    titleWidth = 350),
  
  dashboardSidebar(
    #sidebarMenu(
    #  menuItem("Group ID", tabName = "group", icon = icon("dashboard"))
    #)
    fileInput('datafile', 'Choose CSV file',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    uiOutput("select1")
  ),
  
  dashboardBody(
    fluidRow(
      box(title = "Boxplot of gender",
          width = 8,
          solidHeader = TRUE,
          plotOutput("plot1", height = 500),
          status = "success"),
      valueBoxOutput("pval"),
      valueBoxOutput("num")
      
          
    )
  )
  )
  
  

