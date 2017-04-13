library(shinydashboard)
library(ggplot2)

dashboardPage(
  dashboardHeader(title = "Visualizing two-sample t-test"),
  dashboardSidebar(  
    #sidebarPanel(
    
      sidebarMenuOutput("menu"),
      sliderInput("slider", "Number of trials:", 20, 5000, 1000)),
    
    
    dashboardBody(
      fluidRow(
        box(
          title = "Population Distribution", 
          width = 6,
          status = "primary",
          plotOutput("distPlot1", height = 300)
        ),
        
        box(
          title = "Sample Distribution", 
          width = 6,
          status = "primary",
          plotOutput("sample_dist", height = 300)
        )),
      
      fluidRow(
        column(width = 4,
          box(
            title = "Histogram of Mean Difference",
            width = NULL,
            status = "primary",
            plotOutput("mean_diff1", height = 250)
          ),
          box(
            title = "Number of breaks",
            width = NULL,
            sliderInput("slider1", "Number of breaks:", 5, 200, 100)
          )),
        
        column(width = 4,
          box(
            title = "Histogram of t-statistics",
            width = NULL,
            status = "primary",
            plotOutput("mean_diff2", height = 250)
          ),
          box(
            title = "Number of breaks",
            width = NULL,
            sliderInput("slider2", "Number of breaks:", 5, 200, 100)
          )),
        
        column(width = 4,
          box(
            title = "Histogram of p-values",
            width = NULL,
            status = "primary",
            plotOutput("mean_diff3", height = 250)
          ),
          box(
            title = "Number of breaks",
            width = NULL,
            sliderInput("slider3", "Number of breaks:", 5, 200, 100)
          ))
      )))
    


