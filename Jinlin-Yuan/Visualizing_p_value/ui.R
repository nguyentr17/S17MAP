library(shinydashboard)
library(ggplot2)

dashboardPage(
  dashboardHeader(title = "Visualizing two-sample t-test", titleWidth = 280),
  dashboardSidebar(
    sidebarMenuOutput("menu"),
    htmlOutput("text")
  ),
  
  
  dashboardBody(
    fluidRow(
      box(
        title = "Populations Distribution", 
        width = 6,
        status = "primary",
        plotOutput("distPlot1", height = 300)
      ),
      
      box(
        title = "Samples Distribution", 
        width = 6,
        status = "primary",
        plotOutput("sample_dist", height = 300)
      )),
    
    fluidRow(
      tabBox(
        title = "",
        selected = "Mean Differences",
        tabPanel("Mean Differences",
                 plotOutput("mean_diff1", height = 250)),
        tabPanel("t-statistics",
                 plotOutput("mean_diff2", height = 250))
      ),
    # box(
    #    title = "Histogram of Mean Differences",
    #    width = 4,
    #    status = "primary",
    #    plotOutput("mean_diff1", height = 250)
    #  ),
      
    #  box(
    #   title = "Histogram of t-statistics",
    #    width = 4,
    #    status = "primary",
    #    plotOutput("mean_diff2", height = 250)
    #  ),
      
<<<<<<< HEAD
<<<<<<< HEAD
      numericInput(inputId = 'size2',
                   label = 'Sample size',
                   20,
                   min = -100, max = 100),
      numericInput(inputId = 'sd2',
                   label = 'Standard diviation',
                   10,
                   min = -100, max = 100),
      selectInput(inputId='rep', 

              label = 'Number of Samples Drawn',
              choice = c(100,200,500,1000,2000,5000))),
    
    
    mainPanel(
      plotOutput("distPlot1"),
      ## Made changes: plotted seperately

      plotOutput("mean_diff"),
      plotOutput("test_stats"),
      plotOutput("p_val"),
      plotOutput("distPlot3"),
     # verbatimTextOutput("test_stats"),
     textOutput("test")
    )
  ))
=======
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
=======
      box(
        title = "Histogram of p-values",
        width = 6,
        #width = NULL,
        status = "primary",
        plotOutput("mean_diff3", height = 250)
      )
    )
  )
)
>>>>>>> 809e641e900475fdcf32ebf179be9674888ef5a3

>>>>>>> 43713604d24d534567224cd0ccdfb3d365184f5a

    


