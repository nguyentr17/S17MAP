fluidPage(
  headerPanel("Visualizing two-sample t-test"),
  sidebarLayout(  
  sidebarPanel(
    
      width = 3,
      
      selectInput(inputId='population1', 
                  label = 'population1',
                  choice = c('normal','skewed','uniform')
                  ),
      numericInput(inputId = 'popu_mean1',
                label = 'Population mean', 
                0,
                min = -100, max = 100),
      
      numericInput(inputId = 'size1',
                label = 'Sample size',
                   20,
                min = -100, max = 100),
      numericInput(inputId = 'sd1',
                   label = 'Standard diviation',
                   10,
                min = -100, max = 100),
      
      selectInput(inputId='population2', 
                  label = 'population2',
                  choice = c('normal','skewed','uniform')
      ),
      numericInput(inputId = 'popu_mean2',
                label = 'Population mean', 
                0,
                min = -100, max = 100),
      
      numericInput(inputId = 'size2',
                label = 'Sample size',
                20,
                min = -100, max = 100),
      numericInput(inputId = 'sd2',
                label = 'Standard diviation',
                10,
                min = -100, max = 100),
      selectInput(inputId='rep', 
              label = 'Number of Samples',
              choice = c(100,200,500,1000,2000,5000))),
     
    
    mainPanel(
      plotOutput("distPlot1"),
      ## Made changes: plotted seperately
      plotOutput("mean_diff1"),
      plotOutput("mean_diff2"),
      plotOutput("mean_diff3")
   #   plotOutput("distPlot3"),
     # verbatimTextOutput("test_stats"),
  #   textOutput("test")
    )
  
    ))


