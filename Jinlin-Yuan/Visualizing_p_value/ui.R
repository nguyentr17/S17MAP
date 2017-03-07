fluidPage(
  headerPanel("Visualizing sample mean difference"),
    sidebarPanel(
      
      selectInput(inputId='population1', 
                  label = 'population1',
                  choice = c('normal','skewed','uniform')
                  ),
      numericInput(inputId = 'mean1',
                label = 'Population mean', 
                0,
                min = -100, max = 100),
      
      numericInput(inputId = 'size1',
                label = 'Sample size',
                   100,
                min = -100, max = 100),
      numericInput(inputId = 'sd1',
                   label = 'Standard diviation',
                   10,
                min = -100, max = 100),
      selectInput(inputId='population2', 
                  label = 'population2',
                  choice = c('normal','skewed','uniform')
      ),
      numericInput(inputId = 'mean2',
                label = 'Population mean', 
                0,
                min = -100, max = 100),
      
      numericInput(inputId = 'size2',
                label = 'Sample size',
                100,
                min = -100, max = 100),
      numericInput(inputId = 'sd2',
                label = 'Standard diviation',
                10,
                min = -100, max = 100),
      
     
    
    mainPanel(
      plotOutput("distPlot")
    )
    ))


