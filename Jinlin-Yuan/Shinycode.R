##Shiny graph
library(shiny)
shape <- read.csv("/Users/apple/Desktop/MAP/Original.csv")
shape_1 <- mutate(shape, logTimeUsed = log(shape$timeUsed))
shape_untime = shape_1[shape_1$requestedTime==0,]
shape_untimed = shape_untime[shape_untime$timeUsed!=0,]

ui <- fluidPage(
  pageWithSidebar(
    headerPanel('Plotting of Shapeplosion Game 
                   Performances with Unlimited Time'),
    sidebarPanel(
      selectInput('xval1', 'Independent Variable1',
                  names(shape_untimed)[c(10,11,14,17)]),
                 
      electInput('xval2', 'Independent Variable2',
                 names(shape_untimed)[c(10,11,14,17)]),
      
      selectInput('yval', 'Response Variable', 
                  names(shape_untimed)[c(19,15)])
    ),
  mainPanel(plotOutput('analysis'))
  )
)

server <- function(input, output) {
  selected <- reactive({shape_untimed[,c(input$xval, input$yval)]})
  
  output$analysis <-renderPlot({
    plot(selected())
    ##attempting to draw mean
    means <- tapply(selected()[,c(input$yval)],
                    selected()[,c(input$xval)])
    points(means,col="red",pch=25)
  })
}

shinyApp(ui=ui, server = server)

