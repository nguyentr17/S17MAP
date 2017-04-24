function(input, output) {
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  output$menu <- renderMenu({
    data <- filedata()
    if (is.null(data)) {
      return(NULL)
    } else {
      sidebarMenu(
        menuItem("Random", tabName = "item2", 
                 icon = icon("cog", lib = "glyphicon"),
                 sliderInput("slider", "Sample size:", 5, 516, 30))
      )
    }
  })
  
  output$text <- renderText({"
    <div style='padding: 0.5em 1em 0.5em 1em'>
    <b>Yuan Wang<b>
    </div>
    
    <div style='padding: 0.5em 1em 0.5em 1em'>
    <a>Grinnell College<a>
    </div>
    "})
  
  male <- reactive({
    data <- data()
    if (is.null(data)) {
      return(NULL)
    } else {
      male <- data1[data1$gender == 1,]$TimeUsedSec
      return(male)
    }
  })
  
  female <- reactive({
    data <- data()
    if (is.null(data)) {
      return(NULL)
    } else {
      female <- data1[data1$gender == 0,]$TimeUsedSec
      return(female)
    }
  })
  
}