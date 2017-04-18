function(input, output) {
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  output$filetable <- renderTable({
    filedata()
  })
  
  output$select1 <- renderUI({
    data <- filedata()
    if (is.null(data)) {
      return(NULL)
    }
    selectizeInput("group", "Group ID:", choices = unique(data$groupID), selected = "all", multiple = TRUE)
  })
  
  output$plot1 <- renderPlot({
    data <- filedata()
    if (is.null(data)) {
      return(NULL)
    }
    data1 <- data[data$groupID %in% input$group,]
    data1$gender <- as.factor(data1$gender)
    TimeUsedSec <- data1$TimeUsedSec
    
    ggplot(data = data1, aes(x = gender, y = TimeUsedSec)) +
      geom_boxplot() +
      aes(colour = gender) +
      theme(legend.position="right") +
      labs(title="0 = Female, 1 = Male") +
      stat_summary(fun.y = mean, geom = "point", pch = 8, cex = 3)
  })
  
  output$pval <- renderValueBox({
    data <- filedata()
    if (is.null(data)) {
      valueBox("N/A", "P-value", color = "yellow")
    } else {
      data1 <- data[data$groupID %in% input$group,]
      female <- data1[data1$gender == 0,]$TimeUsedSec
      male <- data1[data1$gender == 1,]$TimeUsedSec
      
      p <- round(t.test(female, male)$p.value, digits = 3)
      
      if (p < 0.1) {
        valueBox(p, "P-value", icon = icon("thumbs-up", lib = "glyphicon"), color = "red")
      } else {
        valueBox(p, "P-value", color = "yellow")
      }
    }
  })
  
  output$num <- renderValueBox({
    data <- filedata()
    if (is.null(data)) {
      valueBox("N/A", "Sample size", color = "blue")
    } else {
      data1 <- data[data$groupID %in% input$group,]
      
      n <- dim(data1)[1]
      valueBox(n, "Sample size", color = "blue")
    }
  })
}