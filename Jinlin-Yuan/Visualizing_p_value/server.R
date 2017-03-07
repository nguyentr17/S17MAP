function(input, output) {
   hx1 <- reactive({
     switch(input$population1,
            "normal" = {rnorm(500, input$popu_mean1, input$sd1)},
            "skewed" = {rchisq(500, df=4) + input$popu_mean1},
            "uniform" = {runif(500, min = input$popu_mean1 - input$sd1, max = input$popu_mean1 + input$sd1)})
   })
   hx2 <- reactive({
     switch(input$population2,
            "normal" = {rnorm(500, input$popu_mean2, input$sd2)},
            "skewed" = {rchisq(500, df=4) + input$popu_mean2},
            "uniform" = {runif(500, min = input$popu_mean2 - input$sd2, max = input$popu_mean2 + input$sd2)})
   })
  
  
  output$distPlot1 <- renderPlot({
    ## made change 1 for dynamic plotting
    low_range = min(input$popu_mean1-4*input$sd1, input$popu_mean2-4*input$sd2)
    high_range = max(input$popu_mean1+4*input$sd1, input$popu_mean2+4*input$sd2)
    height = 0.4/input$sd1

    switch(input$population1,
           "normal" = {
          ##   hx1 <- rnorm(500, input$popu_mean1, input$sd1)
             hist(hx1(), prob = TRUE, main="Normal", cex.axis=.8, 
                  ## made changes 2
                  xlim=c(low_range,high_range),
                  ylim = c(0,height), 
                  breaks=5)
             curve(dnorm(x, mean = input$popu_mean1, sd = input$sd1), 
                   col="red", lwd=2, add=TRUE, yaxt="n")
           },
           "skewed" = {
             ##hx1 <- rchisq(500, df=4) + input$popu_mean1
             hist(hx1(), prob = TRUE, main="Skewed", cex.axis=.8, xlim=c(low_range,high_range), ylim = c(0,height),breaks=5)
             curve(dchisq(x, df = 4), 
                   col="red", lwd=2, add=TRUE, yaxt="n")     
           },
           "uniform" =  {
             ##hx1 <- runif(500, min = input$popu_mean1 - input$sd1, max = input$popu_mean1 + input$sd1)
             hist(hx1(), prob = TRUE, main="Uniform", cex.axis=.8, xlim=c(-3,3), breaks=5)
             curve(dunif(x, min = input$popu_mean1 - input$sd1, max = input$popu_mean1 + input$sd1), 
                   col="red", lwd=2, add=TRUE, yaxt="n")
           }
           
    )
    output$distPlot2 <- renderPlot({
      half_width =  4*input$sd1
      height = 0.4/input$sd1
      switch(input$population2,
             "normal" = {
            ##   hx2 <- rnorm(500, input$popu_mean2, input$sd2)
               half_width = input$popu_mean2 + 4*input$sd2
               height = 0.4/input$sd2
               hist(hx2(), prob = TRUE, main="Normal", cex.axis=.8, xlim=c(low_range,high_range),ylim = c(0,height), breaks=5)
               curve(dnorm(x, mean = input$popu_mean2, sd = input$sd2), 
                     col="blue", lwd=2, add=TRUE, yaxt="n")},
             
             "skewed" = {
            ##   hx2 <- rchisq(500, df=4) + input$popu_mean2
               half_width = input$popu_mean2 + 4*input$sd2
               height = 0.4/input$sd2
               hist(hx2(), prob = TRUE, main="Skewed", cex.axis=.8, xlim=c(low_range,high_range), ylim = c(0,height),breaks=5)
               curve(dchisq(x, df = 4), 
                     col="blue", lwd=2, add=TRUE, yaxt="n")}, 
             
             "uniform" = {
              ## hx2 <- runif(500, min = input$popu_mean2 - input$sd2, max = input$popu_mean2 + input$sd2)
               hist(hx2(), prob = TRUE, main="Uniform", cex.axis=.8, xlim=c(-3,3), breaks=5)
               curve(dunif(x, min = input$popu_mean2 - input$sd2, max = input$popu_mean2 + input$sd2), 
                     col="blue", lwd=2, add=TRUE, yaxt="n")}
      )
    })
    
    output$distPlot3 <- renderPlot({
      sp1 <- sample(hx1(), input$size1, replace = TRUE, prob = NULL)
      sp2 <- sample(hx2(), input$size2, replace = TRUE, prob = NULL)
      # first plot
      plot(sp1, seq_along(sp1), xlim=range(c(sp1,sp2)), col = 'red', pch = 16, cex = 1.5)
      # second plot  EDIT: needs to have same ylim
      par(new = TRUE)
      plot(sp2, seq_along(sp2),xlim=range(c(sp1,sp2)), axes = FALSE, xlab = "", ylab = "", col = 'blue', pch = 17, cex = 1.5)
    })
    
   
   ## output$test_stats <- renderDataTable(expr, options = NULL, searchDelay = 500,
     ##               callback = "function(oTable) {}", escape = TRUE, env = parent.frame(),
       ##             quoted = FALSE, outputArgs = list())
  })
  
  
  ##  mean_diff <- mean(hx1) - mean(hx2)
  ##  zscore <- (mean(hx1) - mean(hx2))/(sqrt( ((sd1)^2)/(n1) + ((sd2)^2)/(n2) ))
  ##pval <- round(t.test(hx1, hx2)$p.value, digits = 3)
  ##data <- c(mean_diff, zscore, pval)
  
  
  
  # Q: overlay the two graphs or show in a column?
  

  #plot(hx1, col="red", type = "h", xlab = "", ylab = "",
  #     main = "normal", axes = FALSE)
  
  #plot(hx2, col="green", type = "h", xlab = "", ylab = "",
  #     main = "normal", axes = FALSE)
  
}




