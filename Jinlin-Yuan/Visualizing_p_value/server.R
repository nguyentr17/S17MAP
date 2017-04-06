function(input, output) {
  hx1 <- reactive({
    switch(input$population1,
           "normal" = {rnorm(5000, input$popu_mean1, input$sd1)}, 
           "skewed" = {rchisq(5000, df=4) + input$popu_mean1},
           "uniform" = {runif(5000, min = input$popu_mean1 - input$sd1, max = input$popu_mean1 + input$sd1)})
  })
  hx2 <- reactive({
    switch(input$population2,
           "normal" = {rnorm(5000, input$popu_mean2, input$sd2)},
           "skewed" = {rchisq(5000, df=4) + input$popu_mean2},
           "uniform" = {runif(5000, min = input$popu_mean2 - input$sd2, max = input$popu_mean2 + input$sd2)})
  })
  
  output$distPlot1 <- renderPlot({
    ## made change 1 for dynamic plotting
    low_range = min(input$popu_mean1-4*input$sd1, input$popu_mean2-4*input$sd2)
    high_range = max(input$popu_mean1+4*input$sd1, input$popu_mean2+4*input$sd2)
    height1 = max(0.4/input$sd1, 0.4/input$sd2)
    height2 = max(0.1*input$sd1, 0.1*input$sd2)
    half_width =  max(4*input$sd2, 4*input$sd1)
    #height2 = 0.4/input$sd2
    switch(input$population1,
           "normal" = {
             curve(dnorm(x, mean = input$popu_mean1, sd = input$sd1),
                   xlim=c(low_range,high_range),
                   ylim = c(0,height1), 
                   col="red",  lwd=2,  yaxt="n",
                   ylab="")},
           
           "skewed" = {
             curve(dchisq(x, df = 4), 
                   xlim=c(low_range,high_range), ylim = c(0,height2),
                   col="red", lwd=2,  yaxt="n")},
           
           "uniform" = {
             ##hx1 <- runif(500, min = input$popu_mean1 - input$sd1, max = input$popu_mean1 + input$sd1)
             curve(dunif(x, min = input$popu_mean1 - input$sd1, max = input$popu_mean1 + input$sd1),
                   xlim=c(-3,3), col="red", lwd=2,  yaxt="n")})
    par(new = TRUE);
    
    switch(input$population2,
           "normal" = {
             ##   hx2 <- rnorm(500, input$popu_mean2, input$sd2)
             curve(dnorm(x, mean = input$popu_mean2, sd = input$sd2), 
                   xlim=c(low_range,high_range),ylim = c(0,height1),
                   col="blue", lwd=2,   yaxt="n")},
           
           "skewed" = {
             ##   hx2 <- rchisq(500, df=4) + input$popu_mean2
             half_width = input$popu_mean2 + 4*input$sd2
             height = 0.4/input$sd2
             curve(dchisq(x, df = 4), 
                   xlim=c(low_range,high_range), ylim = c(0,height2),
                   col="blue", lwd=2,  yaxt="n")}, 
           
           "uniform" = {
             ## hx2 <- runif(500, min = input$popu_mean2 - input$sd2, max = input$popu_mean2 + input$sd2)
             curve(dunif(x, min = input$popu_mean2 - input$sd2, max = input$popu_mean2 + input$sd2), 
                   xlim=c(-3,3), col="blue", lwd=2,  yaxt="n")})
  })
  
  data <- reactive({
    d <- vector()
    for (i in 1:input$rep) {
      sp1 <- sample(hx1(), input$size1)
      sp2 <- sample(hx2(), input$size2)
      mean_diff <- round(mean(sp1) - mean(sp2), digits = 5)
      ttest <- t.test(sp1, sp2, paired = FALSE)
      tscore <- round(ttest$statistic, digits = 5)
      pval <- round(ttest$p.value, digits = 5)
      d <- cbind(d, c(mean_diff, tscore, pval))
    }
    return(d)
  })
  # output$test <- renderPrint(data())
  output$mean_diff1 <- renderPlot({
    data <- vector()
    data <- cbind(data, data())
    
    hist(data[1,], col = "plum", pch = 16, xlab = "", main = "Histogram of Mean Difference", breaks = 100)
    abline(v = input$popu_mean1 - input$popu_mean2, lwd = 2, col = "red")
    
 })
  
  output$mean_diff2 <- renderPlot({
    data <- vector()
    data <- cbind(data, data())
    
    hist(data[2,], col = "wheat1", pch = 16, xlab = "", main = "Histogram of t-statistics", breaks = 100)
    abline(v = (input$popu_mean1 - input$popu_mean2)/(sqrt(input$sd1 * input$sd1/input$size1 + input$sd2 * input$sd2/input$size2)), lwd = 2, col = "red")  
 })
  
  output$mean_diff3 <- renderPlot({
    data <- vector()
    data <- cbind(data, data())
    
    hist(data[3,], col= "palegreen", pch = 20, xlab = "", main = "Histogram of p-values", breaks = 100)
    abline(v = 0.05, lwd = 2, col = "red")
  })
}
