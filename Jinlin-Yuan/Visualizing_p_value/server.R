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
    half_width =  4*input$sd2
    height2 = 0.4/input$sd2
    switch(input$population1,
           "normal" = {
             curve(dnorm(x, mean = input$popu_mean1, sd = input$sd1),
                   xlim=c(low_range,high_range),
                   ylim = c(0,height), 
                   col="red",  lwd=2,  yaxt="n")
                   abline(v = 0)},
          
           "skewed" = {
             curve(dchisq(x, df = 4), 
                   xlim=c(low_range,high_range), ylim = c(0,height),
                   col="red", lwd=2,  yaxt="n")},
          
           "uniform" =  {
             ##hx1 <- runif(500, min = input$popu_mean1 - input$sd1, max = input$popu_mean1 + input$sd1)
             curve(dunif(x, min = input$popu_mean1 - input$sd1, max = input$popu_mean1 + input$sd1),
                   xlim=c(-3,3), col="red", lwd=2,  yaxt="n")})
    par(new = TRUE);
    
    switch(input$population2,
           "normal" = {
             ##   hx2 <- rnorm(500, input$popu_mean2, input$sd2)
             curve(dnorm(x, mean = input$popu_mean2, sd = input$sd2), 
                   xlim=c(low_range,high_range),ylim = c(0,height),
                   col="blue", lwd=2,   yaxt="n")},
           
           "skewed" = {
             ##   hx2 <- rchisq(500, df=4) + input$popu_mean2
             half_width = input$popu_mean2 + 4*input$sd2
             height = 0.4/input$sd2
             curve(dchisq(x, df = 4), 
                   xlim=c(low_range,high_range), ylim = c(0,height),
                   col="blue", lwd=2,  yaxt="n")}, 
           
           "uniform" = {
             ## hx2 <- runif(500, min = input$popu_mean2 - input$sd2, max = input$popu_mean2 + input$sd2)
             curve(dunif(x, min = input$popu_mean2 - input$sd2, max = input$popu_mean2 + input$sd2), 
                   xlim=c(-3,3), col="blue", lwd=2,  yaxt="n")})
})

    data <- reactive({
      d <- vector()
    for (i in 1:input$rep) {
      sp1 <- sample(hx1(), input$size1, replace = FALSE, prob = NULL)
      sp2 <- sample(hx2(), input$size2, replace = FALSE, prob = NULL)
      mean_diff <- round(mean(sp1) - mean(sp2), digits = 5)
      ttest <- t.test(sp1,sp2, paired = FALSE)
      tscore <- round(ttest$statistic, digits = 5)
      pval <- round(ttest$p.value, digits = 5)
      d <- cbind(d, c(mean_diff, tscore, pval))
    }
      return(d)
    })
     output$test <- renderPrint(data())
    output$mean_diff <- renderPlot({
      par(mfrow = c(3,2))
      hist(data()[1,], col = "plum", pch = 16, main = "Histogram of Mean Difference", breaks = 30)
      abline(v = mean1 - mean2, lwd = 2, col = "red")
      plot(data()[1,], seq_along(data[1,]), col = "plum", pch = 16, main = "Dotplot of t-statistics")
      abline(v = mean1 - mean2, lwd = 2, col = "red")
      
      hist(data()[2,], col = "wheat1", pch = 16, main = "Histogram of t-statistics", breaks = 30)
      abline(v = (mean1 - mean2)/(sqrt(sd1*sd1/n1 + sd2*sd2/n2)), lwd = 2, col = "red")  
      plot(data()[2,], seq_along(data[2,]), col = "wheat1", pch = 16, main = "Dotplot of t-statistics")
      abline(v = (mean1 - mean2)/(sqrt(sd1*sd1/n1 + sd2*sd2/n2)), lwd = 2, col = "red")
      
      hist(data()[3,], col= "palegreen", pch = 20, main = "Histogram of p-values", breaks = 30)
      abline(v = 0.05, lwd = 2, col = "red")
      plot(data()[3,], seq_along(data[3,]), col= "palegreen", pch = 20, main = "Dotplot of p-values")
      abline(v = 0.05, lwd = 2, col = "red")
      
      
    })

    #plot(data[1,], col = "red", pch = 16, main = "Dotplot of t-statistics")
    #plot(data[2,], col = "blue", pch = 16, main = "Dotplot of t-statistics")
    #plot(data[3,], col= "green", pch = 20, main = "Dotplot of p-values")
    #return(data)
  
  
   
    
    sp1 <- reactive({sample(hx1(), input$size1, replace = TRUE, prob = NULL)})
    sp2 <- reactive({sample(hx2(), input$size2, replace = TRUE, prob = NULL)})
    output$distPlot3 <- renderPlot({
  
      # Try histogram 
     # hist(sp1(), col = )
      #par(new = TRUE)
     # hist(sp2())
      ## Original 
      # first plot
   plot(seq_along(sp1()),sp1(), ylim=range(c(sp1(),sp2())), col = 'red', pch = 16, cex = 1.5)
      # second plot  EDIT: needs to have same ylim
      par(new = TRUE)
     plot(seq_along(sp2()),sp2(),ylim=range(c(sp1(),sp2())), axes = FALSE, xlab = "", ylab = "", col = 'blue', pch = 17, cex = 1.5)
    })
    output$test_stats <- renderPrint({
     t.test(sp1(),sp2())
    })
    output$mean_diff <- renderText({
      mean(sp1()) - mean(sp2())
      
    })
    
}
  
  
  ##  mean_diff <- mean(hx1) - mean(hx2)
  ##  zscore <- (mean(hx1) - mean(hx2))/(sqrt( ((sd1)^2)/(n1) + ((sd2)^2)/(n2) ))
  ##pval <- round(t.test(hx1, hx2)$p.value, digits = 3)
  ##data <- c(mean_diff, zscore, pval)
  
  
  
  # Q: overlay the two graphs or show in a column?
  

  #plot(hx1, col="red", type = "h", xlab = "", ylab = "",
  #     main = "normal", axes = FALSE)
  
  #plot(hx2, col="green", type = "h", xlab = "", ylab = "",
  #     main = "normal", axes = FALSE)
  





