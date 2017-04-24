function(input, output) {
  hx1 <- reactive({
    switch(input$population1,
           "normal" = {rnorm(5000, input$popu_mean1, input$sd1)}, 
           "skewed" = {rchisq(5000, df = input$sd1, ncp = input$popu_mean1)},
           "uniform" = {runif(5000, min = input$popu_mean1 - input$sd1, max = input$popu_mean1 + input$sd1)})
  })
  hx2 <- reactive({
    switch(input$population2,
           "normal" = {rnorm(5000, input$popu_mean2, input$sd2)},
           "skewed" = {rchisq(5000, df = input$sd2, ncp = input$popu_mean2)},
           "uniform" = {runif(5000, min = input$popu_mean2 - input$sd2, max = input$popu_mean2 + input$sd2)})
  })
  
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Population 1", icon = NULL,
               menuSubItem(
                 icon = NULL,
                 selectInput(inputId='population1', 
                             label = 'population1',
                             choice = c('normal','skewed','uniform'))),
               menuSubItem(
                 icon = NULL,
                 numericInput(inputId = 'popu_mean1',
                              label = 'Population mean/ncp', 
                              0,
                              min = -100, max = 100)),
               menuSubItem(
                 icon = NULL,
                 numericInput(inputId = 'size1',
                              label = 'Sample size',
                              20,
                              min = -100, max = 100)),
               menuSubItem(
                 icon = NULL,
                 numericInput(inputId = 'sd1',
                              label = 'Std. deviation/df',
                              10,
                              min = -100, max = 100))
      ),
      menuItem("Population 2", icon = NULL,
               menuSubItem(
                 icon = NULL,
                 selectInput(inputId='population2', 
                             label = 'population2',
                             choice = c('normal','skewed','uniform'))),
               menuSubItem(
                 icon = NULL,
                 numericInput(inputId = 'popu_mean2',
                              label = 'Population mean/ncp', 
                              0,
                              min = -100, max = 100)),
               menuSubItem(
                 icon = NULL,
                 numericInput(inputId = 'size2',
                              label = 'Sample size',
                              20,
                              min = -100, max = 100)),
               menuSubItem(
                 icon = NULL,
                 numericInput(inputId = 'sd2',
                              label = 'Std. deviation/df',
                              10,
                              min = -100, max = 100))
      ))
  })
  
  
  output$distPlot1 <- renderPlot({
    low_range = min(input$popu_mean1-4*input$sd1, input$popu_mean2-4*input$sd2)
    high_range = max(input$popu_mean1+4*input$sd1, input$popu_mean2+4*input$sd2)
    height1 = max(0.4/input$sd1, 0.4/input$sd2)
    height2 = max(0.1*input$sd1, 0.1*input$sd2)
    half_width =  max(4*input$sd2, 4*input$sd1)
    
    
    #height2 = 0.4/input$sd2
    switch(input$population1,
           "normal" = {
<<<<<<< HEAD
             curve(dnorm(x, mean = input$popu_mean1, sd = input$sd1),
                   xlim=c(low_range,high_range),
<<<<<<< HEAD
                   ylim = c(0,height), 
                   ylab = "Frequency",
                   col="red",  lwd=2,  yaxt="n", legend = legend(5,0, # places a legend at the appropriate place 
                                                                 c("Health","Defense"), # puts text in the legend
                                                                 
                                                                 lty=c(1,1), # gives the legend appropriate symbols (lines)
                                                                 
                                                                 lwd=c(2.5,2.5),col=c("blue","red"))) # gives the legend lines the correct color and width)
                   },
          

           "skewed" = {
             curve(dchisq(x, df = 4), 
                   xlim=c(low_range,high_range), ylim = c(0,height),                   
                   ylab = "Frequency",
=======
                   ylim = c(0,height1), 
                   col="red",  lwd=2,  yaxt="n",
                   ylab="")},
           
           "skewed" = {
             curve(dchisq(x, df = 4), 
                   xlim=c(low_range,high_range), ylim = c(0,height2),
>>>>>>> fae5d0d9d674bc3d3a8e6facf2b6553fd2deba93
                   col="red", lwd=2,  yaxt="n")},
           
           "uniform" = {
             ##hx1 <- runif(500, min = input$popu_mean1 - input$sd1, max = input$popu_mean1 + input$sd1)
             curve(dunif(x, min = input$popu_mean1 - input$sd1, max = input$popu_mean1 + input$sd1),
                   ylab = "Frequency",
                   xlim=c(-3,3), col="red", lwd=2,  yaxt="n")})
    par(new = TRUE);
    switch(input$population2,
           "normal" = {
             curve(dnorm(x, mean = input$popu_mean2, sd = input$sd2), 
<<<<<<< HEAD
                   xlim=c(low_range,high_range),ylim = c(0,height),
                   ylab = "Frequency",
=======
                   xlim=c(low_range,high_range),ylim = c(0,height1),
>>>>>>> fae5d0d9d674bc3d3a8e6facf2b6553fd2deba93
                   col="blue", lwd=2,   yaxt="n")},
           
           "skewed" = {
             half_width = input$popu_mean2 + 4*input$sd2
             height = 0.4/input$sd2
             curve(dchisq(x, df = 4), 
<<<<<<< HEAD
                   xlim=c(low_range,high_range), ylim = c(0,height),
                   ylab = "Frequency",
=======
                   xlim=c(low_range,high_range), ylim = c(0,height2),
>>>>>>> fae5d0d9d674bc3d3a8e6facf2b6553fd2deba93
                   col="blue", lwd=2,  yaxt="n")}, 
           
           "uniform" = {
             ## hx2 <- runif(500, min = input$popu_mean2 - input$sd2, max = input$popu_mean2 + input$sd2)
             curve(dunif(x, min = input$popu_mean2 - input$sd2, max = input$popu_mean2 + input$sd2), 

                   xlim=c(-3,3), col="blue", lwd=2, ylab = "Frequency",
 yaxt="n")})
})




=======
             g <- ggplot(data.frame(x = c(low_range, high_range)), aes(x)) + 
               stat_function(fun = dnorm, args = list(mean = input$popu_mean1, sd = input$sd1), 
                             colour = "red")
           },
           
           "skewed" = {
             g <- ggplot(data.frame(x = c(low_range, high_range)), aes(x)) +
               stat_function(fun = dchisq, args = list(df = input$sd1, ncp = input$popu_mean1),
                             colour = "red")},
           #curve(dchisq(x, df = 4), 
           #       xlim=c(low_range,high_range), ylim = c(0,height2),
           #       col="red", lwd=2,  yaxt="n",
           #       ylab="")},
           
           "uniform" = {
             g <- ggplot(data.frame(x = c(low_range, high_range)), aes(x)) +
               stat_function(fun = dunif, 
                             args = list(min = input$popu_mean1 - input$sd1, max = input$popu_mean1 + input$sd1),
                             colour = "red")})
    
    #curve(dunif(x, min = input$popu_mean1 - input$sd1, max = input$popu_mean1 + input$sd1),
    #       xlim=c(-3,3), col="red", lwd=2,  yaxt="n", ylab="")})
    #par(new = TRUE);
    
    switch(input$population2,
           "normal" = {
             g + stat_function(fun = dnorm, args = list(mean = input$popu_mean2, sd = input$sd2),
                               colour = "blue")},
           
           "skewed" = {
             #curve(dchisq(x, df = 4), 
             #     xlim=c(low_range,high_range), ylim = c(0,height2),
             #          col="blue", lwd=2,  yaxt="n", ylab="")}, 
             g + stat_function(fun = dchisq, args = list(df = input$sd2, ncp = input$popu_mean2), 
                               colour = "blue")},
           
           "uniform" = {
             g + stat_function(fun = dunif, 
                               args = list(min = input$popu_mean2 - input$sd2, max = input$popu_mean2 + input$sd2),
                               colour = "blue")})
    
    #curve(dunif(x, min = input$popu_mean2 - input$sd2, max = input$popu_mean2 + input$sd2), 
    #       xlim=c(-3,3), col="blue", lwd=2,  yaxt="n", ylab="")})
  })
  
>>>>>>> 43713604d24d534567224cd0ccdfb3d365184f5a
  data <- reactive({
    d <- vector()
    for (i in 1:input$slider) {
      sp1 <- sample(hx1(), input$size1)
      sp2 <- sample(hx2(), input$size2)

      mean_diff <- round(mean(sp1) - mean(sp2), digits = 5)
      ttest <- t.test(sp1, sp2, paired = FALSE)
      tscore <- round(ttest$statistic, digits = 5)
      pval <- round(ttest$p.value, digits = 5)
      d <- cbind(d, c(mean_diff, tscore, pval, sp1, sp2))
    }
    return(d)
  })
<<<<<<< HEAD
<<<<<<< HEAD


      output$mean_diff <- renderPlot({
      par(mfrow = c(1,2))
      data <- vector()
      data <- cbind(data, data())
      
      hist(data[1,], col = "plum", pch = 16, xlab = "", main = "Histogram of Mean Difference", breaks = 100)
      abline(v = input$popu_mean1 - input$popu_mean2, lwd = 2, col = "red")
      plot(data[1,], seq_along(data[1,]), col = "plum", pch = 16, xlab = "", main = "Scatterplot of Mean Difference")
      abline(v = input$popu_mean1 - input$popu_mean2, lwd = 2, col = "red")
      })
    
    output$test_stats <- renderPlot({
      par(mfrow = c(1,2))
      data <- vector()
      data <- cbind(data, data())
      
      hist(data[2,], col = "wheat1", pch = 16, xlab = "", main = "Histogram of t-statistics", breaks = 100)
      abline(v = (input$popu_mean1 - input$popu_mean2)/(sqrt(input$sd1 * input$sd1/input$size1 + input$sd2 * input$sd2/input$size2)), lwd = 2, col = "red")  
      plot(data[2,], seq_along(data[2,]), col = "wheat1", pch = 16, xlab = "", main = "Scatterplot of t-statistics")
      abline(v = (input$popu_mean1 - input$popu_mean2)/(sqrt(input$sd1 * input$sd1/input$size1 + input$sd2 * input$sd2/input$size2)), lwd = 2, col = "red")
    })
    
    output$p_val <- renderPlot({
      par(mfrow = c(1,2))
      data <- vector()
      data <- cbind(data, data())
      
      hist(data[3,], col= "palegreen", pch = 20, xlab = "", main = "Histogram of p-values", breaks = 30)
      abline(v = 0.05, lwd = 2, col = "red")
      plot(data[3,], seq_along(data[3,]), col= "palegreen", pch = 20, xlab = "", main = "Scatterplot of p-values")
      abline(v = 0.05, lwd = 2, col = "red")

    })
    }
    


=======
=======
  
  output$sample_dist <- renderPlot({
    data <- vector()
    data <- cbind(data, data())
    samp1 <- data.frame(sample = (data[4,]))
    samp2 <- data.frame(sample = (data[5,]))
    samp1$pop <- "1"
    samp2$pop <- "2"
    h <- rbind(samp1, samp2)
    ggplot(h, aes(sample, fill = pop)) + geom_density(alpha = 0.2)
  })
  
>>>>>>> 43713604d24d534567224cd0ccdfb3d365184f5a
  # output$test <- renderPrint(data())
  output$mean_diff1 <- renderPlot({
    data <- vector()
    data <- cbind(data, data())
    
    hist(data[1,], col = "plum", pch = 16, xlab = "", ylab = "", main = NULL, breaks = input$slider1)
    abline(v = input$popu_mean1 - input$popu_mean2, lwd = 2, col = "red")
    
  })
  
  output$mean_diff2 <- renderPlot({
    data <- vector()
    data <- cbind(data, data())
    
    hist(data[2,], col = "wheat1", pch = 16, xlab = "", ylab = "", main = NULL, breaks = input$slider2)
    abline(v = (input$popu_mean1 - input$popu_mean2)/(sqrt(input$sd1 * input$sd1/input$size1 + input$sd2 * input$sd2/input$size2)), lwd = 2, col = "red")  
  })
  
  output$mean_diff3 <- renderPlot({
    data <- vector()
    data <- cbind(data, data())
    
    hist(data[3,], col= "palegreen", pch = 20, xlab = "", ylab = "", main = NULL, breaks = input$slider3)
    abline(v = 0.05, lwd = 2, col = "red")
  })
}
>>>>>>> fae5d0d9d674bc3d3a8e6facf2b6553fd2deba93
