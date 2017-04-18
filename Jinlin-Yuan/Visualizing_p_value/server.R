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
    ## made change 1 for dynamic plotting
    low_range = min(input$popu_mean1-4*input$sd1, input$popu_mean2-4*input$sd2)
    high_range = max(input$popu_mean1+4*input$sd1, input$popu_mean2+4*input$sd2)
    height1 = max(0.4/input$sd1, 0.4/input$sd2)
    height2 = max(0.1*input$sd1, 0.1*input$sd2)
    half_width =  max(4*input$sd2, 4*input$sd1)
    
    
    #height2 = 0.4/input$sd2
    switch(input$population1,
           "normal" = {
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
