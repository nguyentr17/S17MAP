function(input, output) {
 
  output$distPlot <- renderPlot({
    par(mfrow = c(2,1))
    
    switch(input$population1,
           "normal" = {
             hx1 <- rnorm(input$size1, input$mean1, input$sd1)
             width = input$mean1 + 4*input$sd1
             hist(hx1, prob = TRUE, main="Normal", cex.axis=.8, xlim=c(-width,width), breaks=5)
             curve(dnorm(x, mean = input$mean1, sd = input$sd1), 
                   col="darkblue", lwd=2, add=TRUE, yaxt="n")
             },
           "skewed" = {
             hx1 <- rchisq(input$size1, df=4) + input$mean1
             width = input$mean1 + 4*input$sd1
             hist(hx1, prob = TRUE, main="Skewed", cex.axis=.8, xlim=c(-width,width), breaks=5)
             curve(dchisq(x, df = 4), 
                   col="darkblue", lwd=2, add=TRUE, yaxt="n")     
           },
           "uniform" =  {
             hx1 <- runif(input$size1, min = input$mean1 - input$sd1, max = input$mean1 + input$sd1)
             
             hist(hx1, prob = TRUE, main="Uniform", cex.axis=.8, xlim=c(-3,3), breaks=5)
             curve(dunif(x, min = input$mean1 - input$sd1, max = input$mean1 + input$sd1), 
                   col="darkblue", lwd=2, add=TRUE, yaxt="n")
           })
    
    switch(input$population2,
           "normal" = {
             hx2 <- rnorm(input$size2, input$mean2, input$sd2)
             width = input$mean2 + 4*input$sd2
             hist(hx2, prob = TRUE, main="Normal", cex.axis=.8, xlim=c(-width,width), breaks=5)
             curve(dnorm(x, mean = input$mean2, sd = input$sd2), 
                   col="darkblue", lwd=2, add=TRUE, yaxt="n")},
     
           "skewed" = {
             hx2 <- rchisq(input$size2, df=4) + input$mean2
             width = input$mean2 + 4*input$sd2
             hist(hx2, prob = TRUE, main="Skewed", cex.axis=.8, xlim=c(-width,width), breaks=5)
             curve(dchisq(x, df = 4), 
                   col="darkblue", lwd=2, add=TRUE, yaxt="n")}, 
           
           "uniform" = {
             hx2 <- runif(input$size2, min = input$mean2 - input$sd2, max = input$mean2 + input$sd2)
             
             hist(hx2, prob = TRUE, main="Uniform", cex.axis=.8, xlim=c(-3,3), breaks=5)
             curve(dunif(x, min = input$mean2 - input$sd2, max = input$mean2 + input$sd2), 
                   col="darkblue", lwd=2, add=TRUE, yaxt="n")}
    )
  })
  
  
  ##  mean_diff <- mean(hx1) - mean(hx2)
  ##  zscore <- (mean(hx1) - mean(hx2))/(sqrt( ((sd1)^2)/(n1) + ((sd2)^2)/(n2) ))
  ##pval <- round(t.test(hx1, hx2)$p.value, digits = 3)
  ##data <- c(mean_diff, zscore, pval)
  
  
  
  # Q: overlay the two graphs or show in a column?
  
  return(data)
  
  #plot(hx1, col="red", type = "h", xlab = "", ylab = "",
  #     main = "normal", axes = FALSE)
  
  #plot(hx2, col="green", type = "h", xlab = "", ylab = "",
  #     main = "normal", axes = FALSE)
  
}




