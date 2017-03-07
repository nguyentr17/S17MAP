mean_norm_dist <- function(mean1, mean2, sd1, sd2, n1, n2) {
  data <- vector()
  par(mar=c(2,2,2,2))
  par(mfrow = c(2,1))
  print("Please select a distribution: ")
  s <- readline()
  if (s == "normal") {
    hx1 <- rnorm(n1, mean1, sd1)
    hx2 <- rnorm(n2, mean2, sd2)
    
    hist(hx1, prob = TRUE, main="Normal", cex.axis=.8, xlim=c(-5,5), breaks=20)
    curve(dnorm(x, mean = mean1, sd = sd1), 
          col="darkblue", lwd=2, add=TRUE, yaxt="n")
    
    hist(hx2, prob = TRUE, main="Normal", cex.axis=.8, xlim=c(-5,5), breaks=20)
    curve(dnorm(x, mean = mean2, sd = sd2), 
          col="darkblue", lwd=2, add=TRUE, yaxt="n")
    
  } else if (s == "skewed") {
    hx1 <- rchisq(n1, df=4) + mean1
    hx2 <- rchisq(n2, df=4) + mean2
    
    hist(hx1, prob = TRUE, main="Skewed", cex.axis=.8, xlim=c(0,12), breaks=20)
    curve(dchisq(x, df = 4), 
          col="darkblue", lwd=2, add=TRUE, yaxt="n")
    
    hist(hx2, prob = TRUE, main="Skewed", cex.axis=.8, xlim=c(0,12), breaks=20)
    curve(dchisq(x, df = 4), 
          col="darkblue", lwd=2, add=TRUE, yaxt="n")
    
  } else if (s == "uniform") {
    hx1 <- runif(n1, min = mean1 - sd1, max = mean1 + sd1)
    hx2 <- runif(n2, min = mean2 - sd2, max = mean2 + sd2)
    
    hist(hx1, prob = TRUE, main="Uniform", cex.axis=.8, xlim=c(-3,3), breaks=20)
    curve(dunif(x, min = mean1 - sd1, max = mean1 + sd1), 
          col="darkblue", lwd=2, add=TRUE, yaxt="n")
    
    hist(hx2, prob = TRUE, main="Uniform", cex.axis=.8, xlim=c(-3,3), breaks=20)
    curve(dunif(x, min = mean2 - sd2, max = mean2 + sd2), 
          col="darkblue", lwd=2, add=TRUE, yaxt="n")
    
    
  } else {
    print("Distribution not found.")
    return()
  }
  mean_diff <- mean(hx1) - mean(hx2)
  zscore <- (mean(hx1) - mean(hx2))/(sqrt( ((sd1)^2)/(n1) + ((sd2)^2)/(n2) ))
  pval <- round(t.test(hx1, hx2)$p.value, digits = 3)
  data <- c(mean_diff, zscore, pval)
  
  
  
  # Q: overlay the two graphs or show in a column?
  
  return(data)
  
  #plot(hx1, col="red", type = "h", xlab = "", ylab = "",
  #     main = "normal", axes = FALSE)
  
  #plot(hx2, col="green", type = "h", xlab = "", ylab = "",
  #     main = "normal", axes = FALSE)
  
}




