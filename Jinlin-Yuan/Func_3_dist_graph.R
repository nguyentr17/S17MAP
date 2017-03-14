mean_norm_dist <- function(s, mean1, mean2, sd1, sd2, n1, n2, hx1, hx2) {
  data <- vector()
  par(mar=c(2,2,2,2))
  par(mfrow = c(2,1))
  #print("Please select a distribution: ")
  #s <- readline()
  sp1 <- sample(hx1, n1)
  sp2 <- sample(hx2, n2)
    
  mean_diff <- round(mean(sp1) - mean(sp2), digits = 5)
  ttest <- t.test(sp1, sp2, paired = FALSE)
  tscore <- round(ttest$statistic, digits = 5)
  return(tscore)
  pval <- round(ttest$p.value, digits = 5)
  
  data <- c(mean_diff, tscore, pval)
  
  return(data)
  
  
}


three_plots <- function(reps, s, mean1, mean2, sd1, sd2, n1, n2) {
  data <- vector()
  
  if (s == "normal") {
    hx1 <- rnorm(500, mean1, sd1)
    hx2 <- rnorm(500, mean2, sd2)
    for (i in 1:reps) {
      data <- cbind(data, mean_norm_dist(s, mean1, mean2, sd1, sd2, n1, n2, hx1, hx2))
    }
  } else if (s == "skewed") {
    hx1 <- rchisq(n1, df=4) + mean1
    hx2 <- rchisq(n2, df=4) + mean2
    for (i in 1:reps) {
      data <- cbind(data, mean_norm_dist(s, mean1, mean2, sd1, sd2, n1, n2, hx1, hx2))
    }
  } else if (s == "uniform") {
    hx1 <- runif(n1, min = mean1 - sd1, max = mean1 + sd1)
    hx2 <- runif(n2, min = mean2 - sd2, max = mean2 + sd2)
    for (i in 1:reps) {
      data <- cbind(data, mean_norm_dist(s, mean1, mean2, sd1, sd2, n1, n2, hx1, hx2))
    }
  } else {
    print("Distribution not found.")
    return()
  }
  
  par(mfrow = c(3,2))
  hist(data[1,], col = "plum", pch = 16, main = "Dotplot of Mean Difference", breaks = 30)
  abline(v = mean1 - mean2, lwd = 2, col = "red" )
  plot(data[1,], seq_along(data[1,]), col = "plum", pch = 16, main = "Dotplot of t-statistics")
  abline(v = mean1 - mean2, lwd = 2, col = "red")
  
  hist(data[2,], col = "gold", pch = 16, main = "Dotplot of t-statistics", breaks = 30)
  abline(v = (mean1 - mean2)/(sqrt(sd1*sd1/n1 + sd2*sd2/n2)), lwd = 2, col = "red")  
  plot(data[2,], seq_along(data[2,]), col = "gold", pch = 16, main = "Dotplot of t-statistics")
  abline(v = (mean1 - mean2)/(sqrt(sd1*sd1/n1 + sd2*sd2/n2)), lwd = 2, col = "red")
  
  hist(data[3,], col= "skyblue", pch = 20, main = "Dotplot of p-values", breaks = 30)
  abline(v = 0.05, lwd = 2, col = "red")
  plot(data[3,], seq_along(data[3,]), col= "skyblue", pch = 20, main = "Dotplot of p-values")
  abline(v = 0.05, lwd = 2, col = "red")
  
  #plot(data[1,], col = "red", pch = 16, main = "Dotplot of t-statistics")
  #plot(data[2,], col = "blue", pch = 16, main = "Dotplot of t-statistics")
  #plot(data[3,], col= "green", pch = 20, main = "Dotplot of p-values")
  #return(data)
}


