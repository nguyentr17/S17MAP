install.packages("RCurl")
library(RCurl)
baseURL <- "http://statgames.tietronix.com/statisticallygrounded/webreporter.php?rid=1969&cname=Sales%20Data"
baseURL1 <- "http://statgames.tietronix.com/statisticallygrounded/webreporter.php?rid="
baseURL2 <- "&cname=Sales%20Data"

coffee_truck_dataset <- data.frame()
system.time(for (i in 1:1969) {
  tempURL = paste0(baseURL1,i,baseURL2)
  dat = readLines(tempURL)
  if (length(dat) >= 6 & dat[7] == "") {
    out <- read.csv(textConnection(dat[7:(length(dat) - 1)])) #cut an empty row at the end
    for (line in 1:6) {
      temp <- unlist(strsplit(dat[line], ","))
      out[,temp[1]] <- temp[2] 
    }
    coffee_truck_dataset <- rbind(out,coffee_truck_dataset)
  }
})
### System time: 822 seconds = 14 minutes


