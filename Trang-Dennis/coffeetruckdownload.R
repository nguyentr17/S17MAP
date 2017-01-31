install.packages("XML")
url <- "http://statgames.tietronix.com/statisticallygrounded/webreporter.php"
table <- readHTMLTable(url,which=1)
nrow(table)

install.packages("RCurl")
library(RCurl)
baseURL <- "http://statgames.tietronix.com/statisticallygrounded/webreporter.php?rid=1969&cname=Sales%20Data"
baseURL1 <- "http://statgames.tietronix.com/statisticallygrounded/webreporter.php?rid="
baseURL2 <- "&cname=Sales%20Data"
cofee_truck_dataset <- data.frame()
for (i in 30:1969) {
  tempURL = paste0(baseURL1,i,baseURL2)
  dat = readLines(tempURL) 
  out <- read.csv(textConnection(dat[7:(length(dat) - 1)])) #cut an empty row at the end
  for (line in 1:6) {
    temp <- unlist(strsplit(dat[line], ","))
    out[,temp[1]] <- temp[2] 
  }
  cofee_truck_dataset <- rbind(cofee_truck_dataset,out)
}