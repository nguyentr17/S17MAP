# Libraries
library(mosaic)
library(ggplot2)
library(manipulate)
library(rmarkdown)
library(knitr)
library(dplyr)

# Filter data frame to only untimed data with timeUsed != 0
shape <- Original
shape_1 <- mutate(shape, logTimeUsed = log(shape$timeUsed))
shape_untime <- shape_1[shape_1$requestedTime==0,]
shape_untimed <- shape_untime[shape_untime$timeUsed!=0,]

# Change the unit of timeUsed from millisecond to second
shape_untimed <- mutate(shape_untimed, timeUsedSec = shape_untimed$timeUsed/1000)

# Change the numerical independent var to factors
shape_untimed$numShapes = as.factor(shape_untimed$numShapes)
shape_untimed$timerDisplay = as.factor(shape_untimed$timerDisplay)

# Filter out gender data

cond <- tolower(strtrim(shape_untimed$v1value,1))=="m" &
              tolower(strtrim(shape_untimed$v2value,1))=="f"

v1_gender <- filter(shape_untimed, (tolower(strtrim(shape_untimed$v1label,3))=="gen" |
                             tolower(strtrim(shape_untimed$v1label,3))=="sex" | 
                             tolower(strtrim(shape_untimed$v1label,4))=="male" | 
                             tolower(strtrim(shape_untimed$v1label,1))=="f" ) &
                      
                             strtrim(shape_untimed$v1value,1) != 2 & 
                             strtrim(shape_untimed$v1value,1) != 1 &
                             strtrim(shape_untimed$v1value,1) != 0 &
                      
                             shape_untimed$v1value != "MorF" &
                             shape_untimed$v1value != "cat" &
                             shape_untimed$v1value != "Yes" &
                             shape_untimed$v1value != "Attempt" & !cond)

v1_gender2 <- mutate(v1_gender, gender = ifelse(tolower(strtrim(v1_gender$v1value,1))=="m", 1, 0))

