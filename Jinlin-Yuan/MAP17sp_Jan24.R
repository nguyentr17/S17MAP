# Libraries
library(mosaic)
library(ggplot2)
library(manipulate)
library(rmarkdown)
library(knitr)
library(dplyr)
library(gridExtra)


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

# View all labels
label1_count <- count_(shape,"v1label")
label2_count <- count_(shape,"v2label")
label3_count <- count_(shape,"v3label")

groupID_count <- count_(shape,"groupID")

# Filter data by groupID (top 5 most used group IDs)
shape_HotchkissStudent16 <- shape_untimed[shape_untimed$groupID=="HotchkissStudent16",]
shape_6002T6 <- shape_untimed[shape_untimed$groupID=="6002T6",]
shape_6002T2 <- shape_untimed[shape_untimed$groupID=="6002T2",]
shape_6002T3 <- shape_untimed[shape_untimed$groupID=="6002T3",]
shape_6002T4 <- shape_untimed[shape_untimed$groupID=="6002T4",]

# Trim data by label

v1_music <- shape_untimed[tolower(strtrim(shape$v1label,2))=="mu" | 
                    tolower(strtrim(shape$v1label,2))=="so",]

v1_gender <- shape_untimed[tolower(strtrim(shape$v1label,3))=="gen" | 
                     tolower(strtrim(shape$v1label,3))=="sex" | 
                     tolower(strtrim(shape$v1label,4))=="male" | 
                     tolower(strtrim(shape$v1label,1))=="f",]

v1_hand <- shape_untimed[tolower(strtrim(shape$v1label,3))=="dom" | 
                   tolower(strtrim(shape$v1label,4))=="hand" | 
                   tolower(strtrim(shape$v1label,3))=="non" | 
                   tolower(strtrim(shape$v1label,8))=="step dom",]

# Turn the numerical independent var of v1_* to factors
v1_music$numShapes = as.factor(v1_music$numShapes)
v1_music$timerDisplay = as.factor(v1_music$timerDisplay)
v1_music$matchingScheme = as.factor(v1_music$matchingScheme)

v1_hand$numShapes = as.factor(v1_hand$numShapes)
v1_hand$timerDisplay = as.factor(v1_hand$timerDisplay)

v1_gender$numShapes = as.factor(v1_gender$numShapes)
v1_gender$timerDisplay = as.factor(v1_gender$timerDisplay)


# Display two comparable graphs in the same row in ggplot2
library(gridExtra)
plot1 <- ggplot(data = shape_untimed, aes(x=matchingScheme, y=numErrors)) +
                    geom_boxplot()  + theme(legend.position="top") + labs(title="")
plot2 <- ggplot(data = shape_untimed, aes(x=matchingScheme, y=numErrors)) + 
                    geom_boxplot()  + aes(colour=numShapes) + facet_wrap(~timerDisplay, ncol=4) +
                    theme(legend.position="top") + labs(title="") 
grid.arrange(plot1, plot2, ncol=2)


plot3 <- ggplot(data = shape_HotchkissStudent16, aes(x=matchingScheme, y=numErrors)) +
  geom_boxplot()  + theme(legend.position="top") + labs(title="")
plot4 <- ggplot(data = shape_HotchkissStudent16, aes(x=matchingScheme, y=numErrors)) + 
  geom_boxplot()  + aes(colour=numShapes) + facet_wrap(~timerDisplay, ncol=4) +
  theme(legend.position="top") + labs(title="") 
grid.arrange(plot3, plot4, ncol=2)


plot5 <- ggplot(data = shape_6002T2, aes(x=matchingScheme, y=numErrors)) +
  geom_boxplot()  + theme(legend.position="top") + labs(title="")
plot6 <- ggplot(data = shape_6002T2, aes(x=matchingScheme, y=numErrors)) + 
  geom_boxplot()  + aes(colour=timerDisplay) + facet_wrap(~numShapes, ncol=4) +
  theme(legend.position="top") + labs(title="") 
grid.arrange(plot5, plot6, ncol=2)


plot7 <- ggplot(data = shape_6002T3, aes(x=matchingScheme, y=numErrors)) +
  geom_boxplot()  + theme(legend.position="top") + labs(title="")
plot8 <- ggplot(data = shape_6002T3, aes(x=matchingScheme, y=numErrors)) + 
  geom_boxplot()  + aes(colour=timerDisplay) + facet_wrap(~numShapes, ncol=4) +
  theme(legend.position="top") + labs(title="") 
grid.arrange(plot7, plot8, ncol=2)


plot9 <- ggplot(data = shape_6002T4, aes(x=matchingScheme, y=numErrors)) +
  geom_boxplot()  + theme(legend.position="top") + labs(title="")
plot10 <- ggplot(data = shape_6002T4, aes(x=matchingScheme, y=numErrors)) + 
  geom_boxplot()  + aes(colour=timerDisplay) + facet_wrap(~numShapes, ncol=4) +
  theme(legend.position="top") + labs(title="") 
grid.arrange(plot9, plot10, ncol=2)


plot11 <- ggplot(data = shape_6002T6, aes(x=matchingScheme, y=numErrors)) +
  geom_boxplot()  + theme(legend.position="top") + labs(title="")
plot12 <- ggplot(data = shape_6002T6, aes(x=matchingScheme, y=numErrors)) + 
  geom_boxplot()  + aes(colour=timerDisplay) + facet_wrap(~numShapes, ncol=4) +
  theme(legend.position="top") + labs(title="") 
grid.arrange(plot11, plot12, ncol=2)


