
### Initial analysis

shape <- read.csv("C:/Users/hejinlin/Desktop/Original.csv",
                  stringsAsFactors = F)

shape_untime = shape[shape$requestedTime==0,]
shape_untimed = shape_untime[shape_untime$timeUsed!=0,]
name = names(shape_untimed)
selected = shape_untimed[,c("numShapes","numErrors")]
a = selected[,c("numShapes")]

tapply(shape_untimed$logTimeUsed, shape_untimed$shapesMatched)
#### 
table(shape$numShapes) #fine
15    18    21    24 
5368  1992  6792 24196 
table(shape$matchingScheme) #fine
both   diffColor optionColor       shape 
7069        8336        2014       20929 
##"shape" means that all of the colors are the same, 

#"both" means that every piece has a
#different color than every other piece 
# (but the same color as the piece's corresponding 
#hole) 

#"diffColor" means that all the pieces have different 
#colors (and may or may not have the same color as 
#their corresponding hole) and 

#"optionColor" means that all the pieces are the 
#same shape, but different color.
table(shape$requestedTime) #fine
0    25    45    65 
28977  4539  2687  2145 
table(shape$proximityValue) #WIERD
1     3    15    28    54   107 
59  1711  1315   455 32823  1985 
table(shape$timerDisplay) #fine
0     1 
2719 35629 
summary(shape$numErrors)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.000   0.000   1.000   1.431   2.000  67.000 
table(shape$shapesMatched)
summary(shape$timeUsed)
table(shape$timeUsed)#Need to eliminate the 
shape_untimed = shape_untime[shape_untime$timeUsed!=0,]

freq2 = count_(shape,"v2label")
####Single-factor Plot
# Response 1: errors
boxplot(shape_untimed$numErrors~shape_untimed$proximityValue)
boxplot(shape_untimed$numErrors~shape_untimed$matchingScheme)
boxplot(shape_untimed$numErrors~shape_untimed$numShapes)
# Response 2: timeused
boxplot(log(shape_untimed$timeUsed)~shape_untimed$proximityValue)
boxplot(log(shape_untimed$timeUsed)~shape_untimed$matchingScheme)
boxplot(log(shape_untimed$timeUsed)~shape_untimed$numShapes)

## unlimited time response shapesmatched don't work
## boxplot(shape_untimed$shapesMatched~shape_untimed$proximityValue)

### Todo: 
## -normalize/compute variables
____________________________________________________________________
## Extracting additional variables
"Music" "music" "song" "Song" "MUSICON MUSIC"
v1_music <- shape[tolower(strtrim(shape$v1label,2))=="mu" |
                    tolower(strtrim(shape$v1label,2))=="so",]

"gender" "gender order" "sex" "male" "female" "male athlete" 
"f gender" "gender female" 
v1_gender <- shape[tolower(strtrim(shape$v1label,3))=="gen" |
                     tolower(strtrim(shape$v1label,3))=="sex" |
                     tolower(strtrim(shape$v1label,4))=="male" |
                     tolower(strtrim(shape$v1label,1))=="f",]
"dominant" "DOM" "DOMHAND" "DomHand" "hand" "DOMINANT" "Hand" "Dominant"
"HAND" "Step dominant" "non dominant" "non-dominant hand"
"NONDOMINANT" "Nondominant" "domhand" "dominant hand" "dominhand" 
"Nondominant hand" "Dominant hand" "DOMstep" "DOM Step" 
v1_dominant <-shape[tolower(strtrim(shape$v1label,3))=="dom" | 
                      tolower(strtrim(shape$v1label,4))=="hand" |
                      tolower(strtrim(shape$v1label,3))=="non" |
                      tolower(strtrim(shape$v1label,6))=="step d",]

unique(v1_music$v1label)

_________________________________________________________________________________
## Draw graphs (untimed data, two ind variables, mplot, compared with smaller samples
# with one additional variable "music")
library(ggplot2)
library(mosaic)
mplot(shape_untimed_truncate)
shape_untimed_sec <-mutate(shape_untimed, TimeUsedSec = shape_untimed$timeUsed/1000)
shape_untimed_sec$numShapes = as.factor(shape_untimed_sec$numShapes)
shape_untimed_sec$timerDisplay = as.factor(shape_untimed_sec$timerDisplay)
shape_untimed_sec$matchingScheme = as.factor(shape_untimed_sec$matchingScheme)
shape_untimed_truncate = shape_untimed_sec[shape_untimed_sec$TimeUsedSec<=1000,]

music_untimed <- shape_untimed_sec[tolower(strtrim(shape_untimed_sec$v1label,2))=="mu" |
                                     tolower(strtrim(shape_untimed_sec$v1label,2))=="so",]

#1 Log(Timeused:truncated) ~ numShapes, colored by MatchingScheme
plot1 = ggplot(data = shape_untimed_truncate, aes(x=numShapes, y=TimeUsedSec)) + geom_boxplot()  + aes(colour=matchingScheme) + scale_y_log10() + theme(legend.position="top") + labs(title="") 
plot2 = ggplot(data = music_untimed, aes(x=numShapes, y=TimeUsedSec)) + geom_boxplot()  + aes(colour=matchingScheme) + scale_y_log10() + theme(legend.position="top") + labs(title="") 

#2 Log(Timeused) ~ numShapes, colored by Timer
plot3=ggplot(data = shape_untimed_truncate, aes(x=numShapes, y=TimeUsedSec)) + geom_boxplot()  + aes(colour=timerDisplay) + scale_y_log10() + theme(legend.position="top") + labs(title="") 
plot4=ggplot(data = music_untimed, aes(x=numShapes, y=TimeUsedSec)) + geom_boxplot()  + aes(colour=timerDisplay) + scale_y_log10() + theme(legend.position="top") + labs(title="") 


#3 Log(timeused) ~ matching scheme, colored by Timer
plot5=ggplot(data = shape_untimed_truncate, aes(x=matchingScheme, y=TimeUsedSec)) + geom_boxplot()  + aes(colour=timerDisplay) + scale_y_log10() + theme(legend.position="top") + labs(title="") 
plot6=ggplot(data = music_untimed, aes(x=matchingScheme, y=TimeUsedSec)) + geom_boxplot()  + aes(colour=timerDisplay) + scale_y_log10() + theme(legend.position="top") + labs(title="") 

#4 Log(timeused) ~ matching scheme, colored by numShapes
plot7=ggplot(data = shape_untimed_truncate, aes(x=matchingScheme, y=TimeUsedSec)) + geom_boxplot()  + aes(colour=numShapes) + scale_y_log10() + theme(legend.position="top") + labs(title="") 
plot8=ggplot(data = music_untimed, aes(x=matchingScheme, y=TimeUsedSec)) + geom_boxplot()  + aes(colour=numShapes) + scale_y_log10() + theme(legend.position="top") + labs(title="") 

#5 Log(timeused) ~ timerDisplay, colored by matchingScheme
plot9=ggplot(data = shape_untimed_truncate, aes(x=timerDisplay, y=TimeUsedSec)) + geom_boxplot()  + aes(colour=matchingScheme) + scale_y_log10() + theme(legend.position="top") + labs(title="") 
plot10=ggplot(data = music_untimed, aes(x=timerDisplay, y=TimeUsedSec)) + geom_boxplot()  + aes(colour=matchingScheme) + scale_y_log10() + theme(legend.position="top") + labs(title="") 

#6 Log(timeused) ~ timerDisplay, colored by numShapes
plot11= ggplot(data = shape_untimed_truncate, aes(x=timerDisplay, y=TimeUsedSec)) + geom_boxplot()  + aes(colour=numShapes) + scale_y_log10() + theme(legend.position="top") + labs(title="") 
plot12= ggplot(data = music_untimed, aes(x=timerDisplay, y=TimeUsedSec)) + geom_boxplot()  + aes(colour=numShapes) + scale_y_log10() + theme(legend.position="top") + labs(title="") 

#7 Log(timeused) ~ numShapes, colored by matchingScheme, facet by timerDisplay
plot13=ggplot(data = shape_untimed_truncate, aes(x=numShapes, y=timeUsed)) + geom_boxplot()  + aes(colour=matchingScheme) + scale_y_log10() + facet_wrap(~timerDisplay, ncol=4) + theme(legend.position="top") + labs(title="") 
plot14=ggplot(data = music_untimed, aes(x=numShapes, y=timeUsed)) + geom_boxplot()  + aes(colour=matchingScheme) + scale_y_log10() + facet_wrap(~timerDisplay, ncol=4) + theme(legend.position="top") + labs(title="") 
grid.arrange(plot13,plot14,ncol=2)
## Question
1.get rid of the entries? 
MUSIC on & off 
vlabel: male  vvalue: blank
| vs ||