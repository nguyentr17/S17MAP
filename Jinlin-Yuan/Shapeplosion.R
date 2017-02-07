library(plyr)
library(dplyr)
library(ggplot2)
library(mosaic)
### Getting data without time limit (Sample A)
library(data.table)
shape <- fread("http://kuiper.pearsoncmg.com/shapesplosion/webreporter.php?game=PerfectionFlash&groupID=&winlose=both&random=false&rows=&type=csv")
shape_untime = shape[shape$requestedTime==0,]
shape_untimed = shape_untime[shape_untime$timeUsed!=0,]

shape_untimed$numShapes = as.factor(shape_untimed$numShapes)
shape_untimed$matchingScheme = as.factor(shape_untimed$matchingScheme)
shape_untimed$requestedTime = as.factor(shape_untimed$requestedTime)
shape_untimed$timeUsed = as.numeric(shape_untimed$timeUsed)
shape_untimed$timerDisplay = as.factor(shape_untimed$timerDisplay)
shape_untimed$numErrors = as.numeric(shape_untimed$numErrors)

shape_untimed <-mutate(shape_untimed, TimeUsedSec = shape_untimed$timeUsed/1000)

### Further cleaning of v1label = gender
gender <- filter(shape_untimed, tolower(strtrim(shape_untimed$v1label,3))=="gen" |
                   tolower(strtrim(shape_untimed$v1label,3))=="sex" |
                   tolower(strtrim(shape_untimed$v1label,4))=="male" |
                   tolower(strtrim(shape_untimed$v1label,1))=="f")
#delete specific rows
case <- (tolower(strtrim(gender$v1value,1)) == "m") & 
  (tolower(strtrim(gender$v2value,1)) =="f")
gender1 <- gender[(strtrim(gender$v1value,1) != "1") &
                    (gender$v1value != "2") &
                    (gender$v1label != "Gender\rOrder") &
                    (gender$v1label != "female\rorder") &
                    (gender$v1value != "0") &
                    (tolower(gender$v1value) != "morf") &
                    (gender$studentID != "mb") &
                    (gender$studentID != "31207") &
                    (gender$v1value != "cat") &
                    (gender$studentID != "3659") &
                    (tolower(gender$v1value) != "attempt") &
                    (!case),]


cond <- tolower(strtrim(shape_untimed$v1value,1))=="m" &
  tolower(strtrim(shape_untimed$v2value,1))=="f"

yuan_gender <- filter(shape_untimed, (tolower(strtrim(shape_untimed$v1label,3))=="gen" |
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

#identify all entries with v1value starts with "M"/"F"
#create a new column indicating 1/0 Male/Female

gender1 <- mutate(gender1, 
                  gender = as.factor(ifelse(
                    pmax((tolower(strtrim(gender1$v1label,1)) == "m"), (tolower(strtrim(gender1$v1value,1)) =="m")), 
                    1, 
                    ifelse(pmax(tolower(strtrim(gender1$v1label,1)) == "f",
                                (tolower(strtrim(gender1$v1value,1)) =="f")), 
                           0, 
                           NA))))

gender2 <- na.omit(gender1)

#identifying most played groups:
tb <- as.data.frame(table(gender2$groupID))
tb <- tb[order(-tb$Freq),]
tb <- tb[tb$Freq > 25,]

#Select groups that are under size 50
tb2 <- filter(tb, tb$Freq >= 5 &tb$Freq <= 50)

#Create a vector of groupID's whose size is between 25 and 50
selected_groupID <- as.character(tb2$Var1)


#Hypothesis testing function
par(mar=c(1,1,1,1))
par(mfrow = c(3,6))
groupName <- c()
pvalues <- c()
for (i in 1:length(selected_groupID)) {
  female <- gender2[gender2$groupID == selected_groupID[i] & gender2$gender == 0,]$TimeUsedSec
  male <- gender2[gender2$groupID == selected_groupID[i] & gender2$gender == 1,]$TimeUsedSec
  if (length(female) > 1 & length(male) > 1) {
    groupName <- cbind(groupName, selected_groupID[i])
    p <- round(t.test(female, male)$p.value, digits = 3)
    pvalues <- cbind(pvalues, p)
    
    data1 <- gender2[gender2$groupID == selected_groupID[i],]
    boxplot(TimeUsedSec ~ gender,data=data1, main=paste("n =", dim(data1)[1], ", p-value = ", p),
            xlab="Gender (1=Male, 0=Female)", ylab="Time Used Seconds")
  }
}


#Result of tb$Var1

#[1] HJ375F14  MSP2013   MAT336    HJ190F14  stats2    MAED550   MAT336S14 336S14    mth22601  MATH22015 MAT336S15
#[12] MATH22018 USCOTS15  mth32602  mth22602  mth32601 


#comparing two most played groups (104 vs 79)
HJ375F14 =gender1[gender1$groupID=="HJ375F14",]
MSP2013=gender1[gender1$groupID=="MSP2013",]

# Added sample size (# of observations to the graph)
ggplot(data = HJ375F14, aes(x=matchingScheme, y=timeUsed)) + geom_boxplot()  + 
  aes(colour=gender) + theme(legend.position="none") + labs(title= paste("n =", dim(HJ375F14)[1])) 
ggplot(data = MSP2013, aes(x=matchingScheme, y=timeUsed)) + geom_boxplot()  +
  aes(colour=gender) + theme(legend.position="none") + labs(title= paste("n =", dim(MSP2013)[1])) 

model_gp_1_1 = lm(timeUsed~gender, data = HJ375F14)
model_gp_1_2 = lm(timeUsed~gender + matchingScheme, data = HJ375F14)
model_gp_2_1 = lm(timeUsed~gender, data = MSP2013)
model_gp_2_2 = lm(timeUsed~gender + matchingScheme, data = MSP2013)

### Potential filters
selected = shape_untimed[,c("numShapes","numErrors")]
mplot(HJ375F14)

#### raw info about data
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
v1_gender = shape_untimed[tolower(strtrim(shape_untimed$v1label,3))=="gen" |
                            tolower(strtrim(shape_untimed$v1label,3))=="sex" |
                            tolower(strtrim(shape_untimed$v1label,4))=="male" |
                            tolower(strtrim(shape_untimed$v1label,1))=="f",]
gender <- filter(shape_untimed, tolower(strtrim(shape_untimed$v1label,3))=="gen" |
                   tolower(strtrim(shape_untimed$v1label,3))=="sex" |
                   tolower(strtrim(shape_untimed$v1label,4))=="male" |
                   tolower(strtrim(shape_untimed$v1label,1))=="f")
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

ggplot(data = shape_untimed, aes(x=numShapes, y=numErrors)) + geom_boxplot()  + aes(colour=matchingScheme) +  facet_wrap(~timerDisplay, ncol=4) + theme(legend.position="top") + labs(title="") 

mean(shape_untimed_sec$TimeUsedSec)
mean(music_untimed$TimeUsedSec)
## Question
1.get rid of the entries? 
MUSIC on & off 
vlabel: male  vvalue: blank
| vs ||