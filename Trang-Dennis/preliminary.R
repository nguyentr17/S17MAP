### MAP 2017: 
### Data
tangram <- read.csv("tangrams_data-2.csv", stringsAsFactors = F)
View(tangram)


### Stream data online
library(RCurl)
url <- "http://statgames.tietronix.com/tangrams/webreporter.php?game=tangrams&groupID=&winlose=both&random=false&rows=&type=csv"
tangram_streamed <- read.csv(textConnection(readLines(url)), stringsAsFactors = F)




### Making a treemap
### Goal: distribution of different games
install.packages("treemap")
library(treemap)
library(dplyr)
tangram2 <- count(tangram, "PuzzleName")
itreemap(tangram2, index = "PuzzleName", 
        vSize = "freq",
        vColor = "GameType",
        type = "categorical")

## colored by types of games: fixed or submitted

statsgame <- c("Laughing Man", "Piano", "The Hook", "Complex Hexagon", "Diamond", "House of Tangrams",
               "A Nice Lighthouse", "The Brain Buster", "A Simple Chair", "The Hat Wearer",
               "The Acrobat", "The Bird", "Crouching Cat", "The Goat", "The Six", "The G",
               "Andy's Puzzle", "Walking Person Puzzle", "A Medicine Jar", "Candle")
tangram2$GameType <- "submitted"
tangram2$GameType[tangram2$PuzzleName %in% statsgame] <- "statsgame"

### Treemap for gen_tangram
gen_tangram <- gen_tangram[is.na(gen_tangram$gender) != 1,]
gen_tangram_statsgame <- gen_tangram[gen_tangram$PuzzleName %in% statsgame,]
gen_tangram_count <- count(gen_tangram_statsgame,"PuzzleName")
gen_tangram_count <- gen_tangram_count[order(-gen_tangram_count$freq),]
itreemap(gen_tangram_count, index = "PuzzleName", 
         vSize = "freq")
library(ggplot2)
ggplot(gen_tangram_statsgame[gen_tangram_statsgame$PuzzleName %in% head(gen_tangram_count$PuzzleName,5),], aes(fill = gender, y = TimeUsed, x = PuzzleName)) +
  geom_bar(position = "dodge", stat = "identity")

### A list of solid factors
factorlist <- as.data.frame(table(c(tangram$Factor1,tangram$Factor2, tangram$Factor3)))
View(factorlist)


### STEM
tangram$Factor_STEM <- c()
tangram$Level_STEM <- c()
for (i in c(4,6,8)) {
  if (tangram[[]])
}

grep(paste(gen, collapse = "|"), "Gender", ignore.case = T)


### Gender

