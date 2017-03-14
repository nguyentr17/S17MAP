gen_table <- as.data.frame(table(gen_tangram$GroupName))

test <- gen_tangram[gen_tangram$GroupName == "SECTION6",]

gen_table <- gen_tangram%>% 
  group_by(GroupName) %>%
  summarise(number = n())

gen_table1 <- gen_table1[gen_table1$number>100,]

test1 <- gen_tangram[gen_tangram$GroupName %in% gen_table1$GroupName,]

library(gplots)

groupname <- factor(test1$GroupName)

gender <- factor(test1$level_gender)

interaction.plot(gender, groupname, test1$TimeUsed_log, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),	
                 xlab="Gender", 
                 ylab="TimeUsed_log", 
                 main="Interaction Plot",legend=FALSE)

