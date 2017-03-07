### Graphics 
### Dataset: tangram
### Goal: to show the difference in conclusion given by different metrics used
### --> how to choose appropriate response and explanatory variables


### Time used is small but does not win
tangram$TimeUsed_log <- log(tangram$TimeUsed)
gen_tangram <- tangram[tangram$factor_gender > 0 & tangram$level_gender != -2,]
library(mosaic)
install.packages("manipulate")
library(manipulate)
mplot(gen_tangram)

gen_tangram_norestriction <- gen_tangram[gen_tangram$RequestedTime == 0,]
ggplot(gen_tangram_norestriction, aes(x = level_gender, y = TimeUsed_log))
mplot(gen_tangram_norestriction)


### Requested Time
devtools::install_github("ropensci/plotly")
library(plotly)
library(shiny)
# all gender dataset
plot_ly() %>%
  add_histogram(gen_tangram[gen_tangram$level_gender == "F",]$TimeUsed_log) %>%
  add_histogram(gen_tangram[gen_tangram$level_gender == "M",]$TimeUsed_log) %>%
  layout(barmode = "overlay")
# restriction
plot_ly() %>%
  add_histogram(gen_tangram_norestriction[gen_tangram_norestriction$level_gender == "F",]$TimeUsed_log) %>%
  add_histogram(gen_tangram_norestriction[gen_tangram_norestriction$level_gender == "M",]$TimeUsed_log) %>%
  layout(barmode = "overlay")


# Choose a group study: SECTION6
gen_tangram_individual <- gen_tangram[gen_tangram$GroupName == "SECTION6",]
plot_ly() %>%
  add_histogram(gen_tangram_individual[gen_tangram_individual$level_gender == "F",]$TimeUsed_log) %>%
  add_histogram(gen_tangram_individual[gen_tangram_individual$level_gender == "M",]$TimeUsed_log) %>%
  layout(barmode = "overlay")
t.test(gen_tangram_individual[gen_tangram_individual$level_gender == "F",]$TimeUsed_log, 
       gen_tangram_individual[gen_tangram_individual$level_gender == "M",]$TimeUsed_log)

### ALL GENDER GROUPS: Sampling Distribution of Means and p-value (against sample size)
library(data.table)
library(plyr)
library(dplyr)
tangram_tested <- as.data.table(tangram)
tangram_tested$level_gender[tangram_tested$level_gender == -2] <- "Others"
tangram_tested$GroupName <- tolower(tangram_tested$GroupName) ### ASSUMPTION: case-insenstive for group name
# filtering out null names and groups with small sample size (< 5)
tangram_tested <- tangram_tested[tangram_tested$GroupName != "",]


# Only include 2 levels of gender: Female and Male
tangram_pvalue <- tangram_tested[tangram_tested$level_gender %in% c("F", "M"),]
tangram_pvalue[, `:=`( SampleSize = .N) , by = GroupName] 
tangram_pvalue <- tangram_pvalue[tangram_pvalue$SampleSize >= 5,]
tangram_pvalue[, `:=`(log_timeused_mean = mean(TimeUsed_log), timeused_mean = mean(TimeUsed)), by = c("GroupName", "level_gender")]
tangram_pvalue[, `:=`(n = .N, sd = sd(TimeUsed_log)), by = c("GroupName", "level_gender")]

ttestFun <- function(dat) {
  if (sum(dat$level_gender == "F") > 1 && sum(dat$level_gender == "M") > 1) {
    the_fit <- t.test(TimeUsed_log ~ level_gender, data = dat)
    #setNames(the_fit$p.value, "p.value")}
    c("p.value" = the_fit$p.value, "samplesize" = mean(dat$SampleSize),
      "F_mean" = mean(dat[dat$level_gender == "F",]$TimeUsed_log),
      "M_mean" = mean(dat[dat$level_gender == "M",]$TimeUsed_log),
      "F_SD" = sd(dat[dat$level_gender == "F",]$TimeUsed_log),
      "M_SD" = sd(dat[dat$level_gender == "M",]$TimeUsed_log))}
  else {
    c("p.value" = -1, "samplesize" = mean(dat$SampleSize),
      "F_mean" = -1, "M_mean" = -1, "F_SD" = NA, "F_SD" = NA)
  }
}
# 1. Scatterplot of pvalue vs. sample size

alpha <- 0.1
gender_pval_dist <- ddply(tangram_pvalue, ~ GroupName, ttestFun)
gender_pval_dist$significant <-(gender_pval_dist$p.value < alpha)*1
gender_pval_dist$significant <- as.factor(gender_pval_dist$significant)
ggplot(gender_pval_dist[gender_pval_dist$p.value != -1,], aes(x = samplesize, y = p.value)) + 
  geom_point(aes(colour = significant)) +
  scale_color_manual(values=c("black", "red")) +
  geom_hline(aes(yintercept = alpha), color = "red") +
  geom_text(aes(100,0.1,label = "Significant level = 0.1", vjust = -1), color = "red") +
  scale_x_continuous(name="Sample Size", limits=c(0, 125)) +
  scale_y_continuous(name="t-test p-value", limits=c(0, 1), breaks = seq(0,1, by = 0.1)) 

# 2. Histogram of p-value 
ggplot(gender_pval_dist[gender_pval_dist$p.value != -1,], aes(p.value)) +
  geom_histogram(binwidth = 0.01, position = "identity", alpha = 0.5, colour = "black", fill = "white")
  

# 3. Overlaid histogram for means (Female vs Male)
gender_samplingdis <- tangram_tested[tangram_tested$level_gender != -1,]
gender_samplingdis[,`:=`(SampleSize = .N), by = GroupName]
gender_samplingdis[, `:=`(log_timeused_mean = mean(TimeUsed_log), timeused_mean = mean(TimeUsed)), by = c("GroupName", "level_gender")]
gender_samplingdis[, `:=`(n = .N, sd = sd(TimeUsed_log)), by = c("GroupName", "level_gender")]
gender_samplingdis <- gender_samplingdis[,c("GroupName", "level_gender", "SampleSize", "timeused_mean", "log_timeused_mean","n","sd"), with = FALSE]
gender_samplingdis <- gender_samplingdis[!duplicated(gender_samplingdis),]

### Aggregate data for vertical lines
gender_samplingdis_vline <- gender_samplingdis %>%
  group_by(level_gender) %>%
  summarise(log_timeused_mean = mean(log_timeused_mean), timeused_mean = mean(timeused_mean))

ggplot(gender_samplingdis, aes(timeused_mean, fill = level_gender)) + 
  geom_density(alpha = 0.3) +
  geom_vline(data=gender_samplingdis_vline, aes(xintercept=timeused_mean,  colour=level_gender),
               linetype="dashed", size=1) +
  labs(title = "Distribution of Sample Means of Different Gender Levels") +
  labs(x = "Sample Mean", y = "Density") +
  ylim(0.00, 0.025) +
  scale_x_continuous(minor_breaks = seq(0, 200, by = 25))

ggplot(gender_samplingdis, aes(log_timeused_mean, fill = level_gender)) + 
  geom_density(alpha = 0.3) +
  geom_vline(data=gender_samplingdis_vline, aes(xintercept=log_timeused_mean,  colour=level_gender),
               linetype="dashed", size=1) +
  labs(title = "Distribution of Sample Means of Different Gender Levels") +
  labs(x = "Sample Mean (log)", y = "Density") +
  ylim(0.00, 1.00)
  
### Calculate effect size for samples that show statistically significant samples
gender_samp_significant <- gender_pval_dist[gender_pval_dist$p.value < 0.1 & gender_pval_dist$p.value >= 0, ]
effectsize <- function(dat) {
  M1 <- dat$F_mean
  M2 <- dat$M_mean
  s1 <- dat$F_SD
  s2 <- dat$M_SD
  s.pooled <- sqrt((s1*s1+s2*s2)/2)
  c("effectsize" = abs(M1 - M2)/s.pooled)
}
ddply(gender_samp_significant, ~GroupName, effectsize)




                                  
### ----------------------------------------------------- ###
### coffee truck dataset
library(mosaic)
library(mplot)
library(gridExtra)
library(grid)
mplot(coffee_truck_dataset)
coffee_truck_OSUBIO <- coffee_truck_dataset[coffee_truck_dataset$`Group Name` == "OSUBIO",]
ggplot(data = coffee_truck_OSUBIO, aes(x=Temperature, y = Profit)) + geom_point()
ggplot(data = subset(coffee_truck_OSUBIO, !is.na(coffee_truck_OSUBIO$Location)), aes(x=Temperature, y = Profit)) + geom_point(aes(colour=Location))

### Box plot to show how location impacts profit in one study and in all studies.
### Conclusion: they are different even though it is just a simulation --> neeed to take other factors into account
### Question: how costs are calculated?
limits = c(-300,300)
breaks = seq(limits[1], limits[2], by = 100)
p1 <- ggplot(data = subset(coffee_truck_OSUBIO, !is.na(coffee_truck_OSUBIO$Location)), aes(x=Location, y = Profit)) +
  geom_boxplot() +
  ggtitle("OSUBIO Dataset") +
  scale_y_continuous(limits = limits, breaks = breaks)

p2 <- ggplot(data = subset(coffee_truck_dataset, !is.na(coffee_truck_dataset$Location)), aes(x=Location, y = Profit)) +
  geom_boxplot() +
  ggtitle("Big Dataset") +
  scale_y_continuous(limits = limits, breaks = breaks)

pushViewport(viewport(layout = grid.layout(1, 2)))
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

### Sales instead of Profits
ggplot(data = subset(coffee_truck_OSUBIO, !is.na(coffee_truck_OSUBIO$Location)), aes(x=Location, y = Sales)) +
  geom_boxplot() +
  ggtitle("OSUBIO Dataset") +
  scale_y_continuous(limits = limits, breaks = breaks)

### Adding one more factor
### Music
library(ggplot2)
ggplot(data = subset(coffee_truck_dataset, !is.na(coffee_truck_dataset$Location)), aes(x=Location, y = Profit)) +
  geom_boxplot() +
  ggtitle("Big Dataset") +
  scale_y_continuous(limits = limits, breaks = breaks) +
  facet_grid(Music ~ .)

### Price







