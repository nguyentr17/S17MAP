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
