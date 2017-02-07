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

write.csv(tangram, "tangram_updated.csv")
