# this script runs the simulations, collects the output and plots it

# parameters need to be set in the "setupandrunsimulations.r" file!

source("setupandrunsimulations.r")

# reading in the output from the different scenarios
# elephants

# 
# --------------------- dominant inheritance -------------------------------------

# first want to collate all outputs into a dta frame called "allout"
load("dominant_scenario1.RData")
allout <- output

load("dominant_scenario2.RData")
allout <- rbind(allout, output)

load("dominant_scenario3.RData")
allout <- rbind(allout, output)

load("dominant_scenario4.RData")
allout <- rbind(allout, output)

load("dominant_scenario5.RData")
allout <- rbind(allout, output)

load("dominant_scenario6.RData")
allout <- rbind(allout, output)

rm(output)
allout$scenario <- as.factor(allout$scenario)
summary(allout)

# now plot
# see following link for a great tutotial on violin plots using ggplot2
# http://www.sthda.com/english/wiki/ggplot2-violin-plot-quick-start-guide-r-software-and-data-visualization
library(ggplot2)
source("multiplotfunction.r")  # loads function that plots multiple graphs onto the same page

p1 <- ggplot(allout, aes(x=scenario, y=totpopsize)) + 
  geom_violin(trim=T, scale="width") +
  labs(x="Management scenario", y = "Total number of elephants") +
  stat_summary(fun.data=mean_sdl, fun.args=list(mult=1), 
                 geom="pointrange", color="red")

p2 <- ggplot(allout, aes(x=scenario, y=bigtuskers)) + 
  geom_violin(trim=T, scale="width") +
  labs(x="Management scenario", y = "Number of largetuskers left") +
  stat_summary(fun.data=mean_sdl, fun.args=list(mult=1), 
                 geom="pointrange", color="red")

p3 <- ggplot(allout, aes(x=scenario, y=oldbigtuskers)) + 
  geom_violin(trim=T, scale="width") +
  labs(x="Management scenario", y = "Number of +55 largetuskers left") +
  stat_summary(fun.data=mean_sdl, fun.args=list(mult=1), 
                 geom="pointrange", color="red")

p4 <- ggplot(allout, aes(x=scenario, y=allkilledBT)) + 
  geom_violin(trim=T, scale="width") +
  labs(x="Management scenario", y = "Number of largetuskers killed") +
  stat_summary(fun.data=mean_sdl, fun.args=list(mult=1), 
                 geom="pointrange", color="red")

p5 <- ggplot(allout, aes(x=scenario, y=allkilledST)) + 
  geom_violin(trim=T, scale="width") +
  labs(x="Management scenario", y = "Number of smalltuskers killed") +
  stat_summary(fun.data=mean_sdl, fun.args=list(mult=1), 
                 geom="pointrange", color="red")

p6 <- ggplot(allout, aes(x=scenario, y=income)) + 
  geom_violin(trim=T, scale="width") +
  labs(x="Management scenario", y = "Income generated from permits") +
  stat_summary(fun.data=mean_sdl, fun.args=list(mult=1), 
                 geom="pointrange", color="red")

pdf("output_dominant3.pdf")
multiplot(p1, p2, p3, p4, p5, p6, cols=2)
dev.off()


# 
# --------------------- recessive inheritance ------------------------------------

# first want to collate all outputs into a dta frame called "allout"
load("recessive_scenario1.RData")
allout <- output

load("recessive_scenario2.RData")
allout <- rbind(allout, output)

load("recessive_scenario3.RData")
allout <- rbind(allout, output)

load("recessive_scenario4.RData")
allout <- rbind(allout, output)

load("recessive_scenario5.RData")
allout <- rbind(allout, output)

load("recessive_scenario6.RData")
allout <- rbind(allout, output)

rm(output)
allout$scenario <- as.factor(allout$scenario)
summary(allout)

# now plot
# see following link for a great tutotial on violin plots using ggplot2
# http://www.sthda.com/english/wiki/ggplot2-violin-plot-quick-start-guide-r-software-and-data-visualization
library(ggplot2)
source("multiplotfunction.r")  # loads function that plots multiple graphs onto the same page

p1 <- ggplot(allout, aes(x=scenario, y=totpopsize)) + 
  geom_violin(trim=T, scale="width") +
  labs(x="Management scenario", y = "Total number of elephants") +
  stat_summary(fun.data=mean_sdl, fun.args=list(mult=1), 
               geom="pointrange", color="red")

p2 <- ggplot(allout, aes(x=scenario, y=bigtuskers)) + 
  geom_violin(trim=T, scale="width") +
  labs(x="Management scenario", y = "Number of largetuskers left") +
  stat_summary(fun.data=mean_sdl, fun.args=list(mult=1), 
               geom="pointrange", color="red")

p3 <- ggplot(allout, aes(x=scenario, y=oldbigtuskers)) + 
  geom_violin(trim=T, scale="width") +
  labs(x="Management scenario", y = "Number of +55 largetuskers left") +
  stat_summary(fun.data=mean_sdl, fun.args=list(mult=1), 
               geom="pointrange", color="red")

p4 <- ggplot(allout, aes(x=scenario, y=allkilledBT)) + 
  geom_violin(trim=T, scale="width") +
  labs(x="Management scenario", y = "Number of largetuskers killed") +
  stat_summary(fun.data=mean_sdl, fun.args=list(mult=1), 
               geom="pointrange", color="red")

p5 <- ggplot(allout, aes(x=scenario, y=allkilledST)) + 
  geom_violin(trim=T, scale="width") +
  labs(x="Management scenario", y = "Number of smalltuskers killed") +
  stat_summary(fun.data=mean_sdl, fun.args=list(mult=1), 
               geom="pointrange", color="red")

p6 <- ggplot(allout, aes(x=scenario, y=income)) + 
  geom_violin(trim=T, scale="width") +
  labs(x="Management scenario", y = "Income generated from permits") +
  stat_summary(fun.data=mean_sdl, fun.args=list(mult=1), 
               geom="pointrange", color="red")

pdf("output_recessive3.pdf")
multiplot(p1, p2, p3, p4, p5, p6, cols=2)
dev.off()


