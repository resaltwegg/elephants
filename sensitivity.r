# this script runs the simulations, collects the output and plots it

# parameters need to be set in the "setupandrunsimulations.r" file!

source("setupandrunsimulations.r")

# reading in the output from the different scenarios
# elephants

# 
# --------------------- dominant inheritance -------------------------------------

# first want to collate all outputs into a dta frame called "allout"
load("dominant_scenario1.RData")
sc1 <- output

load("dominant_scenario2.RData")
sc2 <- output

load("dominant_scenario3.RData")
sc3 <- output

load("dominant_scenario4.RData")
sc4 <- output

load("dominant_scenario5.RData")
sc5 <- output

load("dominant_scenario6.RData")
sc6 <- output

diff21 <- apply(sc2,2,mean) - apply(sc1,2,mean)
diff31 <- apply(sc3,2,mean) - apply(sc1,2,mean)
diff41 <- apply(sc4,2,mean) - apply(sc1,2,mean)
diff51 <- apply(sc5,2,mean) - apply(sc1,2,mean)
diff61 <- apply(sc6,2,mean) - apply(sc1,2,mean)

senstable <- as.data.frame(rbind(diff21, diff31, diff41, diff51, diff61))
senstable$scenario <- senstable$scenario +1

senstable$scenario[order(senstable$totpopsize, decreasing=T)] # ranking scenarios according to totpopsize
senstable$scenario[order(senstable$bigtusker, decreasing=T)] # ranking scenarios according to bigtuskers
senstable$scenario[order(senstable$oldbigtuskers, decreasing=T)] # ranking scenarios according to old bigtuskers
senstable$scenario[order(senstable$allkilledBT, decreasing=T)] # ranking scenarios according to number bigtuskers killed
senstable$scenario[order(senstable$allkilledST, decreasing=T)] # ranking scenarios according to number smalltuskers killed
senstable$scenario[order(senstable$income, decreasing=T)] # ranking scenarios according to income


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

