# elephant model for Jeanetta
# this is an extended version of the model after a meeting between Jeanetta and Res on 22 Sept 2015 in Pretoria
# 6 May 2016: added a loop so we can run simulation many times
# 25 Aug 2017: incorporated some changes to demography, scenarios and permit prices (summarised in emails from Jeanetta 21 August 2017)
# 28 Aug 2017: made a function that runs the model

# we now have six components to the population:
# 1. bulls homozygous for large-tusk gene LL
# 2. females homozygous for large-tusk gene LL
# 3. heterozygous bulls LS (also large-tuskers if L dominant) 
# 4. heterozygous females LS
# 5. bulls homozygous for small-tusk gene SS
# 6. females homozygous for small-tusk gene SS

rm(list=ls())  # clear workspace

nyears<-50 # number of years we want to run the simulation for
percquota <- 0.5  # percent offtake, based on total population

# The model is set up to include illegal killing of big tuskers, 
# i.e. hunters purchase a permit for a small tusker but then accidentally kill a big tusker
# This feature is currently not run, however. To activate it, set the errorrate below and
# you would also need to uncomment (and check) some code in the 'recessivemodel.R' and 'dominantmodel.R' files
# errorrate <- 0.01  # number of big tuskers illegally killed for every allowed small tusker

nsims <- 1000  # number of simulations

permitbig <- 20000  # permit price for large tuskers
permitsmall<-2000   # permit price for small tuskers


# initial population size:
# figures similar from APNR: 
# 2080 males, of which 20% (416) are large-tuskers and 80% (1664) small-tuskers
# 2800 females
males <- 2080
females <- 2800

propLT <- 0.2 # proportion of large tuskers among males


# initial age distribution (proportional, these numbers will be scaled by initial population size)
femaleinitialagedistribution<-c(23, 15, 14, 10,  5, 10, 11, 13,  5,  2,  6,  8,  7, 55)
maleinitialagedistribution<-c(72, 56, 59, 59, 41, 51, 43, 57, 41, 35, 39, 19, 25, 20, 32, 24, 27, 18, 14, 24, 17, 19, 19, 18, 22, 11, 11, 10, 21, 13,
                              7, 15, 16, 16,  6, 15,  7,  9,  7,  7, 11,  8,  4,  7,  4,  6,  5,  6,  4,  3,  4,  9,  4,  1,  6, 54)

# #########################
# Demographic parameters
# Males
maxageclasses.m <- 55 # maximum age for males that we keep track of in detail (individuals older than this stay in a single age class)

# bull survival (average from paper!)
surv1.m <- 0.92  # first-year survival (8%)
surv2_10.m <- 0.95 # survival 2nd to 10th year
surv11_16.m <- 0.95 # survival 11th to 16th year
surv15plus.m <- 0.95 # survival after 16th

# Females
maxageclasses.f <- 13  # seems like we don't need to to differentiate age once females are in reproductive age

# cow survival (average from paper!)
surv1.f <- 0.92  # first-year survival (8%)
surv2_10.f <- 0.95 # survival 2nd to 10th year
surv11plus.f <- 0.95 # survival 11th year and older

cinterval.f <- 4 # calving interval in years
sexratio <- 0.5 # sex ratio at birth, proportion of male calves


# -------------------- DOMINANT inheritance --------------------------------------------

# assuming Hardy-Weinberg equilibrium and a ratio of 0.2 (416) large-tusked to 0.8 (1664) small-tusked elephants, we use proportions LL: p^2 = 0.01, LS: 2pq = 0.19 and SS: q^2=0.8
p <- 1-(sqrt(1-propLT)) # frequency of L allele in the population; because only SS bulls are small-tusked if L is dominant, we know that q^2 has to be equal to (1-propLT)

# initial population sizes
initialpoulationsize.LSm <- round(males * 2 * p * (1-p))  # LS bulls
initialpoulationsize.LSf <- round(females * 2 * p * (1-p))   # LS cows
initialpoulationsize.LLm <- round(males * p ^ 2)  # LL bulls
initialpoulationsize.LLf <- round(females * p ^ 2)  # LL cows
initialpoulationsize.SSm <- round(males * (1-p) ^ 2)  # SS bulls
initialpoulationsize.SSf <- round(females * (1-p) ^ 2)  # SS cows

source("dominantmodel.R") # read in function that does the simulation

for (sc in 1:6){ # the loop runs all scenarios
output <- dominantmodel(scenario=sc,  # management scenario
                          nsims, nyears, percquota, errorrate, # number and length of simulations, quotapercent and illegal big-tusker hunt
                          surv1.m, surv2_10.m, surv11_16.m, surv15plus.m, # survival males
                          surv1.f, surv2_10.f, surv11plus.f, # survival females
                          cinterval.f, sexratio, # calving interval and sex ratio at birth
                          maxageclasses.m, initialpoulationsize.LSm, maleinitialagedistribution,  # SL bulls
                          maxageclasses.f, initialpoulationsize.LSf, femaleinitialagedistribution,  # SL cows
                          maxageclasses.m, initialpoulationsize.LLm, maleinitialagedistribution,  # LL bulls
                          maxageclasses.f, initialpoulationsize.LLf, femaleinitialagedistribution,  # LL cows
                          maxageclasses.m, initialpoulationsize.SSm, maleinitialagedistribution,  # SS bulls
                          maxageclasses.f, initialpoulationsize.SSf, femaleinitialagedistribution  # SS cows
)
output$income <- output$allkilledST*permitsmall + output$allkilledBT*permitbig # calculate permit income
save(output, file=paste("dominant_scenario", sc, ".RData", sep=""))
}


# ------------------ RECESSIVE inheritance ------------------------------------------

# assuming Hardy-Weinberg equilibrium and a ratio of 0.2 (416) large-tusked to 0.8 (1664) small-tusked elephants, we use proportions LL: p^2 = 0.2, LS: 2pq = 0.49 and SS: q^2=0.31
p <- sqrt(propLT) # frequency of L allele in the population; because only LL bulls are large-tusked if L is recessive, we know that p^2 has to be equal to propLT

# initial population sizes
initialpoulationsize.LSm <- round(males * 2 * p * (1-p))  # LS bulls
initialpoulationsize.LSf <- round(females * 2 * p * (1-p))   # LS cows
initialpoulationsize.LLm <- round(males * p ^ 2)  # LL bulls
initialpoulationsize.LLf <- round(females * p ^ 2)  # LL cows
initialpoulationsize.SSm <- round(males * (1-p) ^ 2)  # SS bulls
initialpoulationsize.SSf <- round(females * (1-p) ^ 2)  # SS cows

source("recessivemodel.R") # read in function that does the simulation

for (sc in 1:6){ # the loop runs all scenarios
  output <- recessivemodel(scenario=sc,  # management scenario
                          nsims, nyears, percquota, errorrate, # number and length of simulations, quotapercent and illegal big-tusker hunt
                          surv1.m, surv2_10.m, surv11_16.m, surv15plus.m, # survival males
                          surv1.f, surv2_10.f, surv11plus.f, # survival females
                          cinterval.f, sexratio, # calving interval and sex ratio at birth
                          maxageclasses.m, initialpoulationsize.LSm, maleinitialagedistribution,  # SL bulls
                          maxageclasses.f, initialpoulationsize.LSf, femaleinitialagedistribution,  # SL cows
                          maxageclasses.m, initialpoulationsize.LLm, maleinitialagedistribution,  # LL bulls
                          maxageclasses.f, initialpoulationsize.LLf, femaleinitialagedistribution,  # LL cows
                          maxageclasses.m, initialpoulationsize.SSm, maleinitialagedistribution,  # SS bulls
                          maxageclasses.f, initialpoulationsize.SSf, femaleinitialagedistribution  # SS cows
  )
  output$income <- output$allkilledST*permitsmall + output$allkilledBT*permitbig # calculate permit income
  save(output, file=paste("recessive_scenario", sc, ".RData", sep=""))
}