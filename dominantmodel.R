# function that sets up matrices and runs the model
# DOMINANT inheritance

# this function includes the updated demography and scenarios based on Jeanetta's email from 21 August

dominantmodel <- function(scenario=1,  # management scenario
                          nsims, nyears, percquota, errorrate, # number and length of simulations, quotapercent and illegal big-tusker hunt
                          surv1.m, surv2_10.m, surv11_16.m, surv15plus.m, # survival males
                          surv1.f, surv2_10.f, surv11plus.f, # survival females
                          cinterval.f, sexratio, # calfing interval and sex ratio at birth
                          maxageclasses.LSm, initialpoulationsize.LSm, initialagedistribution.LSm,  # LS bulls
                          maxageclasses.LSf, initialpoulationsize.LSf, initialagedistribution.LSf,  # LS cows
                          maxageclasses.LLm, initialpoulationsize.LLm, initialagedistribution.LLm,  # LL bulls
                          maxageclasses.LLf, initialpoulationsize.LLf, initialagedistribution.LLf,  # LL cows
                          maxageclasses.SSm, initialpoulationsize.SSm, initialagedistribution.SSm,  # SS bulls
                          maxageclasses.SSf, initialpoulationsize.SSf, initialagedistribution.SSf  # SS cows
){
  
  repro13_plus.f <-1/cinterval.f # number of calves per female in breeding age (13 or older) per year
  

# setting up loop to run many simulations
# #######################################
totpopsize <- rep(NA, nsims)
bigtuskers <- rep(NA, nsims)
oldbigtuskers <- rep(NA, nsims)
allkilledBT <- rep(NA, nsims)
allkilledST <- rep(NA, nsims)

for (j in 1:nsims){

# ############################################################
# Set initial population size and propagate population forward
# ############################################################
# ***********************************************************************************************

# n.LSm is the matrix with LS male population size over time: rows are age classes, columns time steps
n.LSm<-matrix(0, nrow=maxageclasses.LSm+1, ncol=nyears)
n.LSm[,1] <- rmultinom(1,size=initialpoulationsize.LSm,prob=initialagedistribution.LSm)  # initial stage distribution

# n.LSf is the matrix with LS female population size over time: rows are age classes, columns time steps
n.LSf<-matrix(0, nrow=maxageclasses.LSf+1, ncol=nyears)
n.LSf[,1] <- rmultinom(1,size=initialpoulationsize.LSf,prob=initialagedistribution.LSf)  # initial stage distribution

# n.LLm is the matrix with LL large-tusked male population size over time: rows are age classes, columns time steps
n.LLm<-matrix(0, nrow=maxageclasses.LLm+1, ncol=nyears)
n.LLm[,1] <- rmultinom(1,size=initialpoulationsize.LLm,prob=initialagedistribution.LLm)  # initial stage distribution

# n.LLf is the matrix with LL large-tusked female population size over time: rows are age classes, columns time steps
n.LLf<-matrix(0, nrow=maxageclasses.LLf+1, ncol=nyears)
n.LLf[,1] <- rmultinom(1,size=initialpoulationsize.LLf,prob=initialagedistribution.LLf)  # initial stage distribution

# n.SSm is the matrix with SS small tusked male population size over time: rows are age classes, columns time steps
n.SSm<-matrix(0, nrow=maxageclasses.SSm+1, ncol=nyears)
n.SSm[,1] <- rmultinom(1,size=initialpoulationsize.SSm,prob=initialagedistribution.SSm)  # initial stage distribution

# n.SSf is the matrix with SS small tusked female population size over time: rows are age classes, columns time steps
n.SSf<-matrix(0, nrow=maxageclasses.SSf+1, ncol=nyears)
n.SSf[,1] <- rmultinom(1,size=initialpoulationsize.SSf,prob=initialagedistribution.SSf)  # initial stage distribution

totquota<-rep(NA,nyears)
nbulls<-rep(NA,nyears)  # number of huntable big tuskers
nbulls21<-rep(NA,nyears)  # number of huntable big tuskers
nbulls35<-rep(NA,nyears)  # number of huntable big tuskers
totquota.all<-rep(NA,nyears)  # total quota for all elephants 
killedbigtuskers<-c(NA,rep(0,nyears-1))  # regular offtake of bigtuskers
bigtuskersbycatch<-c(NA,rep(0,nyears-1))  # illegal offtake of bigtuskers


# run the model
for (t in 2:nyears){

# heterozyous males
n.LSm[2,t]<-rbinom(1,n.LSm[1,t-1],surv1.m)
for(i in 2:9) n.LSm[i+1,t]<-rbinom(1,n.LSm[i,t-1],surv2_10.m)
for(i in 10:16) n.LSm[i+1,t]<-rbinom(1,n.LSm[i,t-1],surv11_16.m)
for(i in 17:(maxageclasses.LSm-1)) n.LSm[i+1,t]<-rbinom(1,n.LSm[i,t-1],surv15plus.m)
n.LSm[maxageclasses.LSm+1,t]<-rbinom(1,n.LSm[maxageclasses.LSm,t-1]+ n.LSm[maxageclasses.LSm+1,t-1],surv15plus.m)

# LS females (heterozygous)
n.LSf[2,t]<-rbinom(1,n.LSf[1,t-1],surv1.f)
for(i in 2:9) n.LSf[i+1,t]<-rbinom(1,n.LSf[i,t-1],surv2_10.f)
for(i in 10:(maxageclasses.LSf-1)) n.LSf[i+1,t]<-rbinom(1,n.LSf[i,t-1],surv11plus.f)
n.LSf[maxageclasses.LSf+1,t]<-rbinom(1,n.LSf[maxageclasses.LSf,t-1]+ n.LSf[maxageclasses.LSf+1,t-1],surv11plus.f)

# large-tusked homozygous males
n.LLm[2,t]<-rbinom(1,n.LLm[1,t-1],surv1.m)
for(i in 2:9) n.LLm[i+1,t]<-rbinom(1,n.LLm[i,t-1],surv2_10.m)
for(i in 10:16) n.LLm[i+1,t]<-rbinom(1,n.LLm[i,t-1],surv11_16.m)
for(i in 17:(maxageclasses.LLm-1)) n.LLm[i+1,t]<-rbinom(1,n.LLm[i,t-1],surv15plus.m)
n.LLm[maxageclasses.LLm+1,t]<-rbinom(1,n.LLm[maxageclasses.LLm,t-1]+ n.LLm[maxageclasses.LLm+1,t-1],surv15plus.m)

# females carrying large-tusked gene (homozygous)
n.LLf[2,t]<-rbinom(1,n.LLf[1,t-1],surv1.f)
for(i in 2:9) n.LLf[i+1,t]<-rbinom(1,n.LLf[i,t-1],surv2_10.f)
for(i in 10:(maxageclasses.LLf-1)) n.LLf[i+1,t]<-rbinom(1,n.LLf[i,t-1],surv11plus.f)
n.LLf[maxageclasses.LLf+1,t]<-rbinom(1,n.LLf[maxageclasses.LLf,t-1]+ n.LLf[maxageclasses.LLf+1,t-1],surv11plus.f)


# small-tusked males
n.SSm[2,t]<-rbinom(1,n.SSm[1,t-1],surv1.m)
for(i in 2:9) n.SSm[i+1,t]<-rbinom(1,n.SSm[i,t-1],surv2_10.m)
for(i in 10:16) n.SSm[i+1,t]<-rbinom(1,n.SSm[i,t-1],surv11_16.m)
for(i in 17:(maxageclasses.SSm-1)) n.SSm[i+1,t]<-rbinom(1,n.SSm[i,t-1],surv15plus.m)
n.SSm[maxageclasses.SSm+1,t]<-rbinom(1,n.SSm[maxageclasses.SSm,t-1]+ n.SSm[maxageclasses.SSm+1,t-1],surv15plus.m)

# females carrying small-tusked gene
n.SSf[2,t]<-rbinom(1,n.SSf[1,t-1],surv1.f)
for(i in 2:9) n.SSf[i+1,t]<-rbinom(1,n.SSf[i,t-1],surv2_10.f)
for(i in 10:(maxageclasses.SSf-1)) n.SSf[i+1,t]<-rbinom(1,n.SSf[i,t-1],surv11plus.f)
n.SSf[maxageclasses.SSf+1,t]<-rbinom(1,n.SSf[maxageclasses.SSf,t-1]+ n.SSf[maxageclasses.SSf+1,t-1],surv11plus.f)


# reproduction
# ------------
reprofemales <- n.LSf[maxageclasses.LSf+1,t]+n.LLf[maxageclasses.LLf+1,t]+n.SSf[maxageclasses.SSf+1,t] # total number of females in reproductive age classes
repromales <- sum(n.LSm[36:55,t])+sum(n.LLm[36:55,t])+sum(n.SSm[36:55,t])

# proportions in the population
m.LL <- sum(n.LLm[36:55,t])/repromales
m.LS <- sum(n.LSm[36:55,t])/repromales  
m.SS <- sum(n.SSm[36:55,t])/repromales  

f.LL <- n.LLf[maxageclasses.LLf+1,t]/reprofemales
f.LS <- n.LSf[maxageclasses.LSf+1,t]/reprofemales  
f.SS <- n.SSf[maxageclasses.SSf+1,t]/reprofemales  

# work out number of calves with different allele combination
calves<-rbinom(1,reprofemales, repro13_plus.f)  # number of calves born in population
calves.all <- rmultinom(1,size=calves,prob=c(m.LL*f.LL+0.5*f.LS*m.LL+0.5*f.LL*m.LS+0.25*f.LS*m.LS,  # LL
                                             f.LL*m.LS*0.5+f.LS*m.LL*0.5+f.LS*m.LS*0.5+f.LS*m.SS*0.5+f.SS*m.LS*0.5+f.LL*m.SS+f.SS*m.LL, # LS
                                             m.SS*f.SS+0.5*f.LS*m.SS+0.5*f.SS*m.LS+0.25*f.LS*m.LS)) # SS 


# work out sex for each group
n.LSm[1,t]<-rbinom(1,calves.all[2], sexratio)  # number of male LS calves
n.LSf[1,t]<-calves.all[2]-n.LSm[1,t] # number of female LS calves
n.LLm[1,t]<-rbinom(1,calves.all[1], sexratio)  # number of male LL calves
n.LLf[1,t]<-calves.all[1]-n.LLm[1,t] # number of female LL calves
n.SSm[1,t]<-rbinom(1,calves.all[3], sexratio)  # number of male SS calves
n.SSf[1,t]<-calves.all[3]-n.SSm[1,t] # number of female SS calves


# --------------------------------------------------------------------------------------------------------------------------
# different scenarios for hunting large-tusked bulls
# #############################################################################################
# ---------------------------------------------------------------------------------------------

if (scenario==1) totquota[t] <- 0 # Scenario 1 means no hunting at all
else {  # for all other scenarios, we need to work out how many individuals in each group get killed:
  
  totquota.all[t] <- floor((sum(n.LSm[,t])+sum(n.LSf[,t])+sum(n.LLm[,t])+sum(n.LLf[,t])+sum(n.SSm[,t])+sum(n.SSf[,t])) * percquota/100) # includes calves and juvs!
  
  
  ## scenario 2: entire quota spent on large-tusked bulls >35 years
  # if there are not enough individuals, rest of quote spend on small tuskers
  # -------------------------------------------------------------
  if (scenario == 2){
    nbulls[t] <- sum(n.LLm[35:maxageclasses.LLm+1,t]) + sum(n.LSm[35:maxageclasses.LSm+1,t])
    if(totquota.all[t]>=nbulls[t]) n.LLm[35:maxageclasses.LLm+1,t] <- n.LSm[35:maxageclasses.LLm+1,t] <-0 # if fewer elephants than quota, then all are being finished off
    else {
      pLS <- sum(n.LSm[35:maxageclasses.LSm+1,t])/(sum(n.LLm[35:maxageclasses.LLm+1,t])+sum(n.LSm[35:maxageclasses.LSm+1,t])) # proportion LS
      n.LSm[35:maxageclasses.LSm+1,t]<-table(factor(sample(rep(35:maxageclasses.LSm+1,n.LSm[35:maxageclasses.LSm+1,t]), sum(n.LSm[35:maxageclasses.LSm+1,t])-round(totquota.all[t]*pLS)), levels=35:maxageclasses.LSm+1))[]
      n.LLm[35:maxageclasses.LLm+1,t]<-table(factor(sample(rep(35:maxageclasses.LLm+1,n.LLm[35:maxageclasses.LLm+1,t]), sum(n.LLm[35:maxageclasses.LLm+1,t])-round(totquota.all[t]*(1-pLS))), levels=35:maxageclasses.LLm+1))[]
    }
    
    killedbigtuskers[t] <- min(totquota.all[t], nbulls[t])
    totquota[t]<-totquota.all[t] - killedbigtuskers[t]  # works out how many small-tuskers will be killed to fill quota
    
    # hunting of small-tusked bulls >35 years if quota is not filled by big tuskers
    if(totquota[t]>=(sum(n.SSm[35:maxageclasses.SSm+1,t]))) n.SSm[35:maxageclasses.SSm+1,t] <-0 # if fewer elephants than quota, then all are being finished off
    else {
      n.SSm[35:maxageclasses.SSm+1,t]<-table(factor(sample(rep(35:maxageclasses.SSm+1,n.SSm[35:maxageclasses.SSm+1,t]), sum(n.SSm[35:maxageclasses.SSm+1,t])-totquota[t]), levels=35:maxageclasses.SSm+1))[]
    }
  } # end scenario 2
  
  ## scenario 3: killing young large-tuskers 21-34 
  # -------------------------------------------------------------------------------------
  if (scenario == 3){
    
    nbulls[t] <- sum(n.LLm[21:34,t]) + sum(n.LSm[21:34,t])
    if(totquota.all[t]>=nbulls[t]) n.LLm[21:34,t] <- n.LSm[21:34,t] <-0 # if fewer elephants than quota, then all are being finished off
    else {
      pLS <- sum(n.LSm[21:34,t])/(sum(n.LLm[21:34,t])+sum(n.LSm[21:34,t])) # proportion LS
      n.LSm[21:34,t]<-table(factor(sample(rep(21:34,n.LSm[21:34,t]), sum(n.LSm[21:34,t])-round(totquota.all[t]) * pLS), levels=21:34))[]
      n.LLm[21:34,t]<-table(factor(sample(rep(21:34,n.LLm[21:34,t]), sum(n.LLm[21:34,t])-round(totquota.all[t])* (1-pLS)), levels=21:34))[]
    }
    
    killedbigtuskers[t] <- min(totquota.all[t], nbulls[t])
    totquota[t]<-totquota.all[t] - killedbigtuskers[t]  # works out how many small-tuskers will be killed to fill quota
    
    # hunting of small-tusked bulls >21 years if quota is not filled by big tuskers
    if(totquota[t]>=sum(n.SSm[21:34,t])) n.SSm[21:34,t] <-0 # if fewer elephants than quota, then all are being finished off
    else n.SSm[21:maxageclasses.SSm+1,t]<-table(factor(sample(rep(21:maxageclasses.SSm+1,n.SSm[21:maxageclasses.SSm+1,t]), sum(n.SSm[21:maxageclasses.SSm+1,t])-totquota[t]), levels=21:maxageclasses.SSm+1))[]
    
  } # end scenario 3
  
  ## scenario 4: quota is divided in half, each allocated to 21-34 year old large tuskers and 35+ large tuskers
  # # ---------------------------------------------------------------------------------------
  if (scenario == 4){
    
    nbulls21[t] <- sum(n.LLm[21:34,t]) + sum(n.LSm[21:34,t])
    nbulls35[t] <- sum(n.LLm[35:maxageclasses.LLm+1,t]) + sum(n.LSm[35:maxageclasses.LSm+1,t])
    
    if(totquota.all[t]/2>=nbulls35[t]) n.LLm[35:maxageclasses.LLm+1,t] <- n.LSm[35:maxageclasses.LSm+1,t] <-0 # if fewer elephants than quota, then all are being finished off
    else {
      pLS <- sum(n.LSm[35:maxageclasses.LSm+1,t])/(sum(n.LLm[35:maxageclasses.LLm+1,t])+sum(n.LSm[35:maxageclasses.LSm+1,t])) # proportion LS
      n.LSm[35:maxageclasses.LSm+1,t]<-table(factor(sample(rep(35:maxageclasses.LSm+1,n.LSm[35:maxageclasses.LSm+1,t]), sum(n.LSm[35:maxageclasses.LSm+1,t])-round(totquota.all[t])/2 * pLS), levels=35:maxageclasses.LSm+1))[]
      n.LLm[35:maxageclasses.LLm+1,t]<-table(factor(sample(rep(35:maxageclasses.LLm+1,n.LSm[35:maxageclasses.LLm+1,t]), sum(n.LLm[35:maxageclasses.LLm+1,t])-round(totquota.all[t])/2* (1-pLS)), levels=35:maxageclasses.LLm+1))[]
    }
    
    if(totquota.all[t]/2>=nbulls21[t]) n.LLm[21:34,t] <- n.LSm[21:34,t] <-0 # if fewer elephants than quota, then all are being finished off
    else {
      pLS <- sum(n.LSm[21:34,t])/(sum(n.LLm[21:34,t])+sum(n.LSm[21:34,t])) # proportion LS
      n.LSm[21:34,t]<-table(factor(sample(rep(21:34,n.LSm[21:34,t]), sum(n.LSm[21:34,t])-round(totquota.all[t])/2 * pLS), levels=21:34))[]
      n.LLm[21:34,t]<-table(factor(sample(rep(21:34,n.LSm[21:34,t]), sum(n.LLm[21:34,t])-round(totquota.all[t])/2* (1-pLS)), levels=21:34))[]
    }
    
    killedbigtuskers[t] <- min(totquota.all[t], totquota.all[t]/2 + nbulls21[t], totquota.all[t]/2 + nbulls35[t], nbulls21[t]+nbulls35[t])
    totquota[t]<-totquota.all[t] - killedbigtuskers[t]  # works out how many small-tuskers will be killed to fill quota
    
    # hunting of small-tusked bulls >21 years if quota is not filled by big tuskers
    if(totquota[t]>=sum(n.SSm[21:maxageclasses.SSm+1,t])) n.SSm[21:maxageclasses.SSm+1,t] <-0 # if fewer elephants than quota, then all are being finished off
    else n.SSm[21:maxageclasses.SSm+1,t]<-table(factor(sample(rep(21:maxageclasses.SSm+1,n.SSm[21:maxageclasses.SSm+1,t]), sum(n.SSm[21:maxageclasses.SSm+1,t])-totquota[t]), levels=21:maxageclasses.SSm+1))[]
    
  } # end scenario 4
  
  ## scenario 5: half the quota is for large-tusked bulls > 35, half for small-tusked bulls > 35 
  # --------------------------------------------------------------------------------------------
  if (scenario == 5){
    
    nbulls[t] <- sum(n.LLm[35:maxageclasses.LLm+1,t]) + sum(n.LSm[35:maxageclasses.LSm+1,t])

    if(totquota.all[t]/2>=nbulls[t]) n.LLm[35:maxageclasses.LLm+1,t] <- n.LSm[35:maxageclasses.LSm+1,t] <-0 # if fewer elephants than quota, then all are being finished off
    else {
      pLS <- sum(n.LSm[35:maxageclasses.LSm+1,t])/(sum(n.LLm[35:maxageclasses.LLm+1,t])+sum(n.LSm[35:maxageclasses.LSm+1,t])) # proportion LS
      n.LSm[35:maxageclasses.LSm+1,t]<-table(factor(sample(rep(35:maxageclasses.LSm+1,n.LSm[35:maxageclasses.LSm+1,t]), sum(n.LSm[35:maxageclasses.LSm+1,t])-round(totquota.all[t])/2 * pLS), levels=35:maxageclasses.LSm+1))[]
      n.LLm[35:maxageclasses.LLm+1,t]<-table(factor(sample(rep(35:maxageclasses.LLm+1,n.LSm[35:maxageclasses.LLm+1,t]), sum(n.LLm[35:maxageclasses.LLm+1,t])-round(totquota.all[t])/2* (1-pLS)), levels=35:maxageclasses.LLm+1))[]
    }
    
    killedbigtuskers[t] <- min(totquota.all[t]/2, nbulls[t])
    totquota[t]<-totquota.all[t] - killedbigtuskers[t]  # works out how many small-tuskers will be killed to fill quota
    
    # hunting of small-tusked bulls >35 
    if(totquota[t]>=sum(n.SSm[35:maxageclasses.SSm+1,t])) n.SSm[35:maxageclasses.SSm+1,t] <-0 # if fewer elephants than quota, then all are being finished off
    else n.SSm[35:maxageclasses.SSm+1,t]<-table(factor(sample(rep(35:maxageclasses.SSm+1,n.SSm[35:maxageclasses.SSm+1,t]), sum(n.SSm[35:maxageclasses.SSm+1,t])-totquota[t]), levels=35:maxageclasses.SSm+1))[]
    
  } # end scenario 5
  
  ## scenario 6: half the quota is for large-tusked bulls > 50, half for small-tusked bulls > 50 
  # --------------------------------------------------------------------------------------------
  if (scenario == 6){
    
    nbulls[t] <- sum(n.LLm[50:maxageclasses.LLm+1,t]) + sum(n.LSm[50:maxageclasses.LSm+1,t])
    if(totquota.all[t]/2>=nbulls[t]) n.LLm[50:maxageclasses.LLm+1,t] <- n.LSm[50:maxageclasses.LSm+1,t] <-0 # if fewer elephants than quota, then all are being finished off
    else {
      pLS <- sum(n.LSm[50:maxageclasses.LSm+1,t])/(sum(n.LLm[50:maxageclasses.LLm+1,t])+sum(n.LSm[50:maxageclasses.LSm+1,t])) # proportion LS
      n.LSm[50:maxageclasses.LSm+1,t]<-table(factor(sample(rep(50:maxageclasses.LSm+1,n.LSm[50:maxageclasses.LSm+1,t]), sum(n.LSm[50:maxageclasses.LSm+1,t])-round(totquota.all[t])/2 * pLS), levels=50:maxageclasses.LSm+1))[]
      n.LLm[50:maxageclasses.LLm+1,t]<-table(factor(sample(rep(50:maxageclasses.LLm+1,n.LSm[50:maxageclasses.LLm+1,t]), sum(n.LLm[50:maxageclasses.LLm+1,t])-round(totquota.all[t])/2* (1-pLS)), levels=50:maxageclasses.LLm+1))[]
    }
    
    killedbigtuskers[t] <- min(totquota.all[t]/2, nbulls[t])
    totquota[t]<-totquota.all[t] - killedbigtuskers[t]  # works out how many small-tuskers will be killed to fill quota
    
    # hunting of small-tusked bulls >50 
    if(totquota[t]>=sum(n.SSm[50:maxageclasses.SSm+1,t])) n.SSm[50:maxageclasses.SSm+1,t] <-0 # if fewer elephants than quota, then all are being finished off
    else n.SSm[50:maxageclasses.SSm+1,t]<-table(factor(sample(rep(50:maxageclasses.SSm+1,n.SSm[50:maxageclasses.SSm+1,t]), sum(n.SSm[50:maxageclasses.SSm+1,t])-totquota[t]), levels=50:maxageclasses.SSm+1))[]
    
  } # end scenario 6
  
  
  # 'accidental' killing of big-tuskers during small-tusker hunt
  # ------------------------------------------------------------------------
  # bycatch<-rbinom(1,totquota[t],errorrate)
  # if(bycatch>=sum(n.LSm[21:maxageclasses+1,t])) {n.LSm[21:maxageclasses+1,t] <-0 
  # bigtuskersbycatch[t]<-sum(n.LSm[21:maxageclasses+1,t])}
  # else {n.LSm[21:maxageclasses+1,t]<-table(factor(sample(rep(21:maxageclasses+1,n.LSm[21:maxageclasses+1,t]), sum(n.LSm[21:maxageclasses+1,t])-bycatch), levels=21:maxageclasses+1))[]
  # bigtuskersbycatch[t]<-bycatch}
  
} # end hunting scenarios

 } # end model run
# 
totpopsize[j] <- sum(n.LLf[,nyears]) + sum(n.LLm[,nyears]) + sum(n.LSf[,nyears]) + sum(n.LSm[,nyears]) + sum(n.SSf[,nyears]) + sum(n.SSm[,nyears])
bigtuskers[j] <- sum(n.LLm[,nyears]) + sum(n.LSm[,nyears])
oldbigtuskers[j] <- n.LLm[maxageclasses.LLm+1,nyears] + n.LSm[maxageclasses.LSm+1,nyears]
allkilledBT[j] <- sum(c(killedbigtuskers,bigtuskersbycatch), na.rm=T)
allkilledST[j] <- sum(totquota, na.rm=T)
} # end of loop

return(output <- data.frame(totpopsize, bigtuskers, oldbigtuskers, allkilledBT, allkilledST, scenario))

} # end of function

