library(dplyr)
library(data.table)
library(ggplot2)
library(nlme)
library(lme4)
library(doSNOW)
library(foreach)
library(doParallel)



# decayRate <- function(env, corr){
#   
#   # env  <- rnorm(100, 0, 1)
#   # corr <- 0.8
#   # Nb   <- 10
#   
#   #####
#   
#   Nb       <- length(env)
#   myMatrix <- matrix(0, nrow=Nb, ncol=Nb)
#   
#   if(corr == 0) corr <- 1e-10
#   alpha     <- abs(log(corr)) 
#   
#   myMatrix  <- exp(-1*alpha*abs(col(myMatrix, as.factor = FALSE)-row(myMatrix, as.factor = FALSE)))
#   newEnv    <-  myMatrix %*% env
#   
#   return(newEnv)
#   
# }


set.seed(15)# make sure sampling each time is independent

nID  <- 50
nRep <- 50 # State values of desired parameters
nPop <- 100

AR   <- 0.8

Ve   <- 0.6 * (1/sqrt(1 / (1 - AR^2)))^2
Vid  <- 0.4

inputs <- list("Tmax" = 50,
               "NI" = nID,
               "NP" = nPop,
               "NR" = nRep)
dt <- squid::squidR(inputs)[["full_data"]] %>% as.data.table

dt[ , ':='(I = rep(rnorm(1, 0, sqrt(Vid)), nRep),
           e = unclass(arima.sim(model=list(ar=AR),n=nRep, sd=sqrt(Ve), n.start = 200))[]),
    by=.(Replicate, Individual)]


# dt[ , ':='(I = rep(rnorm(1, 0, sqrt(Vid)), nRep),
#            e = scale(decayRate(rnorm(nRep, 0, 1), AR))[ ,1] * sqrt(Ve)), 
#     by=.(Replicate, Individual)]


dt[ , Phenotype := I + e]

# plot(dt[Individual == 1 & Replicate == 1, Phenotype], type="l")

#### Calculate the number of cores
numCores <- detectCores() - 1
cl       <- makeCluster(numCores)
registerDoSNOW(cl)

### Prepare progress bar ####
iterations <- length(unique(dt$Replicate))
pb         <- txtProgressBar(max = iterations, style = 3)
progress   <- function(n) setTxtProgressBar(pb, n)
opts       <- list(progress = progress)

res = foreach(r             = unique(dt$Replicate), 
              .packages     = c("nlme", "lme4", "data.table"),
              .combine      = "rbind",
              .options.snow = opts) %dopar% {
                         
            # r <- 1
            ####
                         
            dat <- dt[Replicate == r]
            mod1 <-lme(Phenotype ~ 1, random = ~1|Individual, data = dat)
            # plot(ACF(mod1, resType="normalized"))
            
            mod2 <-lmer(Phenotype ~ 1 + (1|Individual) + (1|Time), data = dat)
            
            mod3 <-lme(Phenotype ~ 1, random = ~1|Individual,  correlation=corCAR1(form=~Time|Individual), data = dat)
            # plot(ACF(mod2, resType="normalized"))
            
            data.table(Replicate = r,
                       Vid       = as.numeric(VarCorr(mod1)[1]),
                       Vid_ran   = as.data.table(VarCorr(mod2))[grp == "Individual", vcov],
                       Vid_cor   = as.numeric(VarCorr(mod3)[1]))
  }

stopCluster(cl)
close(pb)


dt_res <- melt(res, id.vars = c("Replicate"), measure.vars = c("Vid", "Vid_ran", "Vid_cor"))


ggplot(data=dt_res, aes(x=variable, y=value)) + 
  geom_boxplot() + 
  geom_hline(yintercept=Vid, colour="red")




