library(dplyr)
library(data.table)
library(ggplot2)
library(nlme)
library(lme4)
library(doSNOW)
library(foreach)
library(doParallel)


set.seed(21)

Tmax <- 100
nID  <- 50
nRep <- 10 # State values of desired parameters
nPop <- 100

AR   <- 0.8

Ve   <- 0.01
Vid  <- 0.4


Vind <- matrix(0, nrow=4, ncol=4)
Vind[1, 1] <- Vid

inputs <- list("Tmax" = Tmax,
               "NI" = nID,
               "NP" = nPop,
               "NR" = nRep,
               
               "B" = c(0,0.2,0,0),
               "Vind" = Vind,
               "Ve" = 0.01,
               
               "Vhsi" = 0.8,
               "ST_ind" = FALSE,
               
               "X1_state" = TRUE,
               "X1_sto_state" = TRUE,
               "X1_sto_shared" = TRUE,
               "X1_sto_autocor_state" = TRUE,
               "X1_sto_corr" = AR
               )

dt <- squid::squidR(inputs)[["full_data"]] %>% as.data.table


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
                plot(ACF(mod1, resType="normalized"))
                
                mod2 <-lmer(Phenotype ~ 1 + (1|Individual) + (1|Time), data = dat)
                
                
                mod3 <-lme(Phenotype ~ 1, random = ~1|Individual,  correlation=corCAR1(form=~Time|Individual), data = dat)
                plot(ACF(mod3, resType="normalized"))

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
