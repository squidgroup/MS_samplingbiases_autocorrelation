library(data.table)
library(doSNOW)
library(foreach)
library(doParallel)

decayRate <- function(env,corr, Nb){
  
  # env  <- rnorm(100, 0, 1)
  # corr <- 0.8
  # Nb   <- 10
  
  #####
  
  myMatrix <- matrix(0, nrow=Nb, ncol=Nb)
  
  if(corr == 0) corr <- 1e-10
  alpha     <- abs(log(corr)) 
  
  myMatrix  <- exp(-1*alpha*abs(col(myMatrix, as.factor = FALSE)-row(myMatrix, as.factor = FALSE)))
  
  newEnv    <-  myMatrix %*% env
  
  return(newEnv)
  
}

########################################
### initiate simulation variables ######
########################################

parameters <- as.data.table(
                  expand.grid("NI"                   = 100,
                              "NP"                   = 100,
                              "NR"                   = c(10),
                              
                              "VI"                   = c(0.0, 0.2, 0.4),
                              "VS"                   = 0,
                              "Ve"                   = 0.05,
                              "e_corr"               = c(0, 0.4, 0.8),
                              
                              "Vhsi"                 = seq(0, 0.8, by=0.1),
                              "ST_ind"               = FALSE,
                              "Tmax"                 = 100,
                              
                              "X1_state"             = TRUE,
                              
                              "X1_sto_state"         = TRUE,
                              "X1_sto_shared"        = c(TRUE, FALSE),
                              
                              "X1_sto_autocor_state" = TRUE,
                              "X1_sto_corr"          = c(0 , 0.4, 0.8),
                              
                              "X1_lin_state"         = c(TRUE, FALSE),
                              "X1_lin_slope"         = 0.1,
                              "X1_lin_shared"        = c(TRUE),
                              
                              "X1_cyc_state"         = c(TRUE, FALSE),
                              "X1_cyc_shared"        = c(TRUE),
                              "X1_cyc_amplitude"     = 2,
                              "X1_cyc_period"        = 25))

# environmental effect variance
parameters$B1 <- sqrt(1 - (parameters$VI + parameters$VS + parameters$Ve))
parameters$VE <- parameters$B1^2
# parameters    <- parameters[!(X1_lin_state == FALSE & X1_lin_shared == FALSE), ]
# parameters    <- parameters[!(X1_cyc_state == FALSE & X1_cyc_shared == FALSE), ]

# add simulation id
max_Sim_id        <- nrow(parameters)
parameters$Sim_id <- 1:max_Sim_id

#######################
# Run all simulations #
#######################

run_sim <- function(param){

  # i <- 2
  
  # Select simulation parameters and convert table to list
  inputs           <- as.list(param)
  
  # create mean population coefficient vector
  inputs$B         <- c(0, inputs$B1, 0, 0)
  
  # Create variance/correlation matrix
  inputs$Vind      <- matrix(0, nrow = 4, ncol = 4)
  inputs$Vind[1,1] <- inputs$VI
  inputs$Vind[2,2] <- inputs$VS
  
  # Run simulation
  dt_list <- squid::squidR(inputs)
  
  dt_full    <- as.data.table(dt_list$full_data)[ , .(Replicate, Individual, Time, Phenotype, I, S1, B1, X1, e)]
  
  dt_sampled <- as.data.table(dt_list$sampled_data)[ 
    , .(Replicate, Individual, Time)]
  
  if(inputs$e_corr > 0){
    # remove residuals before adding autocorrelation
    dt_full[ , Phenotype := Phenotype - e]
    
    # add residual with autocorrelation
    dt_full[  , e := scale(decayRate(e, inputs$e_corr, length(e))) * sqrt(inputs$Ve), by = .(Replicate, Individual)]
    
    # recalculate Phenotype with autocorrelated residuals
    dt_full[ , Phenotype := Phenotype + e]
  }
  
  setkey(dt_full, Replicate, Individual, Time); setkey(dt_sampled, Replicate, Individual, Time); 
  dt_sampled <- merge(dt_sampled , dt_full, by = c("Replicate", "Individual", "Time"), all.x = TRUE)
  
  # Add simulation ids to data.frame
  return(data.table(dt_sampled, "Sim_id" = inputs$Sim_id))
}


# Calculate the number of cores
numCores <- detectCores() - 1
cl       <- makeCluster(numCores)
registerDoSNOW(cl)

### Prepare progress bar ####
iterations <- nrow(parameters)
pb         <- txtProgressBar(max = iterations, style = 3)
progress   <- function(n) setTxtProgressBar(pb, n)
opts       <- list(progress = progress)

tictoc::tic()

dt_sim = foreach(i = 1:nrow(parameters), 
                 # .export       = c('run_sim','decayRate'),
                 .packages     = c("data.table", "squid"),
                 .combine      = rbind,
                 .options.snow = opts) %dopar% {
  run_sim(parameters[i, ])
}

stopCluster(cl)
close(pb)

# Save simulation parameters
fwrite(parameters, "C:/Users/HASSEN/Documents/MS3_squid_data/simulated_data/MS3_parameters_NR_10.csv")

# dt_sim <- fread(file = "C:/Users/HASSEN/Documents/MS3_squid_data/simulated_data/MS3_simulated_data_NR_10.csv")
# dt_sim <- readRDS("C:/Users/HASSEN/Documents/MS3_squid_data/simulated_data/MS3_simulated_data_NR_10.rds")

dt_sim[, GRP := .GRP, by = .(Sim_id, Replicate)]

# Save simulated data as multiple files (one for each sim_Id and Replicate)
dt_sim[ , fwrite(.SD, file = paste0("C:/Users/HASSEN/Documents/MS3_squid_data/simulated_data/NR_10/MS3_simulated_data_NR_10_SimIdRep_",GRP[1],".csv"))
        , by = .(GRP)]


# Save simulated data as a single file
saveRDS(dt_sim, file = "C:/Users/HASSEN/Documents/MS3_squid_data/simulated_data/MS3_simulated_data_NR_10.rds")

tictoc::toc()
