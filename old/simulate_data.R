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

NR <- 10

########################################
### initiate simulation variables ######
########################################

# autocorrelation in residuals
parameters <- as.data.table(
                  expand.grid("NI"                   = 100,
                              "NP"                   = 100,
                              "NR"                   = c(100),
                              
                              "VI"                   = 1/3,
                              "VS"                   = 0,
                              "Ve"                   = 1/3,
                              "e_corr"               = c(0.1, 0.9),
                              "arima"                = c(FALSE),
                              
                              "Vhsi"                 = seq(0, 0.8, by=0.4),
                              "ST_ind"               = FALSE,
                              "Tmax"                 = 100,
                              
                              "X1_state"             = TRUE,
                              
                              "X1_sto_state"         = TRUE,
                              "X1_sto_shared"        = c(TRUE, FALSE),
                              
                              "X1_sto_autocor_state" = FALSE,
                              "X1_sto_corr"          = c(0)))

# autocorrelation in X

parameters2 <- as.data.table(
                expand.grid("NI"                   = 100,
                            "NP"                   = 100,
                            "NR"                   = c(100),
                            
                            "VI"                   = 1/3,
                            "VS"                   = 0,
                            "Ve"                   = 1/3,
                            "e_corr"               = c(0),
                            "arima"                = c(FALSE),
                            
                            "Vhsi"                 = seq(0, 0.8, by=0.4),
                            "ST_ind"               = FALSE,
                            "Tmax"                 = 100,
                            
                            "X1_state"             = TRUE,
                            
                            "X1_sto_state"         = TRUE,
                            "X1_sto_shared"        = c(TRUE, FALSE),
                            
                            "X1_sto_autocor_state" = TRUE,
                            "X1_sto_corr"          = c(0.1, 0.9)))


parameters <- rbind(parameters, parameters2)

# environmental effect variance
parameters$B1 <- sqrt(1 - (parameters$VI + parameters$VS + parameters$Ve))
parameters$VE <- parameters$B1^2


# add simulation id
max_Sim_id        <- nrow(parameters)
parameters$Sim_id <- 1:max_Sim_id

#######################
# Run all simulations #
#######################

run_sim <- function(param){

  # i     <- 1
  # param <- parameters[i, ]
  
  ####
  
  # Select simulation parameters and convert table to list
  inputs           <- as.list(param)
  
  # create mean population coefficient vector
  inputs$B         <- c(0, inputs$B1, 0, 0)
  
  # Create variance/correlation matrix
  inputs$Vind      <- matrix(0, nrow = 4, ncol = 4)
  inputs$Vind[1,1] <- inputs$VI
  inputs$Vind[2,2] <- inputs$VS
  
  # Run simulation
  dt_list <- squid::squidR(inputs, plot = TRUE)
  
  dt_full    <- as.data.table(dt_list$full_data)[ , .(Replicate, Individual, Time, Phenotype, I, S1, B1, X1, e)]
  dt_sampled <- as.data.table(dt_list$sampled_data)[ , .(Replicate, Individual, Time)]
  
  if(inputs$e_corr > 0){
    
    # remove residuals before adding autocorrelation
    dt_full[ , Phenotype := Phenotype - e]
    
    # add residual with autocorrelation
    if(inputs$arima == TRUE){
      Ve <- inputs$Ve * (1/sqrt(1 / (1 - inputs$e_corr^2)))^2
      dt_full[  , e := scale(unclass(arima.sim(model=list(ar=inputs$e_corr),n=length(e), sd=sqrt(1), n.start = 200))[]) * sqrt(inputs$Ve), by = .(Replicate, Individual)]
    }else{
      dt_full[  , e := scale(decayRate(e, inputs$e_corr, length(e))) * sqrt(inputs$Ve), by = .(Replicate, Individual)] 
    }
    
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
                 .packages     = c("data.table", "squid"),
                 .combine      = rbind,
                 .options.snow = opts) %dopar% {
  run_sim(parameters[i, ])
}

stopCluster(cl)
close(pb)

# Save simulation parameters
fwrite(parameters, paste0("C:/Users/HASSEN/Documents/MS3_squid_data/simulated_data/MS3_parameters_NR_",NR,".csv"))

dt_sim[, GRP := .GRP, by = .(Sim_id, Replicate)]

# Save simulated data as multiple files (one for each sim_Id and Replicate)
dt_sim[ , fwrite(.SD, file = paste0("C:/Users/HASSEN/Documents/MS3_squid_data/simulated_data/NR_10/MS3_simulated_data_NR_",NR,"_SimIdRep_",GRP[1],".csv"))
        , by = .(GRP)]


# Save simulated data as a single file
saveRDS(dt_sim, file = paste0("C:/Users/HASSEN/Documents/MS3_squid_data/simulated_data/MS3_simulated_data_NR_",NR,".rds"))

tictoc::toc()
