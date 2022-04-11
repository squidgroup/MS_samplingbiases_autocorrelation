f_simulate_data <- function(parameters){
  
  # parameters <- tar_read(s_param_TRUE)
  
  ########################
  ### load packages ######
  ########################
  
  library(doSNOW)
  library(foreach)
  library(doParallel)
  library(squid)
  
  #######################
  # Run all simulations #
  #######################
  
  run_simulation <- function(param){
    
    # param <- parameters[1, ]
    
    # Select simulation parameters and convert table to list
    inputs           <- as.list(param)
    
    # create mean population coefficient vector
    inputs$B         <- c(0, inputs$B1, 0, 0)
    
    # Create variance/correlation matrix
    inputs$Vind      <- matrix(0, nrow = 4, ncol = 4)
    inputs$Vind[1,1] <- inputs$VI
    inputs$Vind[2,2] <- inputs$VS
    
    # Run simulation
    dt_list    <- squid::squidR(inputs, plot = TRUE)
    dt_sampled <- as.data.table(dt_list$sampled_data)[ , .(Phenotype, Replicate, Individual, Time, e)]
    
    # replicate samples per time and adjust residuals
    dt_sampled <- dt_sampled[rep(1:.N, each=2)]
    dt_sampled[ , Phenotype := Phenotype - e + rnorm(.N, 0, sqrt(inputs$Ve))]
    dt_sampled[ , c("e", "e2") := NULL]
    
    # Add simulation ids to data.frame
    dt_sampled[ , Sim_id     := inputs$Sim_id]
    dt_sampled[ , Replicate  := factor(Replicate)]
    dt_sampled[ , Individual := factor(Individual)]
    
    return(dt_sampled)
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
  
  out = foreach(i = 1:nrow(parameters), 
                   .export       = c("run_simulation"),
                   .packages     = c("data.table", "squid"),
                   .combine      = rbind,
                   .options.snow = opts) %dopar% {
                     run_simulation(parameters[i, ])
                   }
  
  stopCluster(cl)
  close(pb)
  
  return(out)
}
