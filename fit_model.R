library(dplyr)
library(lme4)
library(nlme)
library(glmmTMB)
library(data.table)
library(doSNOW)
library(foreach)
library(doParallel)

NR            <- 10
path_in       <- "C:/Users/HASSEN/Documents/MS3_squid_data/simulated_data/"
file_path_in  <- paste0(path_in, "MS3_parameters_NR_", NR,".csv")
file_path_out <- paste0("C:/Users/HASSEN/Documents/MS3_squid_data/fit_model_data/MS3_fit_model_data_NR_", NR,".csv")

# read data
squid_param <- fread(file_path_in)
GRP         <- 1:(nrow(squid_param) * squid_param[1,NP])


# function that analyses the data of each parameter combination and replicate
fit_model <- function(dt){

  # sim <- 350
  # dt  <- fread(paste0(path_in, "/NR_", NR, "/MS3_simulated_data_NR_",NR,"_SimIdRep_",sim,".csv"))
  
  ####
  
  dt$Individual <- as.factor(dt$Individual)

  # m0 <- lmer(Phenotype ~ 1 + (1|Individual), data = dt)
  # m1 <- lmer(Phenotype ~ 1 + (1|Individual) + (1|Time), data = dt)
  # 
  # 
  # acf(residuals(m0))
  # acf(residuals(m1))
  
  # declare fitted models
  mm_list <- data.frame(Model    = c("null", 
                                     
                                     "time.random"),
                        
                        Model_eq = c("Phenotype ~ 1 + (1|Individual)",
                                     "Phenotype ~ 1 + (1|Individual) + (1|Time)"))

  
  fit_specific_model <- function(dt_model, dt_mm){
    
    # fit model
    mm  <- tryCatch(lmer(as.formula(as.character(dt_model$Model_eq)), data = dt_mm), 
                    error = function(e){NULL})

    res <- data.frame("Sim_id"    = as.numeric(dt_mm$Sim_id[1]),
                      "Replicate" = as.numeric(dt_mm$Replicate[1]),
                      
                      # estimated parameters from the model fit
                      "VI"  = ifelse(is.null(mm), NA, as.data.table(VarCorr(mm))[grp == "Individual", vcov]),
                      "Ve"  = ifelse(is.null(mm), NA, as.data.table(VarCorr(mm))[grp == "Residual", vcov]))
    
    return(res)
  }
  
  mm_res <- mm_list              %>%
              group_by(Model)    %>%
              do(fit_specific_model(., dt)) %>%
              ungroup()          %>%
              mutate(Model = as.character(Model))
  
  ### AR1 function
  # fit model
  # mm   <- tryCatch(lme(Phenotype ~ 1, random = ~ 1|Individual,
  #                      correlation=corAR1(form = ~ Time | Individual), data=dt),
  #                  error = function(e){NULL})
  
  # mm.corAR1 <- data.frame("Model"    = as.character("corAR1"),
  #                        "Sim_id"    = as.numeric(dt$Sim_id[1]),
  #                        "Replicate" = as.numeric(dt$Replicate[1]),
  # 
  #                        # estimated parameters from the model fit
  #                        "VI"  = ifelse(is.null(mm), NA, as.numeric(VarCorr(mm)[1])),
  #                        "Ve"  = ifelse(is.null(mm), NA, as.numeric(VarCorr(mm)[2])),
  #                        stringsAsFactors = FALSE)
  
  
  dt[ , Time := factor(Time)]
  mm   <- tryCatch(glmmTMB(Phenotype ~ 1 + (1|Individual) + ar1(Time - 1|Individual), data=dt),
                   error = function(e){NULL})
  
  VI <- insight::get_variance_random(mm)
  Ve <- insight::get_variance_residual(mm)
  
  mm.corAR1 <- data.frame("Model"    = as.character("corAR1"),
                         "Sim_id"    = as.numeric(dt$Sim_id[1]),
                         "Replicate" = as.numeric(dt$Replicate[1]),

                         # estimated parameters from the model fit
                         "VI"  = ifelse(is.null(mm), NA, ifelse(is.null(VI), 0, VI)),
                         "Ve"  = ifelse(is.null(mm), NA, ifelse(is.null(Ve), 0, Ve)),
                         stringsAsFactors = FALSE)
                         
  mm_res <- rbind(mm_res, mm.corAR1)
  
  # ### CorExp function
  # # fit model
  # mm   <- tryCatch(lme(Phenotype ~ 1, random = ~ 1|Individual,
  #                      correlation=corExp(form = ~ Time | Individual), data=dt), 
  #                  error = function(e){NULL})
  # 
  # mm.corExp <- data.frame("Model"     = as.character("lme.corExp"),
  #                         "Sim_id"    = as.numeric(dt$Sim_id[1]),
  #                         "Replicate" = as.numeric(dt$Replicate[1]),
  #                         
  #                         # estimated parameters from the model fit
  #                         "VI"  = ifelse(is.null(mm), NA, as.numeric(VarCorr(mm)[1])),
  #                         "Ve"  = ifelse(is.null(mm), NA, as.numeric(VarCorr(mm)[2])), 
  #                         stringsAsFactors = FALSE)
  # 
  # mm_res <- rbind(mm_res, mm.corExp)
  
  return(mm_res)

}

# Calculate the number of cores
numCores <- detectCores() - 2
cl       <- makeCluster(numCores)
registerDoSNOW(cl)

### Prepare progress bar ####
iterations <- length(GRP)
pb         <- txtProgressBar(max = iterations, style = 3)
progress   <- function(n) setTxtProgressBar(pb, n)
opts       <- list(progress = progress)

tictoc::tic()

results = as.data.table(foreach(sim = GRP, 
                                .packages     = c("dplyr", "lme4", "nlme", "glmmTMB", "data.table", "insight"),
                                .combine      = "rbind",
                                .options.snow = opts) %dopar% {
                                  
                                  dt <- fread(paste0(path_in, "/NR_", NR,
                                                     "/MS3_simulated_data_NR_",NR,"_SimIdRep_",sim,".csv"))
                                  
                                  res <- fit_model(dt)
                                  
                                  rm(dt)
                                  gc()
                                  
                                  return(res)
                                  
                                })

stopCluster(cl)
close(pb)

tictoc::toc()

fwrite(results, file = file_path_out)
