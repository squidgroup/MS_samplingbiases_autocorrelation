library(lme4)
library(data.table)
library(doSNOW)
library(foreach)
library(doParallel)
library(ggplot2)
library(boot)

NR            <- 10
path_in       <- "C:/Users/HASSEN/Documents/MS3_squid_data/simulated_data/"
file_path_in  <- paste0(path_in, "MS3_parameters_NR_", NR,".csv")

# read data
squid_param <- fread(file_path_in)
setnames(squid_param, c("VI", "Ve"), c("VI_true", "Ve_true"))


GRP_data    <- data.table("Sim_id" = rep(squid_param$Sim_id, each= squid_param[1,NP]),
                          "GRP"    = 1:(nrow(squid_param) * squid_param[1,NP]))


GRP_data <- GRP_data[Sim_id %in% squid_param[  e_corr == 0 & 
                                                 X1_sto_shared == TRUE & 
                                                 X1_lin_state == FALSE & 
                                                 X1_cyc_state == FALSE &
                                                 VI_true %in% c(0.4) &
                                                 Vhsi %in% c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8) &
                                                 X1_sto_corr %in% c(0.8), 
                                               Sim_id]]

# get only the first replicate of each sim Id
GRP_data <- GRP_data[ , .(GRP = GRP[1]), by = Sim_id]

bootstap_data <- function(dt){
  
  # sim <- GRP_data$GRP[1]
  # dt  <- fread(paste0(path_in, "/NR_", NR,
  #                     "/MS3_simulated_data_NR_",NR,"_SimIdRep_",sim,".csv"))
  

 get_VTime <- function(data, indices) {
    
    d   <- data[indices,] # allows boot to select sample
    
    mm  <- tryCatch(lmer(Time ~ 1 + (1|Individual), data = d), 
                    error = function(e){NULL})
    
    res <- ifelse(is.null(mm), NA, 
                   as.data.table(VarCorr(mm))[grp == "Individual", vcov] / 
                     (as.data.table(VarCorr(mm))[grp == "Individual", vcov] + 
                        as.data.table(VarCorr(mm))[grp == "Residual", vcov]))
            
    return(res)
  }
  
  # bootstrapping with 1000 replications
  out <- boot(data=dt, statistic=get_VTime, R=2000)

  return(data.table("Vhsi_true" = squid_param[Sim_id == dt$Sim_id[1], Vhsi],
                    "Vhsi_est"  = out$t[,1]))
}

# Calculate the number of cores
numCores <- detectCores() - 1
cl       <- makeCluster(numCores)
registerDoSNOW(cl)

### Prepare progress bar ####
iterations <- length(GRP_data$GRP)
pb         <- txtProgressBar(max = iterations, style = 3)
progress   <- function(n) setTxtProgressBar(pb, n)
opts       <- list(progress = progress)


results = as.data.table(foreach(sim = GRP_data$GRP, 
                                .packages     = c("lme4", "boot", "data.table"),
                                .combine      = "rbind",
                                # .multicombine = TRUE,
                                .options.snow = opts) %dopar% {
                                  
                                  dt <- fread(paste0(path_in, "/NR_", NR,
                                                     "/MS3_simulated_data_NR_",NR,"_SimIdRep_",sim,".csv"))
                                  
                                  res <- bootstap_data(dt)
                                  
                                  rm(dt)
                                  gc()
                                  
                                  return(res)
                                  
                                })

stopCluster(cl)
close(pb)


p <- ggplot(data = results, aes(x=Vhsi_est)) + 
      geom_histogram(binwidth = 0.02) + 
      geom_vline(aes(xintercept = Vhsi_true), color="red") +
      facet_wrap(.~Vhsi_true) +
      xlab("Estimated V.time with bootstrapping") +
      theme_bw()

print(p)

ggsave("./figures/V_time_bootstrapping.png", plot = p)



