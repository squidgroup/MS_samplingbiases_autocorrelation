library(dplyr)
# library(mgcv)
# library(itsadug)
library(lme4)
library(nlme)
library(data.table)
library(doSNOW)
library(foreach)
library(doParallel)
library(ggplot2)

NR            <- 10
path_in       <- "C:/Users/HASSEN/Documents/MS3_squid_data/simulated_data/"
file_path_in  <- paste0(path_in, "MS3_parameters_NR_", NR,".csv")

# read data
squid_param <- fread(file_path_in)
setnames(squid_param, c("VI", "Ve"), c("VI_true", "Ve_true"))


GRP_data    <- data.table("Sim_id" = rep(squid_param$Sim_id, each= squid_param[1,NP]),
                          "GRP"    = 1:(nrow(squid_param) * squid_param[1,NP]))


GRP_data <- GRP_data[Sim_id %in% squid_param[  e_corr %in% c(0) & 
                                               X1_sto_shared == TRUE & 
                                               X1_lin_state == FALSE & 
                                               X1_cyc_state == FALSE &
                                               VI_true %in% c(0.4) &
                                               Vhsi %in% c(0.0) &
                                               X1_sto_corr %in% c(0.8), 
                                             Sim_id]]

# function that analyses the data of each parameter combination and replicate
fit_model <- function(dt){

  # sim <- GRP_data$GRP[1]
  # dt  <- fread(paste0(path_in, "/NR_", NR, "/MS3_simulated_data_NR_",NR,"_SimIdRep_",sim,".csv"))

  #####
  
  dt$Individual <- as.factor(dt$Individual)
  
  # dt <- dt %>% 
  #   group_by(Individual)                        %>% 
  #   mutate(diff = Time - shift(Time))           %>%
  #   mutate(Time_mean   = round(mean(Time)),
  #          Time_min    = min(Time))             %>%
  #   mutate(Time_dev    = Time - Time_mean,
  #          Time_dev_min = Time - Time_min)      %>%
  #   mutate(Time_period = trunc(Time / (min(diff, na.rm = TRUE)+1)) * (min(diff, na.rm = TRUE)+1)) %>%
  #   mutate(Time_period = trunc(Time / 15) * 15) %>%
  #   mutate(Ind_period  = paste0(Individual, "_", Time_period)) %>%
  #   mutate(sTime  = as.numeric(scale(Time))) %>%
  #   setDT
  
  
  # dt <- start_event(dt, column="Time", event=c("Individual"), label.event ="event")
  # 
  # # squid_param[Sim_id == GRP_data$Sim_id[1]]
  # m0 <- bam(Phenotype ~ 1 + s(Individual, bs='re'), data=dt)
  # 
  # acf_resid(m0)
  # r1 <- start_value_rho(m0, plot=TRUE)
  # 
  # m1 <- bam(Phenotype ~ 1 + s(Individual, bs='re'), data=dt, rho=r1, AR.start=dt$start.event)
  # acf_resid(m1)
  # acf_resid(m1, split_pred = c("Individual"))
  
  
  ##### lmer
  
  lmer_res <- function(squid.dt, mm, label) {
    
    mm.out <- data.frame("Model"      = label,
                         "Sim_id"    = as.numeric(squid.dt$Sim_id[1]),
                         "Replicate" = as.numeric(squid.dt$Replicate[1]),
                         
                         # estimated parameters from the model fit
                         "VI"  = ifelse(is.null(mm), NA, as.data.table(VarCorr(mm))[grp == "Individual", vcov]),
                         "Ve"  = ifelse(is.null(mm), NA, as.data.table(VarCorr(mm))[grp == "Residual", vcov]))
  }

  # NULL
  mm  <- tryCatch(lmer(Phenotype ~ 1 + (1|Individual), data = dt), 
                  error = function(e){NULL})
  mm.res <- lmer_res(dt, mm, "Null_lmer")
  
  
  
  ##### nlme
  
  nlme_res <- function(squid.dt, mm, label) {
    
    mm.out <- data.frame("Model"      = label,
                         "Sim_id"    = as.numeric(squid.dt$Sim_id[1]),
                         "Replicate" = as.numeric(squid.dt$Replicate[1]),
                         
                         # estimated parameters from the model fit
                         "VI"  = ifelse(is.null(mm), NA, as.numeric(VarCorr(mm)["(Intercept)", "Variance"])),
                         "Ve"  = ifelse(is.null(mm), NA, as.numeric(VarCorr(mm)["Residual", "Variance"])))
  }
  
  mm  <- tryCatch(lme(Phenotype ~ 1 , data=dt, method="REML", random = list(Individual = ~ 1)), 
                  error = function(e){NULL})
  mm.res <- rbind(mm.res, nlme_res(dt, mm, "Null_nlme"))
  
  
  mm  <- tryCatch(lme(Phenotype ~ 1 , data=dt, method="REML", random = list(Individual = ~ 1),
                      correlation=corCAR1(form=~Time|Individual)), 
                  error = function(e){NULL})
  mm.res <- rbind(mm.res, nlme_res(dt, mm, "Null_nlme_cor"))
  
  # plot(ACF(mm0, resType="normalized"))
  
  return(mm.res)
  
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
                                .packages     = c("dplyr", "lme4", "nlme", "data.table"),
                                .combine      = "rbind",
                                # .multicombine = TRUE,
                                .options.snow = opts) %dopar% {
                                  
                                  dt  <- fread(paste0(path_in, "/NR_", NR, "/MS3_simulated_data_NR_",NR,"_SimIdRep_",sim,".csv"))
                                  
                                  res <- fit_model(dt)
                                  
                                  rm(dt)
                                  gc()
                                  
                                  return(res)
                                  
                                })

stopCluster(cl)
close(pb)

# combine fitted results and input parameters 
setkey(results, Sim_id); setkey(squid_param, Sim_id);
dt <- merge(results, squid_param[ , .(VI_true, Ve_true, Vhsi, X1_sto_corr, VE, Sim_id)], all.x=TRUE, by="Sim_id")



ggplot(data=dt, aes(x=Model, y=VI)) + geom_boxplot()







dt <- melt(dt, id.vars = c("Sim_id", "Replicate", "Model", "VI_true", "Ve_true", 
                           "Vhsi", "X1_sto_corr", "VE"), 
                 measure.vars = c("VI", "Ve"))

setnames(dt, c("variable", "value"), c("variable_est", "value_est"))

dt[variable_est == "VI",  value_true := VI_true]
dt[variable_est == "Ve",  value_true := Ve_true]

dt[ , VI_true_string := paste0("VI = ", VI_true)]

dt[variable_est == "Ve", Vw_true := VE + value_true]


p <- ggplot(data = dt, aes(x=as.factor(Vhsi), y=value_est, color=as.factor(X1_sto_corr))) +
      geom_boxplot() + 
      geom_hline(aes(yintercept  = value_true)) +
      geom_hline(aes(yintercept  = Vw_true), linetype="dotted", color="red") +
      facet_grid(Model ~ VI_true_string + variable_est) +
      scale_color_discrete(name = "Autocorrelation in X") +
      ylab("Variance value") + xlab("Among-individual variance in sampling") +
      theme_bw() + 
      theme(legend.position = "top",
            strip.text.y    = element_text(size = 8),
            axis.text.x     = element_text(angle = 67.5, hjust = 1, size = 7))

print(p)



