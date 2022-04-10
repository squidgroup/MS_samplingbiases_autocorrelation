f_fit_glmmTMB_vcov_ar1 <- function(dat, mlabel, fformula){
  
  dat      <- rbind(tar_read(s_sim_TRUE), tar_read(s_sim_FALSE))
  mlabel   <- "null"
  fformula <- "Phenotype ~ 1 + (1|Individual) + (1|Time)"
  
  
  param <- tar_read(s_param_TRUE)
  param[VI == 0.4 & Vhsi == 0 & X1_sto_corr == 0.8 & X1_cyc_state == FALSE & X1_lin_state == FALSE, .(Sim_id)]
  
  
  library(glmmTMB)
  
  
  dat <- dat[Sim_id == "82_NR10_sharedTRUE" & Replicate == 1]
  
  dat[ , Time := factor(Time)]
  dat[ , Individual := factor(Individual)]
  
  
  fit <- glmmTMB(Phenotype ~ ar1(0+Time|Individual), data=dat)
  
  
  
  
  
  
  
  
  fit_model <- function(dt){
    
    # dt <- dat[Sim_id == Sim_id[1] & Replicate == 1]
    
    fit  <- tryCatch(lmer(as.formula(fformula), data = dt), 
                     error = function(e){NULL})
    
    res <- data.frame("Sim_id"    = dt$Sim_id[1],
                      "Replicate" = as.numeric(dt$Replicate[1]))
    
    vcov_m <- as.data.frame(VarCorr(fit))
    
    # get estimated variance values from the model fit
    m <- lapply(1:nrow(vcov_m), function(i){
      res[[paste0("V_", vcov_m[i , "grp"])]] <<- vcov_m[i , "vcov"]
    })
    
    return(res)
  }
  

}