f_fit_lmer <- function(dat, mlabel, fformula){
  
  # dat <- rbind(tar_read(s_sim_5_TRUE), tar_read(s_sim_5_FALSE))
  # mlabel   <- "null"
  # fformula <- "Phenotype ~ 1 + (1|Individual)"
  
  ####
  
  library(lme4)
  
  fit_model <- function(dt){
    
    fit  <- tryCatch(lmer(as.formula(fformula), data = dt), 
                     error = function(e){NULL})
    
    res <- data.frame("Sim_id"    = dt$Sim_id[1],
                      "Replicate" = as.numeric(dt$Replicate[1]),
                      
                      # estimated parameters from the model fit
                      "VI"  = ifelse(is.null(fit), NA, as.data.table(VarCorr(fit))[grp == "Individual", vcov]),
                      "Vw"  = ifelse(is.null(fit), NA, as.data.table(VarCorr(fit))[grp == "Residual", vcov]))
    return(res)
  }
  
  out <- dat                          %>%
          group_by(Sim_id, Replicate) %>%
          do(fit_model(.))            %>%
          ungroup()                   %>%
          mutate(Model = mlabel)
  
  return(out)
}