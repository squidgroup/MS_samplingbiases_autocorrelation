f_fit_lmer <- function(dat, mlabel, fformula){
  
  # dat <- rbind(tar_read(s_sim_TRUE), tar_read(s_sim_FALSE))
  # mlabel   <- "null"
  # fformula <- "Phenotype ~ 1 + (1|Individual) + (1|Time)"
  
  ####
  
  library(lme4)
  
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
  
  out <- dat                          %>%
          group_by(Sim_id, Replicate) %>%
          do(fit_model(.))            %>%
          ungroup()                   %>%
          mutate(Model = mlabel)
  
  return(out)
}