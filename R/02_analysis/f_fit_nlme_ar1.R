f_fit_nlme_ar1 <- function(dat, mlabel, fformula){
  
  # dat      <- rbind(tar_read(s_sim_5_TRUE), tar_read(s_sim_5_FALSE))
  # mlabel   <- "ar1"
  # fformula <- "Phenotype ~ 1 + (1|Individual)"
  
  ####
  
  library(nlme)
  
  fit_model <- function(dt){
    
    fit  <- tryCatch(lme(Phenotype ~ 1, random = ~ 1|Individual,
                         correlation=corAR1(form = ~ Time | Individual), data=dt), 
                     error = function(e){NULL})
    
    res <- data.frame("Sim_id"    = dt$Sim_id[1],
                      "Replicate" = as.numeric(dt$Replicate[1]),
                      
                      # estimated parameters from the model fit
                      "VI"  = ifelse(is.null(fit), NA, as.numeric(VarCorr(fit)[1])),
                      "Vw"  = ifelse(is.null(fit), NA, as.numeric(VarCorr(fit)[2])),
                      stringsAsFactors = FALSE)
    return(res)
  }
  
  out <- dat %>%
          group_by(Sim_id, Replicate) %>%
          do(fit_model(.))            %>%
          ungroup()                   %>%
          mutate(Model = mlabel)
  
  return(out)
}