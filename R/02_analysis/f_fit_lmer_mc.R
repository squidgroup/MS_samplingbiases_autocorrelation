f_fit_lmer_mc <- function(dat, mlabel, fformula){
  
  # dat      <- rbind(tar_read(s_sim_5_TRUE), tar_read(s_sim_5_FALSE))
  # mlabel   <- "time_mc"
  # fformula <- "Phenotype ~ 1 + scale(Time_mean) + scale(Time_dev) + (1|Individual)"
  
  #####
  
  dat <- dat %>% 
    
    .[ , Time_mean := mean(Time), by=Individual] %>% 
    .[ , Time_dev  := Time - Time_mean]
    
  
  out <- f_fit_lmer(dat, mlabel, fformula)
  
  return(out)
}