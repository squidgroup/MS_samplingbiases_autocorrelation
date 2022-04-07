f_fit_lmer_id_period <- function(dat, mlabel, fformula){
  
  # dat      <- rbind(tar_read(s_sim_5_TRUE), tar_read(s_sim_5_FALSE))
  # mlabel   <- "time_id_period"
  # fformula <- "Phenotype ~ 1 + (1|Individual) + (1|Time) + (1|Time:Individual)"
  
  #####
  
  # dat[ , Period := rep(seq(1:ceiling(.N/2)), each=2)[1:.N], by=.(Sim_id, Replicate, Individual)]
    
  out <- f_fit_lmer(dat, mlabel, fformula)
  
  
  return(out)
}
