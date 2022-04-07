f_fit_lmer_id_period2 <- function(dat, mlabel, fformula){

  # dat      <- rbind(tar_read(s_sim_5_TRUE), tar_read(s_sim_5_FALSE))
  # mlabel   <- "time_id_period2"
  # fformula <- "Phenotype ~ 1 + (1|Individual) + (1|Time_period:Individual)"
  
  #####
  
  find_period <- function(dtime){
    tmin   <- which.min(dtime)
    period <- rep(FALSE, length(dtime))
    period[(tmin-1):tmin] <- TRUE
    period
  }
  
  dat[ , Time_delta := c(NA_integer_, Time[2:.N]-Time[1:(.N-1)]), by=.(Sim_id, Replicate, Individual)]
  dat[ , Period := find_period(Time_delta), by=.(Sim_id, Replicate)]
  dat[ , Time_period := Time]
  dat[Period == TRUE, Time_period := -100]
  
  out <- f_fit_lmer(dat, mlabel, fformula)
  
  
  return(out)
}