f_init_sim_param <- function(NI                   = 50,
                             NP                   = 50,
                             NR                   = 10,
                             Nrep                 = 2,
                             
                             VI                   = c(0, 0.4),
                             VS                   = 0,
                             Ve                   = 0.05,
                             
                             Vhsi                 = seq(0, 0.8, by=0.2),
                             ST_ind               = FALSE,
                             Tmax                 = 100,
                             
                             B1                   = 1,
                             
                             X1_state             = TRUE,
                             
                             X1_sto_state         = TRUE,
                             X1_sto_shared        = TRUE,
                             X1_sto_V             = 1, 
                             
                             X1_sto_autocor_state = TRUE,
                             X1_sto_corr          = c(0 , 0.4, 0.8),
                             
                             X1_lin_state         = c(TRUE, FALSE),
                             X1_lin_slope         = 0.15,
                             X1_lin_shared        = c(TRUE),
                             
                             X1_cyc_state         = c(TRUE, FALSE),
                             X1_cyc_shared        = c(TRUE),
                             X1_cyc_amplitude     = 5,
                             X1_cyc_period        = 25,
                             Rep                  = 1){
  
  #########################################
  ### initiate simulation parameters ######
  #########################################
  
  out <- as.data.table(
    expand.grid("NI"                   = NI,
                "NP"                   = NP,
                "NR"                   = NR,
                "Nrep"                 = Nrep, 
                
                "VI"                   = VI,
                "VS"                   = VS,
                "Ve"                   = Ve,
                
                "Vhsi"                 = Vhsi,
                "ST_ind"               = ST_ind,
                "Tmax"                 = Tmax,
                
                "B1"                   = B1,
                
                "X1_state"             = X1_state,
                
                "X1_sto_state"         = X1_sto_state,
                "X1_sto_shared"        = X1_sto_shared,
                "X1_sto_V"             = X1_sto_V, 
                
                "X1_sto_autocor_state" = X1_sto_autocor_state,
                "X1_sto_corr"          = X1_sto_corr,
                
                "X1_lin_state"         = X1_lin_state,
                "X1_lin_slope"         = X1_lin_slope,
                "X1_lin_shared"        = X1_lin_shared,
                
                "X1_cyc_state"         = X1_cyc_state,
                "X1_cyc_shared"        = X1_cyc_shared,
                "X1_cyc_amplitude"     = X1_cyc_amplitude,
                "X1_cyc_period"        = X1_cyc_period))
  
  # remove scenarios where the environment has a linear and cyclic effect in the same time 
  out <- out[!(X1_lin_state == TRUE & X1_cyc_state == TRUE)]
  
  # calculate environmental effect variance
  # out$B1 <- sqrt(1 - (out$VI + out$VS + out$Ve))
  out$VE <- out$B1^2
  
  # add simulation id
  out[ , Sim_id := paste0(1:.N, "_NR", NR, "_shared", X1_sto_shared)]
  
  return(out)
}