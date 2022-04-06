f_environment_figs <- function(path){
  
  library(foreach)
  
  set.seed(50)
  path <- file.path(path, "manuscript", "figures")
  
  parm <- as.data.table(
            expand.grid(
                "Tmax"                 = 100,
                
                "X1_state"             = TRUE,
                
                "X1_sto_state"         = TRUE,
                "X1_sto_shared"        = TRUE,
                "X1_sto_V"             = 1, 
                
                "X1_sto_autocor_state" = TRUE,
                "X1_sto_corr"          = c(0, 0.8),
                
                "X1_lin_state"         = c(TRUE, FALSE),
                "X1_lin_shared"        = TRUE,
                "X1_lin_slope"         = 0.15,
                
                "X1_cyc_state"         = c(TRUE, FALSE),
                "X1_cyc_shared"        = TRUE,
                "X1_cyc_amplitude"     = 5,
                "X1_cyc_period"        = 25))
  
  parm <- parm[!(X1_lin_state == TRUE & X1_cyc_state == TRUE)]
  parm[ , Sim_id := 1:.N]
  
  run_simulation <- function(par){
    
    # Select simulation parameters and convert table to list
    inputs    <- as.list(par)
    
    # Run simulation
    dt_list    <- squid::squidR(inputs)
    dt         <- as.data.table(dt_list$full_data)[ , .(X1, Time)]
    
    # Add simulation ids to data.frame
    dt[ , Sim_id := inputs$Sim_id]
    
    return(dt)
  }
  
  dat = foreach(i        = 1:nrow(parm), 
                .combine = rbind) %do% {
                  run_simulation(parm[i, ])
                }

  # combine simulated data with input parameters
  dat <- merge(dat, parm[ , .(Sim_id, X1_sto_corr, X1_lin_state, X1_cyc_state)], by="Sim_id")
  
  dat[ , Env_type := ifelse(X1_lin_state == FALSE & X1_cyc_state == FALSE, "stochastic", 
                     ifelse(X1_lin_state == TRUE  & X1_cyc_state == FALSE, "linear",
                     ifelse(X1_lin_state == FALSE & X1_cyc_state == TRUE,  "cyclic", NA_character_)))]
  
  p <- ggplot(data=dat) + 
        geom_line(aes(x=Time, y=X1)) +
        facet_grid(Env_type ~ X1_sto_corr) +
        ylab("X") +
        theme_bw()
  
  # save figures
  ggsave(filename = file.path(path, "fig_environments.png"),
         plot     = p)
  
  return(p)
}
