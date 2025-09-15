f_sampling_figs <- function(path){
  
  # path <- tar_read(out_path)
  
  ####
  
  library("foreach")
  
  #### example sampling schemes
  
  set.seed(50)
  path <- file.path(path, "manuscript", "figures")
  
  parm <- as.data.table(
    expand.grid(
      "Tmax"   = 100,
      "NI"     = 20,
      "NR"     = 5,
      "ST_ind" = FALSE,
      "Vhsi"   = seq(0, 0.8, by=0.4)))
  
  parm[ , Sim_id := 1:.N]
  
  run_simulation <- function(par){
    
    # Select simulation parameters and convert table to list
    inputs    <- as.list(par)
    
    # Run simulation
    dt_list    <- squid::squidR(inputs)
    dt         <- as.data.table(dt_list$sampled_data)[ , .(Individual, Time)]
    
    # Add simulation ids to data.frame
    dt[ , Sim_id := inputs$Sim_id]
    
    return(dt)
  }
  
  dat = foreach(i        = 1:nrow(parm), 
                .combine = rbind) %do% {
                  run_simulation(parm[i, ])
                }
  
  # combine simulated data with input parameters
  dat <- merge(dat, parm[ , .(Sim_id, Vhsi)], by="Sim_id")
  
  dat[ , Vhsi := factor(Vhsi, labels = c("R[IS]^2 == 0", "R[IS]^2 == 0.4", "R[IS]^2 == 0.8"))]
  
  (p <- ggplot(data=dat) + 
        geom_line(aes(x=Time, y=Individual, group=Individual), color="grey") +
        geom_point(aes(x=Time, y=Individual, group=Individual)) +
        facet_grid(. ~ Vhsi, labeller = label_parsed) +
        ylab("Individual id") +
        xlab("Time") +
        theme_bw())
  
  # save figures
  ggsave(filename = file.path(path, "fig_sampling.png"),
         plot     = p,
         width    = 6,
         height   = 2.5,
         units    = "in")
  
  ggsave(filename = file.path(path, "fig_sampling.pdf"),
         plot     = p,
         width    = 6,
         height   = 2.5,
         units    = "in",
         dpi      = 300)
  
  return(p)
}
