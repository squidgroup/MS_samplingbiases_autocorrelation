library(targets)
library(tarchetypes)

source("R/functions.R") # loads functions

# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr", "data.table", 
                            "ggplot2", "patchwork"))

# End this file with a list of target objects.
list(
  
  #++++++++++++++++++++++++++++++++++++++
  ##### SIMULATION ######################
  #++++++++++++++++++++++++++++++++++++++
  
  tar_target(s_param_5_TRUE,         f_init_sim_param(NR=5, X1_sto_shared=TRUE, NP=1, NI=20)),
  tar_target(s_param_5_FALSE,        f_init_sim_param(NR=5, X1_sto_shared=FALSE, NP=1, NI=20)),
 
  tar_target(s_sim_5_TRUE,           f_simulate_data(s_param_5_TRUE)),
  tar_target(s_sim_5_FALSE,          f_simulate_data(s_param_5_FALSE)),
  
  
  #++++++++++++++++++++++++++++++++++++++
  ##### ANALYSIS ########################
  #++++++++++++++++++++++++++++++++++++++
  

  #++++++++++++++++++++++++++++++++++++++
  ##### RESULTS #########################
  #++++++++++++++++++++++++++++++++++++++
  
  
  
  tar_target(END, 0)
)