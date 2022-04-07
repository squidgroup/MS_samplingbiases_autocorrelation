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
  
  tar_target(s_param_5_TRUE,         f_init_sim_param(NR=5, X1_sto_shared=TRUE)),
  tar_target(s_param_5_FALSE,        f_init_sim_param(NR=5, X1_sto_shared=FALSE)),
 
  tar_target(s_sim_5_TRUE,           f_simulate_data(s_param_5_TRUE)),
  tar_target(s_sim_5_FALSE,          f_simulate_data(s_param_5_FALSE)),
  
  
  #++++++++++++++++++++++++++++++++++++++
  ##### ANALYSIS ########################
  #++++++++++++++++++++++++++++++++++++++
  
  tar_target(a_null,           f_fit_lmer(rbind(s_sim_5_TRUE, s_sim_5_FALSE),
                                          "null",
                                          "Phenotype ~ 1 + (1|Individual)")),
  tar_target(a_time_fix,       f_fit_lmer(rbind(s_sim_5_TRUE, s_sim_5_FALSE),
                                          "time_fix",
                                          "Phenotype ~ 1 + scale(Time) + (1|Individual)")),
  tar_target(a_time_ran,       f_fit_lmer(rbind(s_sim_5_TRUE, s_sim_5_FALSE),
                                          "time_ran",
                                          "Phenotype ~ 1 + (1|Individual) + (1|Time)")),
  tar_target(a_time_fix_ran,   f_fit_lmer(rbind(s_sim_5_TRUE, s_sim_5_FALSE),
                                          "time_fix_ran",
                                          "Phenotype ~ 1 + scale(Time) + (1|Individual) + (1|Time)")),
  
  tar_target(a_id_period,      f_fit_lmer_id_period(rbind(s_sim_5_TRUE, s_sim_5_FALSE),
                                             "time_id_period",
                                             "Phenotype ~ 1 + (1|Individual) + (1|Period:Individual)")),
  tar_target(a_id_period2,      f_fit_lmer_id_period2(rbind(s_sim_5_TRUE, s_sim_5_FALSE),
                                             "time_id_period2",
                                             "Phenotype ~ 1 + (1|Individual) + (1|Time_period:Individual)")),
  tar_target(a_time_mc,        f_fit_lmer_mc(rbind(s_sim_5_TRUE, s_sim_5_FALSE),
                                             "time_mc",
                                             "Phenotype ~ 1 + scale(Time_mean) + scale(Time_dev) + (1|Individual)")),
  
  tar_target(a_ar1,            f_fit_nlme_ar1(rbind(s_sim_5_TRUE, s_sim_5_FALSE),
                                             "ar1",
                                             "Phenotype ~ 1 + (1|Individual)")),
  
  #++++++++++++++++++++++++++++++++++++++
  ##### RESULTS #########################
  #++++++++++++++++++++++++++++++++++++++
  
  tar_target(out_path,       "./output"),
  
  tar_target(r_var_figs,       f_variance_figs(rbind(a_null, 
                                                     a_time_fix, a_time_ran, a_time_fix_ran,
                                                     a_id_period, a_id_period2, 
                                                     a_time_mc, a_ar1),
                                               rbind(s_param_5_TRUE, s_param_5_FALSE),
                                               out_path)),
  
  tar_target(r_env_figs,       f_environment_figs(out_path)),
  
  
  tar_target(END, 0)
)