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
  
  tar_target(s_param_TRUE,         f_init_sim_param(X1_sto_shared=TRUE)),
  tar_target(s_param_FALSE,        f_init_sim_param(X1_sto_shared=FALSE)),
 
  tar_target(s_sim_TRUE,           f_simulate_data(s_param_TRUE)),
  tar_target(s_sim_FALSE,          f_simulate_data(s_param_FALSE)),
  
  
  #++++++++++++++++++++++++++++++++++++++
  ##### ANALYSIS ########################
  #++++++++++++++++++++++++++++++++++++++
  
  tar_target(a_null,           f_fit_lmer(rbind(s_sim_TRUE, s_sim_FALSE),
                                          "null",
                                          "Phenotype ~ 1 + (1|Individual)")),

  tar_target(a_time_ran,       f_fit_lmer(rbind(s_sim_TRUE, s_sim_FALSE),
                                          "time_ran",
                                          "Phenotype ~ 1 + (1|Individual) + (1|Time)")),

  tar_target(a_time_ind,       f_fit_lmer_id_period(rbind(s_sim_TRUE, s_sim_FALSE),
                                          "time_ind",
                                          "Phenotype ~ 1 + (1|Individual) + (1|Time) + (1|Time:Individual)")),
  
  
  
  
  tar_target(a_time_ran_all,   f_fit_lmer(rbind(s_sim_TRUE, s_sim_FALSE),
                                          "time_ran_all",
                                          "Phenotype ~ 1 + (1|Individual) + (1|Time)")),
  
  tar_target(a_time_ind_all,   f_fit_lmer(rbind(s_sim_TRUE, s_sim_FALSE),
                                          "time_ran_all",
                                          "Phenotype ~ 1 + (1|Individual) + (1|Time) + (1|Time:Individual)")),
  
  
  
  
  
  # tar_target(a_time_fix,       f_fit_lmer(rbind(s_sim_5_TRUE, s_sim_5_FALSE),
  #                                         "time_fix",
  #                                         "Phenotype ~ 1 + scale(Time) + (1|Individual)")),
  # tar_target(a_time_fix_ran,   f_fit_lmer(rbind(s_sim_5_TRUE, s_sim_5_FALSE),
  #                                         "time_fix_ran",
  #                                         "Phenotype ~ 1 + scale(Time) + (1|Individual) + (1|Time)")),
  
  # tar_target(a_time_ind2,      f_fit_lmer_id_period2(rbind(s_sim_5_TRUE, s_sim_5_FALSE),
  #                                            "time_ind2",
  #                                            "Phenotype ~ 1 + (1|Individual) + (1|Time_period:Individual) + (1|Time)")),
  # tar_target(a_time_mc,        f_fit_lmer_mc(rbind(s_sim_5_TRUE, s_sim_5_FALSE),
  #                                            "time_mc",
  #                                            "Phenotype ~ 1 + scale(Time_mean) + scale(Time_dev) + (1|Individual)")),
  
  # tar_target(a_ar1,            f_fit_nlme_ar1(rbind(s_sim_5_TRUE, s_sim_5_FALSE),
  #                                            "ar1",
  #                                            "Phenotype ~ 1 + (1|Individual)")),
  
  #++++++++++++++++++++++++++++++++++++++
  ##### RESULTS #########################
  #++++++++++++++++++++++++++++++++++++++
  
  tar_target(out_path,       "./output"),
  
  tar_target(r_var_figs,       f_variance_figs(rbind(a_null,
                                                     a_time_ran,
                                                     a_time_ind),
                                               rbind(s_param_TRUE, s_param_FALSE),
                                               out_path)),
  
  tar_target(r_env_figs,       f_environment_figs(out_path)),
  
  
  tar_target(END, 0)
)