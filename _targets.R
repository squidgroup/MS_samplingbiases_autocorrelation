library("targets")
library("tarchetypes")
library("future")
library("future.callr")

plan(callr)

source("R/functions.R") # loads functions

# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr", "data.table", 
                            "ggplot2", "patchwork"))

# A List of target objects.
list(
  
  #++++++++++++++++++++++++++++++++++++++
  ##### SIMULATION ######################
  #++++++++++++++++++++++++++++++++++++++
  
  # set simulation parameters when environment is shared among individuals
  tar_target(s_param_TRUE,         f_init_sim_param(X1_sto_shared=TRUE)),
  # set simulation parameters when environment is not shared among individuals
  tar_target(s_param_FALSE,        f_init_sim_param(X1_sto_shared=FALSE)),
 
  # simulate data with environment shared among individuals
  tar_target(s_sim_TRUE,           f_simulate_data(s_param_TRUE),
             format="fst_dt",
             resources = tar_resources(
               fst = tar_resources_fst(compress = 100)
             )),
  # simulate data with environment not shared among individuals
  tar_target(s_sim_FALSE,          f_simulate_data(s_param_FALSE),
             format="fst_dt",
             resources = tar_resources(
               fst = tar_resources_fst(compress = 100)
             )),
  
  ####
  
  # set simulation parameters when total duration is 1000 time steps
  tar_target(s_param_large_NR,     f_init_sim_param(Tmax=1000, NR=100, Nrep=1, VI=0.4, Vhsi=c(0, 0.4, 0.8), X1_sto_corr=c(0.8),
                                                    X1_lin_state=FALSE, X1_cyc_state=FALSE, X1_sto_shared=FALSE)),

  # simulate data with total duration is 1000 time steps
  tar_target(s_sim_large_NR,       f_simulate_data(s_param_large_NR),
             format="fst_dt",
             resources = tar_resources(
               fst = tar_resources_fst(compress = 100)
             )),
  
  #++++++++++++++++++++++++++++++++++++++
  ##### ANALYSIS ########################
  #++++++++++++++++++++++++++++++++++++++
  
  # fit models
  tar_target(a_null,           f_fit_lmer(rbind(s_sim_TRUE, s_sim_FALSE),
                                          "null",
                                          "Phenotype ~ 1 + (1|Individual)")),
  
  ##
  
  tar_target(a_time_fix,       f_fit_lmer(rbind(s_sim_TRUE, s_sim_FALSE),
                                          "time_fix",
                                          "Phenotype ~ 1 + Time + (1|Individual)")),

  tar_target(a_time_ran,       f_fit_lmer(rbind(s_sim_TRUE, s_sim_FALSE),
                                         "time_ran",
                                         "Phenotype ~ 1 + (1|Individual) + (1|Time)")),

  tar_target(a_time_ind,       f_fit_lmer(rbind(s_sim_TRUE, s_sim_FALSE),
                                          "time_ind",
                                          "Phenotype ~ 1 + (1|Individual) + (1|Time) + (1|Time:Individual)")),

  ##

  tar_target(a_ar1,            f_fit_glmmTMB(s_sim_TRUE,
                                         "ar1",
                                         "Phenotype ~ 1 + (1|Individual) + ar1(factor(Time)+0|Replicate)")),

  tar_target(a_ar1_ind,        f_fit_glmmTMB(s_sim_FALSE,
                                         "ar1_ind",
                                         "Phenotype ~ 1 + (1|Individual) + ar1(factor(Time)+0|Individual)")),
  
  tar_target(a_ar1_ind_large_NR, f_fit_glmmTMB(s_sim_large_NR,
                                             "ar1_ind",
                                             "Phenotype ~ 1 + (1|Individual) + ar1(factor(Time)+0|Individual)")),
  
  
  #++++++++++++++++++++++++++++++++++++++
  ##### RESULTS #########################
  #++++++++++++++++++++++++++++++++++++++
  
  tar_target(out_path,       "./output"),
  
  # make figures
  
  tar_target(r_var_figs,      f_variance_figs(data.table::rbindlist(list(a_null,
                                                                          a_time_ran,
                                                                          a_time_fix,
                                                                          a_time_ind,
                                                                          a_ar1,
                                                                          a_ar1_ind),
                                                                     fill=TRUE),
                                               rbind(s_param_TRUE, s_param_FALSE),
                                               out_path)),
  
  
  tar_target(r_var_figs_all, f_variance_figs_all(data.table::rbindlist(list(a_null,
                                                                            a_time_ran,
                                                                            a_time_fix,
                                                                            a_time_ind,
                                                                            a_ar1,
                                                                            a_ar1_ind),
                                                                       fill=TRUE),
                                                 rbind(s_param_TRUE, s_param_FALSE), 
                                                 out_path)),
  
  
  tar_target(r_var_figs_alllarge_NR, f_variance_figs_all_large_NR(a_ar1_ind_large_NR,
                                                                  s_param_large_NR, 
                                                                  out_path)),
  
  
  tar_target(r_env_figs,       f_environment_figs(out_path)),
  
  tar_target(r_samp_figs,      f_sampling_figs(out_path)),
  
  
  tar_target(END, 0)
)