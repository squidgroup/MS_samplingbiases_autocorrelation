f_variance_figs <- function(dat, param, path){
  
  # dat   <- rbind(tar_read(a_null), tar_read(a_time_ran), tar_read(a_time_ind))
  # param <- rbind(tar_read(s_param_TRUE), tar_read(s_param_FALSE))
  # path  <- tar_read(out_path)
  
  ####
  
  path <- file.path(path, "manuscript", "figures")
  
  
  setnames(param, c("VI", "Ve"), c("VI_true", "Ve_true"))
  
  # combine parameters estimate values and true values 
  dat <- merge(dat, param[ , .(VI_true, Ve_true, Vhsi, X1_sto_shared, X1_sto_corr, 
                               X1_lin_state, X1_cyc_state, VE, Sim_id)], 
               by="Sim_id") %>% as.data.table() %>%
    
          melt(., id.vars = c("Sim_id", "Replicate", "Model", "VI_true", "Ve_true", "Vhsi", "X1_sto_shared", "X1_sto_corr", "X1_lin_state", "X1_cyc_state", "VE"), 
               measure.vars = c("VI", "Vw"))
  
  setnames(dat, c("variable", "value"), c("variable_est", "value_est"))
  
  dat[variable_est == "VI",  value_true := VI_true]
  dat[variable_est == "Vw",  value_true := Ve_true]
  
  dat[ , VI_true_string := paste0("VI = ", VI_true)]
  
  dat[variable_est == "Vw", Vw_true := VE + value_true]
  
  dat[ , Model := factor(Model, levels = c("null",
                                          "time_ran",
                                          "time_ind"))]
  
  plot_fig <- function(dt, corr="X1_sto_corr", corr_label="Autocorrelation in X"){
    
    ggplot(data = dt, aes(x=as.factor(Vhsi), y=value_est, color=as.factor(dt[[corr]]))) +
      geom_boxplot() + 
      geom_hline(aes(yintercept  = value_true)) +
      geom_hline(aes(yintercept  = Vw_true), linetype="dotted", color="red") +
      facet_grid(Model ~ VI_true_string + variable_est) +
      scale_color_discrete(name = corr_label) +
      ylab("Variance value") + xlab("Among-individual variance in sampling") +
      theme_bw() + 
      theme(legend.position = "top",
            strip.text.y    = element_text(size = 8),
            axis.text.x     = element_text(angle = 67.5, hjust = 1, size = 7))
  }
  
  p_sto_shared <- plot_fig(dat[X1_lin_state == FALSE & X1_cyc_state == FALSE & X1_sto_shared == TRUE])
  p_lin_shared <- plot_fig(dat[X1_lin_state == TRUE  & X1_cyc_state == FALSE & X1_sto_shared == TRUE])
  p_cyc_shared <- plot_fig(dat[X1_lin_state == FALSE & X1_cyc_state == TRUE  & X1_sto_shared == TRUE])
  
  p_sto_unshared <- plot_fig(dat[X1_lin_state == FALSE & X1_cyc_state == FALSE & X1_sto_shared == FALSE])
  p_lin_unshared <- plot_fig(dat[X1_lin_state == TRUE  & X1_cyc_state == FALSE & X1_sto_shared == FALSE])
  p_cyc_unshared <- plot_fig(dat[X1_lin_state == FALSE & X1_cyc_state == TRUE  & X1_sto_shared == FALSE])
  
  # save figures
  ggsave(filename = file.path(path, "fig_sto_shared.png"),
         plot = p_sto_shared)
  ggsave(filename = file.path(path, "fig_lin_shared.png"),
         plot = p_lin_shared)
  ggsave(filename = file.path(path, "fig_cyc_shared.png"),
         plot = p_cyc_shared)
  
  ggsave(filename = file.path(path, "fig_sto_unshared.png"),
         plot = p_sto_unshared)
  ggsave(filename = file.path(path, "fig_lin_unshared.png"),
         plot = p_lin_unshared)
  ggsave(filename = file.path(path, "fig_cyc_unshared.png"),
         plot = p_cyc_unshared)
  
  
  return(list(p_sto_shared,
              p_lin_shared,
              p_cyc_shared,
              
              p_sto_unshared,
              p_lin_unshared,
              p_cyc_unshared))
}