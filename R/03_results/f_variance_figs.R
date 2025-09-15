f_variance_figs <- function(dat, param, path){
  
  # dat   <- data.table::rbindlist(list(tar_read(a_null),
  #                                     tar_read(a_time_ran),
  #                                     tar_read(a_time_fix),
  #                                     tar_read(a_time_ind),
  #                                     tar_read(a_ar1),
  #                                     tar_read(a_ar1_ind)), fill=TRUE)
  # param <- rbind(tar_read(s_param_TRUE), tar_read(s_param_FALSE))
  # path  <- tar_read(out_path)

  ####
  
  path <- file.path(path, "manuscript", "figures", "variance_components")
  
  setnames(param, c("VI", "Ve"), c("VI_true", "Ve_true"))
  dat[, ':='(VI = Individual_sd__Intercept^2, Vw = Residual_sd__Observation^2)]
  
  # combine parameters estimate values and true values 
  dat <- merge(dat, param[ , .(VI_true, Ve_true, Vhsi, X1_sto_shared, X1_sto_corr, 
                               X1_lin_state, X1_cyc_state, VE, Sim_id)], 
               by="Sim_id") %>% as.data.table() %>%
    
          melt(., id.vars = c("Sim_id", "Replicate", "Model", "state", "VI_true", "Ve_true", "Vhsi", "X1_sto_shared", "X1_sto_corr", "X1_lin_state", "X1_cyc_state", "VE"), 
               measure.vars = c("VI", "Vw"))
  
  setnames(dat, c("variable", "value"), c("variable_est", "value_est"))
  
  dat[variable_est == "VI",  value_true := VI_true]
  dat[variable_est == "Vw",  value_true := Ve_true]
  
  dat[ , VI_true_string := paste0("sigma[I]^2 == ", VI_true)]
  
  dat[variable_est == "Vw", Vw_true := VE + value_true]
  
  dat[ , variable_est := factor(variable_est, labels = c(expression(hat(sigma[I])^2), expression(hat(sigma[W])^2)))]
  
  dat[ , Model := factor(Model, 
                         levels = c("null", "time_fix", "time_ran", "time_ind", "ar1", "ar1_ind"),
                         labels = c("'(A) null'", "'(B) time.fix'", "'(C) time.ran'", "'(D) time.ind.ran'", "'(E) ar1.time'", "'(E) ar1.time.ind'"))]
  
  
  
  plot_fig <- function(dt, corr="X1_sto_corr", corr_label=expression("Temporal autocorrelation in X ("~rho*")")){
    
    # dt         <- dat[X1_lin_state == FALSE & X1_cyc_state == TRUE  & X1_sto_shared == TRUE]
    # corr       <- "X1_sto_corr"
    # corr_label <- "Autocorrelation in X (rho)"
    
    pd <- position_dodge(1)
    dt_missing <- dt[ , .(missing = as.character(100 - (sum(state == "ok")/ max(as.numeric(Replicate))) * 100)), 
                    .(Model, VI_true_string, variable_est, Vhsi, X1_sto_corr)] %>%
                    .[missing == "0", missing := NA_character_]
    
    ggplot(data = dt, aes(x=as.factor(Vhsi), y=value_est, color=as.factor(dt[[corr]]))) +
      geom_boxplot(position=pd, outlier.size = 0.5) + 
      geom_hline(aes(yintercept  = value_true), linetype="dashed") +
      geom_hline(aes(yintercept  = Vw_true), linetype="dotted") +
      geom_text(data = dt_missing, aes(x=as.factor(Vhsi), y=Inf,
                                       label = missing,
                                       color=factor(X1_sto_corr)),
                vjust=1.5, hjust=0.5, size=2, position=pd, fontface="bold") +
      facet_grid(Model ~ VI_true_string + variable_est, labeller = label_parsed) +
      scale_color_discrete(name = corr_label) +
      ylim(0, max(dat$value_est)+0.05) +
      ylab("Variance value") + xlab(expression("Repeatability in the timing of sampling ("~R[IS]^2*")")) +
      theme_bw() + 
      theme(legend.position = "top",
            strip.text.y    = element_text(size = 8),
            axis.text.x     = element_text(angle = 67.5, hjust = 1, size = 7))
  }
  
  (p_sto_shared <- plot_fig(dat[X1_lin_state == FALSE & X1_cyc_state == FALSE & X1_sto_shared == TRUE]))
  p_lin_shared <- plot_fig(dat[X1_lin_state == TRUE  & X1_cyc_state == FALSE & X1_sto_shared == TRUE])
  p_cyc_shared <- plot_fig(dat[X1_lin_state == FALSE & X1_cyc_state == TRUE  & X1_sto_shared == TRUE])
  
  p_sto_unshared <- plot_fig(dat[X1_lin_state == FALSE & X1_cyc_state == FALSE & X1_sto_shared == FALSE])
  p_lin_unshared <- plot_fig(dat[X1_lin_state == TRUE  & X1_cyc_state == FALSE & X1_sto_shared == FALSE])
  p_cyc_unshared <- plot_fig(dat[X1_lin_state == FALSE & X1_cyc_state == TRUE  & X1_sto_shared == FALSE])
  
  # save figures
  ggsave(filename = file.path(path, "fig_sto_shared.png"),
         plot = p_sto_shared)
  ggsave(filename = file.path(path, "pdf/fig_sto_shared.pdf"),
         plot = p_sto_shared, dpi = 300)
  
  ggsave(filename = file.path(path, "fig_lin_shared.png"),
         plot = p_lin_shared)
  ggsave(filename = file.path(path, "pdf/fig_lin_shared.pdf"),
         plot = p_lin_shared, dpi = 300)
  
  ggsave(filename = file.path(path, "fig_cyc_shared.png"),
         plot = p_cyc_shared)
  ggsave(filename = file.path(path, "pdf/fig_cyc_shared.pdf"),
         plot = p_cyc_shared, dpi = 300)
  
  ggsave(filename = file.path(path, "fig_sto_unshared.png"),
         plot = p_sto_unshared)
  ggsave(filename = file.path(path, "pdf/fig_sto_unshared.pdf"),
         plot = p_sto_unshared, dpi = 300)
  
  ggsave(filename = file.path(path, "fig_lin_unshared.png"),
         plot = p_lin_unshared)
  ggsave(filename = file.path(path, "pdf/fig_lin_unshared.pdf"),
         plot = p_lin_unshared, dpi = 300)
  
  ggsave(filename = file.path(path, "fig_cyc_unshared.png"),
         plot = p_cyc_unshared)
  ggsave(filename = file.path(path, "pdf/fig_cyc_unshared.pdf"),
         plot = p_cyc_unshared, dpi = 300)
  
  
  return(list(p_sto_shared,
              p_lin_shared,
              p_cyc_shared,
              
              p_sto_unshared,
              p_lin_unshared,
              p_cyc_unshared))
}
