f_variance_figs_all_large_NR <- function(dat, param, path){
  
  # dat   <- tar_read(a_ar1_ind_large_NR)
  # param <- tar_read(s_param_large_NR)
  # path  <- tar_read(out_path)
  
  ####
  
  
  plot_fig <- function(dt, corr="X1_sto_corr", corr_label="Autocorrelation in X (rho)"){
    
    ggplot(data = dt, aes(x=as.factor(Vhsi), y=value, color=as.factor(dt[[corr]]))) +
      geom_boxplot() + 
      facet_grid(Model ~ VI_true_string + variable) +
      scale_color_discrete(name = corr_label) +
      # ylim(0, 1.2) +
      ylab("Variance value") + xlab("Repeatability in the timing of sampling") +
      theme_bw() + 
      theme(legend.position = "top",
            strip.text.y    = element_text(size = 8),
            axis.text.x     = element_text(angle = 67.5, hjust = 1, size = 7))
  }
  
  path <- file.path(path, "manuscript", "fig_testing")
  
  setnames(param, c("VI", "Ve"), c("VI_true", "Ve_true"))
  dat[, ':='(VI = Individual_sd__Intercept^2, Vw = Residual_sd__Observation^2)]
  
  ############
  ## stochastic + unshared
  ## ar1.time|ind
  
  Sim_id2 <- param[X1_sto_shared == FALSE & X1_lin_state == FALSE & X1_cyc_state == FALSE, Sim_id]
  dat2 <- dat[Model == "ar1_ind" & Sim_id %in% Sim_id2]
  dat2[, ':='(Vtime_ind = Individual_sd__Time^2)]
  setnames(dat2, "Individual_cor__TimeTime", "cor_time")
  
  # combine parameter estimate values and true values 
  dat2 <- merge(dat2, param[ , .(VI_true, Ve_true, Vhsi, X1_sto_shared, X1_sto_corr, Sim_id)], all.x=TRUE, 
                by="Sim_id") %>% as.data.table() %>%
    
    melt(., id.vars = c("Sim_id", "Replicate", "Model", "VI_true", "Ve_true", "Vhsi", "X1_sto_shared", "X1_sto_corr"), 
         measure.vars = c("VI", "Vtime_ind", "Vw", "cor_time"))
  
  dat2[ , VI_true_string := paste0("VI = ", VI_true)]
  
  p2 <- plot_fig(dat2[variable != "cor_time"])
  # correlation
  p22 <- plot_fig(dat2[variable == "cor_time"]) + geom_hline(yintercept = unique(dat2$X1_sto_corr)) + ylim(0, 1)
  
  ggsave(filename = file.path(path, "ar1.time.ind.ran_stochastic_unshared_large_NR.png"), 
         plot = p2, 
         width = 8, height = 3)  
  
  ggsave(filename = file.path(path, "ar1.time.ind.ran_stochastic_unshared_large_NR_cor.png"), 
         plot = p22, 
         width = 4, height = 4) 
  
  return(list(p2,
              p22))
}