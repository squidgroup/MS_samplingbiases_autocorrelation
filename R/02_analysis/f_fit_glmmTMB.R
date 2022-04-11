f_fit_glmmTMB <- function(dat, mlabel, fformula){
  
  # dat <- rbind(tar_read(s_sim_TRUE), tar_read(s_sim_FALSE))
  # mlabel   <- "time_ran"
  # fformula <- "Phenotype ~ 1 + (1|Individual) + (1|Time)"
  
  ####
  
  library(glmmTMB)
  
  fit_model <- function(dt){
    
    # dt <- dat[Sim_id == "50_NR10_sharedTRUE" & Replicate == 20]
  
    fit  <- tryCatch(glmmTMB(as.formula(fformula), data = dt), 
                     error = function(e){
                         print(dt$Sim_id[1])
                         print(dt$Replicate[1])
                         NULL
                       })
    
    res <- data.frame("Sim_id"    = dt$Sim_id[1],
                      "Replicate" = dt$Replicate[1])

    vcov_m <- broom.mixed::tidy(fit) %>% 
                filter(effect == "ran_pars") %>%
                mutate(name     = paste0(group, "_",term),
                       estimate = estimate^2) %>%
                select(name, estimate) %>%
                as.data.table()

    # get estimated variance values from the model fit
    m <- lapply(1:nrow(vcov_m), function(i){
      res[[vcov_m[i , name]]] <<- vcov_m[i , estimate]
    })
                      
    return(res)
  }
  
  out <- dat                          %>%
          group_by(Sim_id, Replicate) %>%
          do(fit_model(.))            %>%
          ungroup()                   %>%
          mutate(Model = mlabel)
  
  return(out)
}