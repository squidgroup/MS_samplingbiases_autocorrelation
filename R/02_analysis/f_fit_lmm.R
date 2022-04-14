f_fit_lmm <- function(dat, mlabel, fformula, fct="lmer"){
  
  # dat      <- rbind(tar_read(s_sim_TRUE), tar_read(s_sim_FALSE))
  # dat      <- tar_read(s_sim_TRUE)
  # mlabel   <- "ar1_ind"
  # fformula <- "Phenotype ~ 1 + (1|Individual) + ar1(factor(Time)+0|Replicate)"
  # fct      <- "glmmTMB"
  
  ####
  
  if(fct == "lmer"){
    library(lme4)
  }else if(fct == "glmmTMB") {
    library(glmmTMB)
  }
  
  term_list <- c("Individual_sd__(Intercept)", 
                 "Residual_sd__Observation", 
                 "Time_sd__(Intercept)",
                 "Time:Individual_sd__(Intercept)",
                 "Replicate_sd__factor(Time)1",
                 "Individual.1_sd__factor(Time)1")

  
  fit_model <- function(dt){
    
    # dt <- dat[Sim_id == unique(Sim_id)[1] & Replicate == 1]

    fit  <- tryCatch(
            if(fct == "lmer"){
              lmer(as.formula(fformula), data = dt)
            }else if(fct == "glmmTMB") {
              glmmTMB(as.formula(fformula), data = dt)
            },
            error = function(e){
               print(dt$Sim_id[1])
               print(dt$Replicate[1])
               NULL
             })

    res <- data.frame("Sim_id"    = dt$Sim_id[1],
                      "Replicate" = dt$Replicate[1])

    if(!is.null(fit)){
    
      # check convergence
      if(fct == "lmer"){
        convergence <- TRUE
      }else if(fct == "glmmTMB") {
        convergence <- fit$sdr$pdHess
      }
      
      if(convergence == TRUE){
      
        vcov_m <- broom.mixed::tidy(fit)                       %>%
                    filter(effect == "ran_pars")               %>%
                    mutate(name     = paste0(group, "_",term),
                           estimate = estimate^2)              %>%
                    filter(name %in% term_list)                %>%
                    select(name, estimate)                     %>%
                    as.data.table()
    
        # get estimated variance values from the model fit
        m <- lapply(1:nrow(vcov_m), function(i){
          res[[vcov_m[i , name]]] <<- vcov_m[i , estimate]
        })
        
        res[["state"]] <- "ok"
        
      }else{
        res[["state"]] <- "not converged"
      }
      
    }else{
      res[["state"]] <- "error"
    }
                      
    return(res)
  }
  
  out <- dat                          %>%
          group_by(Sim_id, Replicate) %>%
          do(res = fit_model(.))
          
  out <- bind_rows(out[["res"]])      %>% 
          mutate(Model = mlabel)
    
  return(out)
}