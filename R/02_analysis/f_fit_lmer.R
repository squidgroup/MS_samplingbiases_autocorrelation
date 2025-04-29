f_fit_lmer <- function(dat, mlabel, fformula){
  
  # dat      <- rbind(tar_read(s_sim_TRUE), tar_read(s_sim_FALSE))
  # mlabel   <- "time_ind"
  # fformula <- "Phenotype ~ 1 + (1|Individual) + (1|Time) + (1|Time:Individual)"
  
  ####

  library(lme4)

  fit_model <- function(dt){
    
    # dt <- dat[Sim_id == unique(Sim_id)[1] & Replicate == 1]

    fit  <- tryCatch(lme4::lmer(as.formula(fformula), data = dt),
            error = function(e){NULL})
    
    res <- data.frame("Sim_id"    = dt$Sim_id[1],
                      "Replicate" = dt$Replicate[1])
    
    if(!is.null(fit)){
      
      vcov_m <- broom.mixed::tidy(fit)                  %>%
        filter(effect == "ran_pars")                    %>%
        mutate(term = gsub("factor|[.()[:digit:]]", '', 
                           paste0(group, "_",term)))    %>%
        distinct(term, .keep_all=TRUE)                  %>%
        select(term, estimate)                          %>%
        as.data.table()
      
      # get estimated variance values from the model fit
      m <- lapply(1:nrow(vcov_m), function(i){
        res[[vcov_m[i , term]]] <<- vcov_m[i , estimate]
      })
      
      res[["state"]] <- "ok"
      
    }else{
      res[["state"]] <- "error"
    }
                      
    return(res)
  }
  
  out <- dat                          %>%
          group_by(Sim_id, Replicate) %>%
          do(res = fit_model(.))
          
  out <- bind_rows(out[["res"]])      %>% 
          mutate(Model = mlabel)      %>%
          as.data.table()
    
  return(out)
}