f_fit_glmmTMB <- function(dat, mlabel, fformula){
  
  # dat      <- tar_read(s_sim_FALSE)
  # mlabel   <- "ar1_ind"
  # fformula <- "Phenotype ~ 1 + (1|Individual) + ar1(factor(Time)+0|Replicate)"

  ####
  
  library(glmmTMB)

  # dat <- dat[Sim_id == "45_NR5_sharedFALSE" & Replicate %in% 7:10]
  
  fit_model <- function(dt){
    
    # dt <- dat[Sim_id == "42_NR10_sharedFALSE" & Replicate == 1]
    
    fit  <- tryCatch(glmmTMB::glmmTMB(as.formula(fformula), data = dt),
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
      
      # check convergence
      convergence <- fit$sdr$pdHess
      
      if(convergence == TRUE){
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
          mutate(Model = mlabel)      %>%
          as.data.table()
    
  return(out)
}