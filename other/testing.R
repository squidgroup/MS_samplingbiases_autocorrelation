library(glmmTMB)

data <- tar_read(s_sim_FALSE)
par  <- tar_read(s_param_FALSE)

id  <- par[Vhsi==0.8 & VI==0.4 & X1_sto_corr==0.8 & X1_lin_state==FALSE & X1_cyc_state==FALSE, Sim_id]

dat <- data[Sim_id == id & Replicate==1]

# dat[ , Time := factor(Time)]


ggplot(data=dat, aes(x=Time, y=Individual)) + geom_point()


(fit.ar1 <- glmmTMB(Phenotype ~ (1|Individual) + ar1(factor(Time) + 0 | Individual), data=dat))


test <- broom.mixed::tidy(fit.ar1) 



#####################################################


# dat      <- rbind(tar_read(s_sim_TRUE), tar_read(s_sim_FALSE))
dat      <- tar_read(s_sim_FALSE)
mlabel   <- "ar1_ind"
fformula <- "Phenotype ~ 1 + (1|Individual) + ar1(factor(Time)+0|Individual)"
fct      <- "glmmTMB"

####

dat <- dat[Replicate == 1 & Sim_id %in% unique(Sim_id)[sample(length(unique(Sim_id)), 50)]]

if(fct == "lmer"){
  library(lme4)
}else if(fct == "glmmTMB") {
  library(glmmTMB)
}

a_ar1_ind <- as.data.table(a_ar1_ind)
a_ar1_ind[state == "error"]

fit_model <- function(dt){
  
  dt <- dat[Sim_id == "1_NR10_sharedFALSE" & Replicate == 59]
  
  fit  <- tryCatch(
    if(fct == "lmer"){
      lmer(as.formula(fformula), data = dt)
    }else if(fct == "glmmTMB") {
      glmmTMB(as.formula(fformula), data = dt)
    },
    error = function(e){
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
                  mutate(term = gsub("factor|[.()[:digit:]]", '', 
                                     paste0(group, "_",term))) %>%
                  distinct(term, .keep_all=TRUE)             %>%
                  select(term, estimate)                     %>%
                  as.data.table()
      
      # get estimated variance values from the model fit
      m <- lapply(1:nrow(vcov_m), function(i){
        res[[vcov_m[i , term]]] <<- vcov_m[i , estimate]
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
