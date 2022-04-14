library(glmmTMB)

data <- tar_read(s_sim_FALSE)
par  <- tar_read(s_param_FALSE)

id  <- par[Vhsi==0.8 & VI==0.4 & X1_sto_corr==0.8 & X1_lin_state==FALSE & X1_cyc_state==FALSE, Sim_id]

dat <- data[Sim_id == id & Replicate==1]

# dat[ , Time := factor(Time)]


ggplot(data=dat, aes(x=Time, y=Individual)) + geom_point()


(fit.ar1 <- glmmTMB(Phenotype ~ (1|Individual) + ar1(factor(Time) + 0 | Individual), data=dat))


test <- broom.mixed::tidy(fit.ar1) 
