library(dplyr)
library(mgcv)
library(itsadug)
library(data.table)
library(ggplot2)


########################################
# Simulate data

N    <- 1000

AR   <- 0.8
Ve   <- 1
VeAR <- Ve * (1/sqrt(1 / (1 - AR^2)))^2
e    <- arima.sim(model=list(ar=AR),n=N, sd =sqrt(VeAR), n.start = 200)
# e  <- rnorm(N,0,0.05)

# AR <- 0.8
# Vx <- 1 * (1/sqrt(1 / (1 - AR^2)))^2
# x  <- arima.sim(model=list(ar=AR),n=N, sd =sqrt(Vx), n.start = 200)
# x  <- rnorm(N,0,1)
x <- rep(0, N)

time  <- 1:N
y     <- 0.1*x + e

dt <- data.frame(y,x, time, "Subject" = 1)

dt <- start_event(dt, column="time", event=c("Subject"), label="event")
head(dt)

########################################
# fit models

m0 <- bam(y ~ time, data=dt)
acf_resid(m0)
r1 <- start_value_rho(m0, plot=TRUE)


m1 <- bam(y ~ time, data=dt, rho=r1, AR.start=dt$event)
acf_resid(m1)



