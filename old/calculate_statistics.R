library(dplyr)

# read data
fit_data   <- readRDS(file = "C:/Users/hasse/Documents/MS3_squid_data/fit_model_data/MS3_fit_model_data.rds")
parameters <- read.csv("C:/Users/hasse/Documents/MS3_squid_data/simulated_data/MS3_parameters.csv")

# statistic functions
rmse      <- function(actual,estimated){sqrt(mean((actual - estimated)^2))}
precision <-function(x){
  precision <- abs(quantile(x,probs=.25)-quantile(x,probs=.75))
  names(precision) <- NULL
  precision
}

# function that calculates the value of each statistics
calculate_statistics <- function(dt, parameters){
  
  # save simulation's id
  parameters <- parameters[parameters$Sim_id == dt$Sim_id[1], ]
  
  return(data.frame("Sim_id"     = dt$Sim_id[1],
                    "variable"   = c("VI", "Ve", "Rep"),
                    "rmse"       = c(rmse(parameters$VI, dt$VI), rmse(parameters$Ve, dt$Ve), rmse(parameters$Rep, dt$Rep)),
                    "precis"     = c(precision(dt$VI), precision(dt$Ve), precision(dt$Rep))))
}

dt_statistics     <- fit_data               %>%
                      group_by(Sim_id)      %>%
                      do(calculate_statistics(., parameters))

fwrite(dt_statistics, file = "C:/Users/hasse/Documents/MS3_squid_data/statistics_data/MS3_statistics_data.csv")