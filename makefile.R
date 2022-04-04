tictoc::tic()

rm(list=ls())

source("simulate_data.R")

rm(list=ls())

source("fit_model.R")

rm(list=ls())

rmarkdown::render("display_results.Rmd")
browseURL("display_results.html")

rm(list=ls())

tictoc::toc()