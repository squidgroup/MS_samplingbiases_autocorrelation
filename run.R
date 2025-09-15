source("R/packages/packages.R") # loads packages
source("R/functions.R") # loads functions

library("targets")
library("tarchetypes")

### Run targets

# tar_make()
tar_make_future(workers = 6)
