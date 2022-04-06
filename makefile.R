source("R/packages/packages.R") # loads packages
source("R/functions.R") # loads functions

library(targets)
library(tarchetypes)

### Run targets

# tar_watch(seconds = 10, outdated = FALSE, targets_only = TRUE)

tar_make()
tar_meta(fields = error, complete_only = TRUE)

### Check targets

# tar_manifest()
# tar_glimpse()
# tar_visnetwork()
# tar_outdated()


### Progress and report 

# tar_progress()
# tar_meta()

# retrieve errors and warnings
# tar_meta(fields = error, complete_only = TRUE)
# tar_meta(fields = warnings, complete_only = TRUE)


### Clean up local internal files 

# tar_prune()
# tar_destroy()