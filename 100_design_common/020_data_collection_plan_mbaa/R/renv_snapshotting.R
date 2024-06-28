# we don't use renv::activate() in order to only manage and use the renv library
# at specific stages. I.e. we avoid a .Rprofile file with the source() statement
# below. (First-time setup: run renv::activate() and then remove .Rprofile)

source("renv/activate.R") # this is 'activating renv on demand'
renv::upgrade() # makes sure latest renv version is in use
if (file.exists(".Rprofile")) unlink(".Rprofile")
renv::hydrate(update = "all") # populates or updates renv project library with
                              # the package versions used when renv is
                              # not active
renv::snapshot() # records packages with their versions in renv.lock
# renv::install("yaml") # links a missing package in the renv project library
# renv::record("yaml") # records a renv project library package in renv.lock

# then commit all changes
# (first-time setup: stage & commit the renv directory and the renv.lock file)
