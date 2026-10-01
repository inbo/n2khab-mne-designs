# We only temporarily activate renv (renv::activate()), just to manage and use the renv library
# at specific stages. I.e. we avoid a .Rprofile file with the source() statement
# below.

renv::activate() # creates .Rprofile that sources renv script
renv::upgrade() # makes sure latest renv version is in use
# populate or update renv project library with the package versions currently used outside renv
renv::hydrate(update = "all")
# renv::hydrate("yaml", update = "all") # links a missing package in the renv project library
renv::snapshot() # records packages with their versions in renv.lock
# renv::record("yaml") # records a renv project library package in renv.lock
renv::deactivate() # removes the infrastructure added by activate()
# if (file.exists(".Rprofile")) unlink(".Rprofile") # inactivate renv 'the hard way',
#                                                    but make sure you didn't have other
#                                                    statements in .Rprofile

# then commit all changes
# (first-time setup: stage & commit the renv directory and the renv.lock file)
