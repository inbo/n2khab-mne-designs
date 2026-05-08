# we don't use renv::activate() in order to only manage and use the renv library
# at specific stages. I.e. we avoid a .Rprofile file with the source() statement
# below. (First-time setup: run renv::activate() and then remove .Rprofile)

# source("renv/activate.R") # this is 'activating renv on demand'
# if asked 'Would you like to restore the project library?', answer N

# Above line has been outcommented since it appears that hydrating &
# snapshotting can now be done based on current library paths, without
# activating the project. When asked, choose accordingly.

renv::upgrade() # makes sure latest renv version is in use
# populate or update renv project library with the package versions used
renv::hydrate(update = "all")
# renv::hydrate("yaml", update = "all") # links a missing package in the renv project library
renv::snapshot() # records packages with their versions in renv.lock
# renv::record("yaml") # records a renv project library package in renv.lock
if (file.exists(".Rprofile")) unlink(".Rprofile") # inactivate renv

# then commit all changes
# (first-time setup: stage & commit the renv directory and the renv.lock file)
