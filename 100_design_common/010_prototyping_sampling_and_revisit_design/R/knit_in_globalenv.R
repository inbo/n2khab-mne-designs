# This script allows to execute (selected) Rmd files in the interactive R
# session without the need to run them manually in the IDE

central_files <- list.files(".", "^[0].+Rmd$")
as.matrix(central_files)

# set this vector as needed:
files_to_run <- central_files[-10]

# first run index.Rmd manually, as knitr::knit("index.Rmd") doesn't process
# YAML and stops when reading params

# Then, overwrite elements of params as needed (beware of: active_modules)

# Then run:

for (file in files_to_run) {
  knitr::knit(file)
}

