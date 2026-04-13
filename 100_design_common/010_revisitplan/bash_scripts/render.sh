#! /bin/bash

# To render the bookdown project, just run below command in the
# '010_revisitplan' directory:

Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::html_document2", \
    params = list(regenerate_binary_files = FALSE, save_rdata = TRUE))'

# It will give rise to a single HTML file in this directory. If it already
# existed, it will be overwritten.

# Note that 'save_rdata = TRUE' can overwrite an existing RData file in the
# '010_revisitplan/data/binary/results' directory.

# 'regenerate_binary_files = FALSE' is the default, but if new or updated
# (sub)modules or versions thereoff are implemented, then it must be set to
# TRUE in order to update some (git-ignored) binary files. These files are
# always created when they are missing, regardless of the parameter setting.

# To also reproduce package versions with renv, add the extra list element
# 'reproduce_r_package_versions = TRUE' in the above 'params' argument; its
# default is FALSE.
