# Comparing the contents of two supposedly identical RData files

# First run setup chunk
#
# Then run the code to create rdata_set (see R/save_rdata.R)

rdata_path_new <- file.path(
  datapath,
  "binary/results",
  "objects_panflpan5.RData"
)

rdata_path_old <- file.path(
  datapath,
  "binary/results",
  "objects_panflpan5_rep_0.15.0.RData"
)

load(rdata_path_new)
oldenv <- new.env()
load(rdata_path_old, envir = oldenv)
ls(envir = oldenv)

compare_old_new <- function(objname) {
  isTRUE(
    all.equal(
      get(objname, envir = oldenv),
      get(objname),
      check.attributes = FALSE
    )
  )
}

for (i in rdata_set) {
  if (!compare_old_new(i)) {
    msg <- glue::glue("Difference detected for {i}")
    warning(msg)
  }
}

