filter_grts_mh_by_address <- function(
    addresses,
    spatrast = grts_mh_n2khab,
    spatrast_index = grts_mh_n2khab_index,
    cells = NULL,
    drop_address = FALSE) {
  if (is.null(cells)) {
    cells <- subset(spatrast_index, grts_address %in% addresses)$id
  }
  r <- spatrast[cells, drop = FALSE]
  if (drop_address) {
    ifel(!is.na(r), TRUE, r)
  } else r
}
