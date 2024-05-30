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

add_point_coords_grts <- function(
    df,
    grts_var = "grts_address",
    spatrast = grts_mh_n2khab,
    spatrast_index = grts_mh_n2khab_index,
    spatial = TRUE) {

  addresses <- df %>%
    distinct(.data[[grts_var]]) %>%
    pull(.data[[grts_var]]) %>%
    sort()

  grts_cells <- spatrast_index %>%
    filter(grts_address %in% addresses) %>%
    arrange(grts_address) %>%
    pull(id)

  coords <- terra::xyFromCell(spatrast, grts_cells)

  df %>%
    left_join(
      tibble(grts_address = addresses, x = coords[,"x"], y = coords[,"y"]),
      join_by(grts_address)
    ) %>%
    {if (isFALSE(spatial)) . else {
      st_as_sf(., coords = c("x", "y"), crs = crs(spatrast))
    }}
}
