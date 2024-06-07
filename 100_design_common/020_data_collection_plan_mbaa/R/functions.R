figdim <- function(x) {
  if (opts_knit$get("rmarkdown.pandoc.to") == "html") {
    x
  } else {
    x * 0.75
  }
}

our_update_theme_bars <- function() theme(
  panel.background = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(colour = col_gridline_bw),
  panel.grid.minor.y = element_line(colour = col_gridline_bw),
  axis.ticks.x = element_blank()
)
our_update_theme_maps <- function() theme(
  panel.grid = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank()
)
our_update_theme_facets <- function() theme(
  strip.background = element_rect(
    fill = inbocol_2_light,
    colour = inbocol_2_light
  ),
  strip.text = element_text(colour = inbocol_2_contrast)
)
our_update_theme_maps_in_facets <- function() theme(
  panel.border = element_rect(colour = col_gridline_bw, fill = NA),
  panel.background = element_blank()
)
our_update_theme_revisitdiag_horfacets <- function() theme(
  panel.grid = element_blank(),
  panel.border = element_rect(fill = NA),
  panel.background = element_blank(),
  strip.text.y = element_text(angle = 0),
  axis.ticks = element_blank()
)

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
  } else {
    r
  }
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
      tibble(grts_address = addresses, x = coords[, "x"], y = coords[, "y"]),
      join_by(grts_address)
    ) %>%
    {
      if (isFALSE(spatial)) {
        .
      } else {
        st_as_sf(., coords = c("x", "y"), crs = crs(spatrast))
      }
    }
}
