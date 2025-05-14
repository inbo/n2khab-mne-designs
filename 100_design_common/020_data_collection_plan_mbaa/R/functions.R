figdim <- function(x) {
  if (opts_knit$get("rmarkdown.pandoc.to") == "html") {
    x
  } else {
    x * 0.75
  }
}

our_update_theme_bars <- function() {
  theme(
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = col_gridline_bw),
    panel.grid.minor.y = element_line(colour = col_gridline_bw),
    axis.ticks.x = element_blank()
  )
}
our_update_theme_maps <- function() {
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
}
our_update_theme_facets <- function() {
  theme(
    strip.background = element_rect(
      fill = inbocol_2_light,
      colour = inbocol_2_light
    ),
    strip.text = element_text(colour = inbocol_2_contrast)
  )
}
our_update_theme_maps_in_facets <- function() {
  theme(
    panel.border = element_rect(colour = col_gridline_bw, fill = NA),
    panel.background = element_blank()
  )
}
our_update_theme_revisitdiag_horfacets <- function() {
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA),
    panel.background = element_blank(),
    strip.text.y = element_text(angle = 0),
    axis.ticks = element_blank()
  )
}
our_update_theme_bars_in_horfacets <- function() {
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = col_gridline_bw),
    panel.grid.minor.x = element_line(colour = col_gridline_bw),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_rect(colour = col_gridline_bw, fill = NA),
    strip.text.y = element_text(angle = 0),
    legend.position = "top"
  )
}

kbl_bt <- function(x, ...) kbl(x = x, booktabs = TRUE, ...)

longtable_styling <- function(kable_input) {
  kable_styling(
    kable_input = kable_input,
    latex_options = "repeat_header",
    repeat_header_text = "\\textit{(vervolg)}"
  )
}

our_column_spec <- function(kable_input, column, width, ...) {
  res <- kable_input
  stopifnot(identical(length(column), length(width)))
  for (i in seq_along(column)) {
    res <- column_spec(res, column = column[i], width = width[i], ...)
  }
  res
}

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
    r[!is.na(r)] <- 1
  }
  r
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
