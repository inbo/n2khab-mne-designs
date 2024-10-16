#' Return the \code{watercourses} data source as an \code{sf} object
#'
#' https://zenodo.org/records/4420905
#' https://doi.org/10.5281/zenodo.4420905
#' Returns the raw data source \code{watercourses}, with unique polygon
#' identifier \code{polygon_id}.
#' Multiple polygons can have the same \code{region_name}.
#' The coordinate reference system is 'BD72 / Belgian Lambert 72'
#' (EPSG-code \href{https://epsg.io/31370}{31370}).
#'
#' Original columns of the raw data source were mapped as:
#' \itemize{
#' \item \code{OIDN} -> \code{polygon_id}
#' \item \code{UIDN} -> \code{manifestation_id}
#' \item \code{VHAG} -> \code{region_id}
#' \item \code{NAAM} -> \code{name}
#' \item \code{NAMEN} -> \code{name2}
#' \item \code{LENGTE} -> \code{length [m]}
#
#' }
#'
#' Apart from the label, there is no complementary information between
#' \code{polygon_code} and \code{polygon_id}.
#'
#' @inheritParams read_habitatmap_stdized
#'
#' @return
#' A Simple feature collection of geometry type \code{MULTIPOLYGON}.
#'
#' @examples
#' \dontrun{
#' watercourses <- read_watercourses()
#' watercourses
#' }
#'
#' @importFrom sf
#' read_sf
#' st_drop_geometry
#' @importFrom dplyr
#' %>%
#' select
#' mutate
#' arrange
#' @importFrom rlang .data
#' @export
read_watercourses <- function(
  file = NULL
  ) {
  stopifnot(n2khab = require("n2khab"))
  stopifnot(dplyr = require("dplyr"))
  stopifnot(sf = require("sf"))
  stopifnot(units = require("units"))

  if(is.null(file)) {
    file <- file.path(locate_n2khab_data(), "10_raw/watercourses")
  }

  suppressWarnings(
    watercourses <- read_sf(file, crs = st_crs(31370))
    # WARNING: st_crs <- replacing crs does not reproject data; use `st_transform` for that.
  )

  # documentation VHA:
  # https://www.vlaanderen.be/digitaal-vlaanderen/onze-diensten-en-platformen/basiskaart-vlaanderen-grb/objectenhandboek-basiskaart-vlaanderen-grb/vha-waterloopsegment-wlas
  watercourses <-
    watercourses %>%
    select(
      polygon_id = OIDN,
      rank = UIDN,
      vhag_code = VHAG,
      name = NAAM,
      name2 = NAMEN,
      length = LENGTE
    ) %>%
    mutate_at(vars(vhag_code, rank, name, name2), as.factor) %>%
    mutate_at(vars(length), function (col) set_units(col, m)) %>%
    arrange(vhag_code, rank, polygon_id)

  ## units

  return(watercourses)
}



#' Load spatial water info from multiple sources.
#'
#' Load the following sources:
#' - n2khab/read_watersurfaces()
#' - n2khab/read_habitatstreams()
#' - n2khab/read_watercourse_100mseg
#' All data in Belgian Lambert 72 (EPSG-code 31370) crs.
#'
#' @return a list of spatial water data sets.
#'
load_all_water_sources <- function( ) {

  # required packages
  stopifnot(n2khab = require("n2khab"))

  # all the water we have
  watersurf_raw <- read_watersurfaces()
  waterstreams_raw <- read_habitatstreams() # 3260
  # read_watercouses raw dataset
  watercourses_raw <- read_watercourses()
  # watercourses_raw <- read_watercourse_100mseg(element = "lines")

  # combine water data in a list
  all_wata <- list(
    "surfaces" = watersurf_raw,
    "streams" = waterstreams_raw,
    "courses" = watercourses_raw
  )

  # ... and return it
  return(all_wata)
} # /load_all_water_sources



#' Narrow a list of sources to an area within a radius.
#'
#' For each data source available, return only those observations
#' which are within a given distance (`radius`)
#' from a point (`reference_point`).
#' All data in Belgian Lambert 72 (EPSG-code 31370) crs.
#'
#' @param data_sources the full water data
#' @param reference_point a point `c(x,y)` which is
#'        the center of our analysis world
#' @param radius the focus area radius, in meters
#'
#' @return a list of (shortened) spatial water data sets.
#
#' @examples
#' \dontrun{
#' narrow_sources_radius(
#'   load_all_water_sources,
#'   c(148600, 208900),
#'   radius = 1000
#' )
#' }
#'
narrow_sources_radius <- function(data_sources, reference_point, radius) {

  # select only water bodies within a radius
  narrowed_collection <- lapply(
    data_sources,
    function (raw_spatial) geometry_within_radius(
        reference_point,
        radius = radius,
        raw_spatial
    )
  )

  return(narrowed_collection)
} # /narrow_sources_radius



#' Get distances to water from multiple data sources.
#'
#' For each data source available, compute de cross distance
#' with the focus points (e.g. observation wells).
#' All data in Belgian Lambert 72 (EPSG-code 31370) crs.
#'
#' @param points sf::Points
#' @param data_sources the water data to which distance is computed
#'
#' @return A data frame with distances of water (rows) to points (cols),
#'         in meters.
#'
collect_combined_distances <- function(points, data_sources) {

  # initiate empty distance collection
  distance_collection <- NULL

  # loop water data sources
  for (idx in seq_along(data_sources)) {

    # extract data
    wata <- data_sources[[idx]]
    if (nrow(wata) == 0) {
      # there might be none of this kind of wata in the area
      next
    }

    # compute distance
    dist_matrix <- as.data.frame(st_distance(wata, points))
    # if (nrow(dist_matrix) == 0) {
    #   # this should be handled by the above
    #   next
    # }

    # point id's as column names
    colnames(dist_matrix) <- points$id

    # append information
    dist_matrix$source <- names(data_sources)[[idx]] # water data source
    dist_matrix$idx <- rownames(wata) # index

    # append distance collection
    if (is.null(distance_collection)) {
      distance_collection <- dist_matrix
    } else {
      distance_collection <- rbind(distance_collection, dist_matrix)
    }

  } # /loop sources
  return(distance_collection)
} # /collect_combined_distances



#' Compute the minimum of cross distances.
#'
#' For a matrix of cross distances as it is computed by
#' the `collect_combined_distances` function above.
#'
#' @param cross_distances a data frame of cross distances,
#'        which also contains the data source and index.
#'
#' @return A data frame with:
#'         - minimum distance to water
#'         - reference to the water (source, index)
#'
minimum_cross_distance <- function(cross_distances) {

  # extract distance matrix
  distances_matrix <- cross_distances %>%
    select(-source, -idx)

  # find index of the minimum water distance
  min_index <- distances_matrix %>%
    summarize_all(which.min)

  # info to extract for each observation
  get_mins <- function(idx) {
    row <- min_index[[idx]]
    list(
      "min_dist" = cross_distances[row, idx],
      "min_src" = cross_distances[row, "source"],
      "min_idx" = as.integer(cross_distances[row, "idx"])
    )
  }


  # extract min info for each observation, and tidy
  closest_distances <- sapply(colnames(min_index), get_mins) %>%
    t %>%
    as_tibble %>%
    tidyr::unnest(cols = colnames(.))

  # store index
  closest_distances$idx <- colnames(min_index)

  # return joinable data frame
  return(closest_distances %>% relocate(idx))
} # /minimum_cross_distance
