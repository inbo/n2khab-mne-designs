#----------------
#--- SPATIAL ----
#----------------


#' Filter points within a radius
#'
#' Selects all points from a collection of points which
#' are within a given radius from a center.
#'
#' @param center a point of XY coordinates
#' in Belgian Lambert 72 (EPSG-code 31370) crs.
#' If this is given as a simple XY vector, conversion
#' to an `st_point` is attempted.
#' @param radius the filter radius, in meters
#' @param points a collection of `sf` points
#'
#' @return a subset of the given points.
#'
#' @examples
#' \dontrun{
#' points_within_radius(
#'   c(148600, 208900),
#'   100,
#'   watina::as_points(obswells_db %>% collect())
#' )
#' }
#'
points_within_radius <- function(center, radius, points) {

  # make sure `sf` is loaded
  stopifnot(sf = require("sf"))
  stopifnot(watina = require("watina"))

  # ensure center is a point
  if (!inherits(center, "POINT")) {
    tryCatch({
      center <- st_point(x = center, dim = "XY")
    }, error = function(e) {
      message("could not convert center to POINT:")
      stop(e)
    })
  }

  # assert data types of the other arguments
  assert_that(inherits(points, "sf"),
    msg = "The `points` must be an `sf` object.")

  assert_that(is.numeric(radius) && radius >= 0,
    msg = "radius must be numeric and greater than zero.")

  # create a buffer around the center point
  buf <- st_buffer(center, dist = radius)

  # compute the intersect of points and buffer
  intersect <- st_intersects(buf, points)[[1]]

  # return the matched points
  return(points[intersect, ])

} # /points_within_radius



#-------------------------
#--- Terrain Features ----
#-------------------------

#' Get DHMV WCS Elevation Data (DTM, 1m)
#'
#' The function extracts elevation data for the special purpose of
#' analyzing spatiotemporal coupling (i.e. with rather fixed parameters).
#' In the background, this sends a query to the DHMV WCS service,
#' downloads it to a temporary file from which it is read with `terra::rast()`
#' and returned as a `SpatRaster`object
#' to extract elevations.
#'
#' @param point_sf coordinates of the point(s) to query; crs: Belgian Lambert 72.
#' @param margin safety margin around the point, in meters
#' @return elevation of points
#'
#' @details In the future, this will connect to inbospatial::get_coverage_wcs
#' See [metadata Vlaanderen](https://github.com/inbo/inbospatial/pull/12)
#'
#' @importFrom sf st_as_sf st_transform st_coordinates
#' @importFrom terra rast `res<-` project
#' @importFrom assertthat assert_that
#' @importFrom httr parse_url build_url GET write_disk stop_for_status
#' @importFrom stringr str_extract str_replace
#'
#' @examples
#' \dontrun{
#' query_elevation_wcs(
#'   point_sf = sf_points,
#'   margin = 1
#' )
#' }
#'
query_elevation_wcs <- function(point_sf, margin = 1) {

  # package requirements
  stopifnot(
    assertthat = require("assertthat", quietly = TRUE),
    httr = require("httr", quietly = TRUE),
    sf = require("sf", quietly = TRUE),
    terra = require("terra", quietly = TRUE)
  )

  # data type assertions
  assert_that(inherits(point_sf, "sf"))
  assert_that(is.numeric(margin) && margin > 0)


  # target area
  xy <- st_coordinates(point_sf)

  xmin <- min(xy[,"X"]) - margin
  xmax <- max(xy[,"X"]) + margin
  ymin <- min(xy[,"Y"]) - margin
  ymax <- max(xy[,"Y"]) + margin

  # get bbox
  bbox <- paste(xmin, ymin, xmax, ymax, sep = ",")

  ## get coverage
  # in the future, this could be replaced by inbospatial::get_coverage_wcs
  # https://github.com/inbo/inbospatial/pull/12
  # switch will also require bbox adjustment
  #
  # most parameters are fixed or ignored
  get_coverage_wcs <- function(
    wcs = "DHMV",
    bbox,
    layername,
    resolution = 1,
    wcs_crs = "EPSG:31370",
    output_crs = "EPSG:31370",
    bbox_crs = "EPSG:31370",
    version = "1.0.0",
    ...) {

    # the URL components
    base_url = "https://geo.api.vlaanderen.be"
    endpoint = "/DHMV/wcs"
    message(paste0("connecting to ", wcs))

    # the query parameters which worked in QGIS
    elevation_query = list(
      service = "WCS",
      version = "1.0.0",
      request = "GetCoverage",
      format = "GeoTIFF",
      coverage = layername,
      bbox = bbox,
      resx = resolution,
      resy = resolution,
      crs = "EPSG:31370"
    )

    # get wcs data
    file = tempfile(fileext = ".tif")
    http_response = GET(
      url = modify_url(base_url, path = endpoint),
      query = elevation_query,
      write_disk(file)
    )
    # note: saving this to a file is optional,
    # but might prevent double download or loss of data

    stop_for_status(http_response)
    message("elevation query succesful!")

    # re-read raster file
    data = rast(file)
    return(data)
  }

  data <- get_coverage_wcs(
    wcs = "DHMV",
    bbox = bbox,
    layername = "DHMVII_DTM_1m",
    resolution = 1
    )

  values <- extract(data, point_sf)
  names(values) <- c("id", "elevation")

  elevations <- as.data.frame(cbind(xy, values))
  elevations <- st_as_sf(
    elevations,
    coords = c("X", "Y"),
    crs = 31370
  )


  return(elevations)

} # /query_elevation_wcs



#-------------
#--- DHMV ----
#-------------
# Digitaal Hoogtemodel Vlaanderen (Elevation Map of Flanders)

#' OBSOLETE query elevation data from a local SQLite database
#'
#' DHMV point data must be stored in a local file
#' See/use `unzip_dhmv_points.sh` file.
#'
query_elevation_sqlite <- function(point_sf, conn = NULL, margin = 10) {

  # assert package availability
  stopifnot(assertthat = require("assertthat"),
            gstat = require("gstat"),
            dplyr = require("dplyr"),
            sf = require("sf"),
            DBI = require("DBI")
  )

  # assert data types of the arguments
  assert_that(inherits(point_sf, "sf"),
    msg = "The `points` must be an `sf` object.")
  assert_that(is.numeric(margin) && margin >= 0,
    msg = "The `margin` must be numeric and greater than zero.")

  # if required, open a temporary connection
  temp_conn <- FALSE
  if (is.null(conn)) {
    conn <- dbConnect(RSQLite::SQLite(), file.path("./dhmv_points.db"))
    temp_conn <- TRUE
  } else {
    assert_that(inherits(conn, "DBIConnection"),
      msg = "`conn` must be a DBI connection")
  }

  # target area
  xy <- st_coordinates(point_sf)

  xmin <- min(xy[,"X"]) - margin
  xmax <- max(xy[,"X"]) + margin
  ymin <- min(xy[,"Y"]) - margin
  ymax <- max(xy[,"Y"]) + margin

  # get raw elevations from sqlite
  query_string <- paste0(
  " SELECT x, y, h
    FROM points
    WHERE x BETWEEN ", xmin," AND ", xmax,
  "   AND y BETWEEN ", ymin," AND ", ymax,
  ";")

  area <- DBI::dbGetQuery(conn, query_string)

  if (FALSE) {
    elevations_raw <- tbl(conn, "points")

    # extract point coords
    print(length(point_sf))
    # query area around point
    area <- elevations_raw %>%
      filter(x >= xmin, x <= xmax,
             y >= ymin, y <= ymax
      ) %>% collect
  } # /inefficient method

  # error if no points found
  if (nrow(area)<1) {
    stop("Insufficient number of points found in the selected area.
        Try increasing the `margin` width.")
  }

  # convert to sf
  area_sf <- st_as_sf(area, coords = c("x", "y"), crs = 31370)

  # interpolate, inverse distance weighted
  model <- gstat(formula = h ~ 1, data = area_sf)

  # predict elevation at point
  elevation <- predict(model, point_sf)

  # close temporary sqlite connection
  if (temp_conn) {
    dbDisconnect(conn)
  }

  # return
  return(elevation)

} # /query_elevation_sqlite
