#!/usr/bin/Rscript


# This script is a collection of queries of information about spatial locations.
# All the `query_*`-functions below require a data frame with columns
# `[idx, x, y]`. They return a data frame with the index (`idx`) and extra info.
# Coordinates are expected to be in the `BD72 / Belgian Lambert 72` reference
# system (https://epsg.org/crs_31370/BD72-Belgian-Lambert-72.html)

# Table of Content:
#  - example data
#  - general helpers
#  - queries:
#    - clusters
#    - dhmv elevation
#    - soilclass
#    - distance from water body
#  - testing


#_______________________________________________________________________________
# example data
#_______________________________________________________________________________


#' Provide example data for testing the functions below.
#'
#' Test data consists of data frame with an index column (derived from
#' the `PeilpuntWID` in watina) and x/y coordinates in BD72 CRS.
#' Well locations are arbitrarily chosen from some peer locations around the
#' country.
#'
#' @return data frame with test data
#'
#' @examples
#' \dontrun{
#'   test_data <- get_example_data()
#' }
#'
#' @export
#'
get_example_data <- function() {

  # peilpunten %>% filter(region == "KAL") %>% select(PeilpuntWID, x, y)

  test_data <- read.table(text = "
    2567, 184915, 179471
    8329, 182965, 169191
    7611, 169642, 167381
    12222,  22835, 198080
    4025, 203178, 170295
    15255, 154265, 234791
    14295, 148608, 208916
    12542, 215044, 197330",
    sep = ",")
  colnames(test_data) <- c("idx", "x", "y")
  return(test_data)
}



#_______________________________________________________________________________
# general helpers
#_______________________________________________________________________________

#' Common assertions about the data, and the index and coordinate columns
#'
#' Those assertions are:
#'   - `data` is a data frame
#'   - `index_column` is a string
#'   - `index_column` is a column in `data`
#'   - `coordinate_columns` are a column in `data`
#'
#' @keywords internal
#'
check_common_assertions <- function (
    data, index_column, coordinate_columns,
    skip_coordinates = FALSE
    ) {

  stopifnot(assertthat = require('assertthat'))

  # data type
  assertthat::assert_that(
    inherits(data, "data.frame"),
    msg = paste0("Input data must be a data.frame-like object.")
  )

  # index column
  assertthat::assert_that(is.character(index_column),
    msg = paste0("The `index_column` must be of type `character`.")
  )

  assertthat::assert_that(
    index_column %in% colnames(data),
    msg = paste0(
      "The index column `", index_column,
      "` is not in the data columns: ",
      paste(colnames(data), collapse = ",")
    )
  )

  # optionally skip coordinate columns
  if (skip_coordinates) {
    return(invisible(NULL))
  }

  # coordinate columns
  assertthat::assert_that(
    length(coordinate_columns) == 2,
    msg = paste0(
      "In this package, spatial data is two-dimensional ",
      "for all practical purposes. ",
      'Please provide two `coordinate_columns`, e.g. `c("x", "y")`.'
    )
  )


  for (col in coordinate_columns) {
    assertthat::assert_that(
      col %in% colnames(data),
      msg = paste0(
        "The coordinate column `", col,
        "` is not in the data columns: ",
        paste(colnames(data), collapse = ",")
      )
    )
  }


}


#' A generic function to join extra information to data.
#'
#' Join the `additional_data` to `data` based on an `index_column`.
#' By default, matching columns will be deleted from the `data`.
#'
#' @param data the data, in data frame format
#' @param additional_data the new data to be appended, also in data frame format
#' @param index_column the column holding a row identifier, e.g. `idx`
#' @param delete_existing boolean to enable prior removal of matching columns.
#'
#' @return the data, joined by the additional_data
#'
#' @examples
#' \dontrun{
#'    join_lookup(
#'      test_data,
#'      query_clusters(test_data),
#'      delete_existing = TRUE
#'      )
#' }
#'
#' @keywords internal
#'
join_lookup <- function(
    data,
    additional_data,
    index_column = "idx",
    delete_existing = TRUE
  ) {

  # type_dataidx <- typeof(data[, index_column])
  # type_lookupidx <- typeof(additional_data[, index_column])

  # if (!(type_dataidx == type_lookupidx)) {
  #   message(paste0("data (", type_dataidx,
  #       ") and additional_data (", type_lookupidx,") are not of the same type. ",
  #       "attempting to convert additional_data index."))
  # data[, index_column]
  # }

  # this circumvents occasional type mismatch of <integer> and <tbl_df:integer>
  data <- as.data.frame(data)
  additional_data <- as.data.frame(additional_data)

  # data <- data[!is.na(data[, index_column]), ]
  # additional_data <- additional_data[!is.na(additional_data[, index_column]), ]

  data[, index_column] <- type.convert(data[, index_column], as.is = TRUE)
  additional_data[, index_column] <- type.convert(additional_data[, index_column], as.is = TRUE)


  if (delete_existing) {
    # remove overlapping columns before join
    for (col in colnames(additional_data)) {
      if (!(col == index_column) && (col %in% colnames(data))) {
        data <- data %>%
          dplyr::select(-dplyr::one_of(col))
      }
    }

    # join, after deleting existing cols
    data <- data %>%
      dplyr::left_join(
        additional_data,
        by = index_column,
        relationship = "many-to-many"
      )
  } else {
    # solve column overlap by a suffix
    data <- data %>%
      dplyr::left_join(
        additional_data,
        by = index_column,
        relationship = "many-to-many",
        suffix = c("", "_")
      )
  }


  return(data)
}


#' A wrapper to enable piped join of extra info.
#'
#' Modifies a `query_*` function to be directly joined to the data
#' by wrapping it in a `join_lookup` (see above).
#' For examples, see the application in this script.
#'
#' @param query_function a function that queries extra info for a given dataset
#'
#' @return join_function a function which directly joins the info to the data.
#'
#' @keywords internal
#'
wrap_query_to_join <- function (query_function) {
  join_function <- function(data, index_column = "idx", ...) {
    join_lookup(
      data,
      suppressMessages(query_function(data, index_column = index_column, ...)),
      index_column = index_column,
      delete_existing = TRUE
    )
  }

  return(join_function)
}


#_______________________________________________________________________________
# split-apply-cache-combine
#_______________________________________________________________________________

local_cache_folder <- "cache"

#' Splitting data by a categorical into equal-sized groups
#'
#' This function will slice/cut a categorical `column`
#' into `n_bins` subgroups based on occurrence count within the groups.
#'
#' @param .data a data.frame-like table which contains the categorical
#' @param count_column the categorical column which contains groups to count
#' @param n_bins an integer giving the number of bins to assign
#' @param show boolean to indicate whether to plot the cuts
#'
#' @return boolean to indicate whether data was stored.
#'
#' @keywords internal
#'
split_balanced <- function(.data, count_column, n_bins = 2, show = FALSE) {

  # define the bins as a sequence of their edges
  bins <- seq(0, nrow(.data), length.out = n_bins)

  # cumulated count of elements in the group
  cumn <- .data %>%
    pull(!!enquo(count_column)) %>%
    tapply(., ., length) %>%
    cumsum()

  # find the first occurrence of elements above all bin edges
  firsts <- unique(apply(
    outer(cumn, bins,
      FUN = function(X,Y) X>Y
      ), 2,
    function (x) suppressWarnings(min(which(x)))
  )-1) # (the last one will be `Inf`)

  # optionally plot
  if (show) {
    ggplot(NULL) +
      geom_line(aes(seq(1, length(cumn)), cumn)) +
      geom_vline(xintercept = firsts) +
      geom_hline(yintercept = cumn[firsts])
  }

  # retrieve and label the groups
  grp <- .data %>%
    pull(!!enquo(count_column)) %>%
    as.numeric() %>%
    cut(firsts, labels = FALSE) %>%
    sprintf("%05.0f", .) %>%
    paste0(count_column, "_grp", .)

  return(factor(grp))
}



#' A wrapper to enable de-serialized HDD storage of data blocks.
#'
#' Modifies a `query_*` function to save (chunks of) queried data
#' to disk after loading.
#' The function will skip existing data, unless you specify otherwise.
#'
#' @param query_function a function that queries extra info for a given dataset
#' @param store_filepath specification of the cache subfolder to store results
#' @param data the data on which new columns are joined
#' @param index_column the column holding a row identifier, e.g. `idx`
#' @param ... other parameters are passed to the query
#'
#' @return boolean to indicate whether data was stored.
#'
#' @keywords internal
#'
query_to_cache <- function (query_function, store_filepath,
      data, index_column = "idx", ...) {

  # skip cached files
  if (file.exists(store_filepath)) return(FALSE)

  stopifnot(
    arrow = require("arrow")
  )

  # query and join the data
  if (FALSE) {
    # debubbing helper
    additional_data <- query_function(
            data, index_column = index_column,
            # n_quantiles = 20,
            # db_conn = watina_dwh,
            progress = TRUE
    )
  }
  # print(additional_data)
  additional_data <- suppressMessages(query_function(
          data, index_column = index_column, ...))
  # print(additional_data)
  result <- join_lookup(
      data = data,
      additional_data,
      index_column = index_column,
      delete_existing = TRUE
    )

  # print(knitr::kable(head(result)))

  # store data to disk
  write_parquet(result, sink = store_filepath)

  return(TRUE)
}


#' combining all cached chunks to one data file.
#'
#' @param label subfolder name to the relevant data
#' @param output_file optional path to the combined results,
#'                    creating a standard filename if not provided
#'
#' @return path to the output file
#'
combine_subfolder_data <- function (label, output_file = NA) {

  stopifnot(
    arrow = require("arrow"),
    dplyr = require("dplyr")
  )

  # find all files
  storage_path <- here::here(local_cache_folder, label)
  all_files <- list.files(storage_path)
  all_data <- lapply(all_files,
    FUN = function(fi) read_parquet(here::here(local_cache_folder, label, fi))
    )

  # save to disk
  if (is.na(output_file)) {
    output_file <- here::here(local_cache_folder, paste0("_", label, ".parquet", collapse = ""))
  }
  write_parquet(dplyr::bind_rows(all_data), sink = output_file)

  return(output_file)
}


#' Query data with parallel processes and store to disk.
#'
#' @inherit query_to_cache
#' @param split_data the data, `split()` by category groups
#' @param label a label, serving as data path and filename
#' @param query_function the function to query extra data
#' @param sequential boolean do decide between serial and parallel execution
#'
#' @export
#'
parallel_query_to_cache <- function(
    split_data, label, query_function,
    sequential = FALSE, verbose = FALSE, ...
  ) {

  numCores <- parallel::detectCores() - 2
  registerDoParallel(numCores)

  query_step <- function(i) {
    system(paste0("mkdir -p '", here::here(local_cache_folder, label), "'"))
    cluster_group <- names(split_data)[[i]]
    subdata <- split_data[[i]]
    storage_path <- here::here(local_cache_folder, label, cluster_group)
    if (verbose && !file.exists(storage_path)) print(cluster_group)
    query_to_cache(
      query_function = query_function,
      store_filepath = storage_path,
      data = subdata,
      ...
      )
    # if (verbose) print(storage_path)
  }

  # parallel application of the query function
  # https://www.rdocumentation.org/packages/foreach/versions/1.5.2/topics/foreach
  if (sequential) {
    foreach(i = 1:length(split_data), .combine=rbind) %do% {
      query_step(i)
    } -> count_nonexisting
  } else {
    foreach(i = 1:length(split_data), .combine=rbind) %dopar% {
      query_step(i)
    } -> count_nonexisting
  }

  # combine downloaded data in case of changes
  output_file <- here::here(local_cache_folder, paste0("_", label, ".parquet", collapse = ""))
  if (!file.exists(output_file) || any(count_nonexisting)) {
    combine_subfolder_data(label, output_file = output_file)
  }
}



#' Load and/or join cached summary file.
#'
#' @param .data the original data to join, serving to identify data path and filename
#' @param label the label, serving to identify data path and filename
#' @param index_column the column holding a row identifier, e.g. `idx`
#'
#' @returns an (extended) data frame
#'
join_auxiliary_cache <- function(
    .data = NULL,
    label = c(
      "metadata",
      "elevation",
      "waterdistance",
      "empiricalmodes",
      "quantiles",
      "diffdata"
    ),
    index_column = "idx"
  ) {

  stopifnot(
    assertthat = require('assertthat'),
    dplyr = require('dplyr')
  )

  # if provided, data must be in a frame.
  if (!is.null(.data)) {
    assertthat::assert_that(
      inherits(.data, "data.frame"),
      msg = paste0("Input data must be a data.frame-like object.")
    )
  }

  # label
  assertthat::assert_that(is.character(label),
    msg = paste0("The `label` must be of type `character`.")
  )

  # index column
  assertthat::assert_that(is.character(index_column),
    msg = paste0("The `index_column` must be of type `character`.")
  )

  # load additional data
  fi <- paste0("_", label, ".parquet")
  additional_data <- read_parquet(here::here(local_cache_folder, fi))

  # return or append and return data
  if (is.null(.data)) return(additional_data)

  # skip duplicate columns
  print(colnames(.data))
  print(colnames(additional_data))
  additional_data <- additional_data %>%
    select(matches(index_column), !any_of(colnames(.data )))

  data_appended <- .data %>%
    dplyr::left_join(additional_data,
      by = index_column,
      relationship = "many-to-many" # quantiles and diffdata are not 1-1
    )

  return(data_appended)
}


#' Empty a cache subfolder of choice.
#'
#' @param subfolder the label, serving as data path and filename
#'
empty_cache_subfolder <- function(subfolder) {

  storage_path <- here::here(local_cache_folder, subfolder)

  if (interactive()) {
    confirm_delete <- utils::askYesNo(
      msg = paste0("Really delete everything?",
                   storage_path,
                   collapse = "\n"
                   ),
      default = FALSE,
    )
    if (confirm_delete) system(paste0("rm -rf ", storage_path))
  } else {
    # we suppose you know what you are doing if you call this function.
    system(paste0("rm -rf ", storage_path))
  }
}


#_______________________________________________________________________________
# clusters
#_______________________________________________________________________________

#' Group a set of locations in clusters.
#'
#' This will compute the cross distance of locations in a data set based on
#' `x, y` coordinates, and cluster the locations
#'  based on a `characteristic_distance`.
#'
#' @param data the data, in data frame format
#' @param index_column the column holding a row identifier, e.g. `idx`
#' @param coordinate_columns columns in which the coordinates are stored,
#'        e.g. `c(x, y)`
#' @param characteristic_distance distance (m) used to determine clusters
#'        (equivalent to tree cut height in `stats::cutree`).
#'
#' @return cluster_lookup a data frame with the index column and cluster nummer.
#'        the `cluster` column is converted to a factor
#'        with the clusters as levels.
#'
#' @examples
#' \dontrun{
#'    query_clusters(test_data, characteristic_distance = 32000)
#'    # note: `characteristic_distance` should usually be much smaller.
#' }
#'
#' @export
#'
query_clusters <- function (
    data,
    index_column = "idx",
    coordinate_columns = NULL,
    characteristic_distance = 1 # m
    ) {

  if (is.null(coordinate_columns)) {
    coordinate_columns <- c("x", "y")
  }

  stopifnot(
    assertthat = require("assertthat"),
    dplyr = require("dplyr")
  )

  check_common_assertions(data, index_column, coordinate_columns)

  # because this produces a lookup, we will work on distinct rows.
  data_distinct <- data[, c(index_column, coordinate_columns)] %>%
    dplyr::distinct(.keep_all = TRUE)

  # TODO: there is a `fastcluster::hclust` alternative which
  #       might have selective advantage on large data sets

  cluster_lookup <- data_distinct[, coordinate_columns] %>%
    stats::dist() %>%
    stats::hclust(method = "complete") %>%
    stats::cutree(h = characteristic_distance) %>%
    dplyr::as_tibble() %>%
    dplyr::rename(cluster = value) %>%
    dplyr::bind_cols(
      as_tibble(data_distinct[, index_column]),
      .,
      .name_repair = "unique") %>%
    setNames(c(index_column, "cluster")) %>%
    distinct(.keep_all = TRUE) %>%
    dplyr::mutate_at(dplyr::vars(cluster), as.factor)

  return(cluster_lookup)
}


#' Group a set of locations in clusters and join cluster info.
#'
#' @inherit query_clusters
#'
#' @export
#'
join_clusters <- wrap_query_to_join(query_clusters)


#' Remove clusters with too few members.
#'
#' This will filter the data to only retain locations in clusters with
#' a minimum count of members.
#'
#' @param data the data, in data frame format
#' @param cluster_column the column holding a cluster, e.g. `cluster`
#' @param minimum_cluster_member_count as the name suggests.
#'
#' @return subset of the data which exceeds cluster size threshold.
#'
#' @examples
#' \dontrun{
#'   get_example_data() %>%
#'     join_clusters(characteristic_distance = 32000) %>%
#'     remove_underpopulated_clusters(minimum_cluster_member_count = 2)
#' }
#'
remove_underpopulated_clusters <- function(
    data,
    cluster_column = "cluster",
    minimum_cluster_member_count = 1
    ) {

  stopifnot(
    assertthat = require("assertthat"),
    dplyr = require("dplyr")
  )

  # data
  assertthat::assert_that(
    inherits(data, "data.frame"),
    msg = paste0("Input data must be a data.frame-like object.")
  )

  assertthat::assert_that(
    cluster_column %in% colnames(data),
    msg = paste0(
      "The cluster column `", cluster_column,
      "` is not in the data columns: ",
      paste(colnames(data), collapse = ",")
    )
  )


  # find clusters with few members
  list_of_excluded_clusters <- sapply((data %>%
    group_by(!!!dplyr::syms(cluster_column)) %>%
    summarize(count = n()) %>%
    filter(count < minimum_cluster_member_count)
    )[, cluster_column], FUN = as.character)
  # I miss pandas.

  # remove irrelevant clusters
  data <- data[!(as.character(data[, cluster_column]) %in% list_of_excluded_clusters), ]
  data[, cluster_column] <- droplevels(data[, cluster_column])
  # I desparately miss pandas.

  return(data)
}



#_______________________________________________________________________________
# dhmv elevation
#_______________________________________________________________________________

#' Return the normed vector.
#'
#' i.e. `v/|v|`
#'
#' @param vec a vector
#'
#' @return the vector scaled to unit length
#'
#' @examples
#' \dontrun{
#'   normed(c(1, 1)) # == c(0.7071068, 0.7071068)
#' }
#'
get_normed <- function(vec) vec / norm(vec, type = "2")



#' Query the elevation of a point.
#'
#' Given an xy tuple of `c("x" = ?, "y" = ?)`, this function
#' will query and return the elevation data from DHMV Vlaanderen DTM WCS.
#' The function requires an index, to make sure data gets associated correctly,
#' and it will also return the slope calculated from points within 2.5m range.
#'
#' @param idx an identifier, such as a value from an index column
#' @param xy a vector of coordinates, `c(x, y)`
#'
#' @return list("idx" = idx, "elevation_dhmv" = h, "slope_r2.5m" = slope)
#'     the elevation and slope at the given point,
#'     interpolated from a `1m` resolution raster with +/-0.05m accuracy.
#'     Returns `NA` if the web query fails.
#'
#' @examples
#' \dontrun{
#'   xy <- c(178379, 209418)
#'   get_single_point_elevation("test", xy)$elevation # == 9.6m
#' }
#'
#' @export
#'
get_single_point_elevation <- function (idx, xy) {

  stopifnot(inbospatial = require('inbospatial'))

  margin <- 2.5 # query an area around the focus point

  outcome <- NULL # required to identify failure in the tryCatch
  # occasionally, the query fails with a `404`; this is caught.
  tryCatch({
    outcome <- inbospatial::get_coverage_wcs(
      wcs = "dhmv",
      bbox = sf::st_bbox(
        c(xmin = xy[1]-margin, xmax = xy[1]+margin,
          ymin = xy[2]-margin, ymax = xy[2]+margin
          ), crs = sf::st_crs(31370)),
      layername = "DHMVII_DTM_1m",
      version = "1.0.0",
      wcs_crs = "EPSG:31370",
      resolution = 1
      )
  },
  error = function(cond) {
    message(paste0("Error on DHMV query for xy = {", paste(xy, collapse = ", "), "}."))
    message(paste0(">\t", cond, "\n"))
  }
  )

  # in case the query was unsuccesful, return NA
  if (is.null(outcome)) {
    return(list("idx" = idx, "elevation_dhmv" = NA, "slope_r2.5m" = NA))
  }

  # otherwise, extract and return the elevation value.
  i <- as.integer(floor(length(as.matrix(outcome))/2+1))
  h <- as.numeric(outcome[i]) # elevation!

  mat <- as.matrix(outcome, wide = TRUE)

  z <- as.vector(mat)
  x <- rep(1:(2*margin), each = 2*margin)-margin-0.5 # the geographical x -> columns
  y <- rep(1:(2*margin), times = 2*margin)-margin-0.5 # the geographical y -> rows

  # get the x, y component of the third eigenvector (= surface normal)
  ea <- prcomp(cbind(x, y, z)) # PCA of x, y, z
  # the longer the x,y component, the more tilted the surface
  slope <- norm(get_normed(ea$rotation[,3])[1:2], type = "2")

  return(list("idx" = idx, "elevation_dhmv" = h, "slope_r2.5m" = slope))

}




#' Query the elevation of many locations.
#'
#' This will iterate over all data rows and return the elevation
#' from a DHMV query.
#'
#' @param data the data, in data frame format
#' @param index_column the column holding a row identifier, e.g. `idx`
#' @param coordinate_columns columns in which the coordinates are stored
#'        e.g. `c(x, y)`
#'
#' @return elevation_lookup a data frame with the index column and elevation
#'     info. Elevation info are the `elevation_dhmv`, and a `slope_XXXm`.
#'     The range for the slope is hardcoded above.
#'
#' @examples
#' \dontrun{
#'    query_elevation(test_data)
#' }
#'
#' @export
#'
query_elevation <- function(
    data,
    index_column = "idx",
    coordinate_columns = NULL,
    progress = TRUE
    ) {

  if (is.null(coordinate_columns)) {
    coordinate_columns <- c("x", "y")
  }

  stopifnot(
    assertthat = require("assertthat"),
    dplyr = require("dplyr"),
    sf = require("sf")
  )

  check_common_assertions(data, index_column, coordinate_columns)

  # because this produces a lookup, we will work on distinct rows.
  data_distinct <- data[, c(index_column, coordinate_columns)] %>%
    dplyr::distinct(.keep_all = TRUE)

  if (progress) pb <- txtProgressBar(min = 0, max = nrow(data_distinct),
                       initial = 0, style = 1)

  # helper function to query multiple positions
  rowwise_elevation <- function(i){
    # update the progress bar
    if (progress) setTxtProgressBar(pb,i)

    idx <- data_distinct[i, index_column]

    # ensure format of the xy tuple
    xy <- as.vector(as.matrix(
        data_distinct[i, coordinate_columns]
      ))

    # query and return the elevation
    return(get_single_point_elevation(idx, xy))

  }

  # this will execute the row-wise data query from DHMV
  # [!] takes a long time: start execution, grab a coffee and a good book.
  elevation_lookup <- dplyr::bind_rows(
    lapply(
      1:nrow(data_distinct),
      FUN = rowwise_elevation
    )
  )

  if (progress) close(pb) # close the progress bar

  colnames(elevation_lookup) <-
    c(index_column, "elevation_dhmv", "slope_r2.5m")


  # there can still be duplicates,
  #   if "distinct" above returned multiple coords
  elevation_lookup <- elevation_lookup %>%
    dplyr::summarize(
      dplyr::across(
        dplyr::everything(),
        ~ mean(.x, na.rm = TRUE)
      ),
      .by = !!index_column
    )
  # elevation_lookup <- elevation_lookup %>%
  #   dplyr::distinct(.keep_all = TRUE)

  return(elevation_lookup)

}


#' Query and join the elevation of many locations.
#'
#' @inherit query_elevation
#'
#' @export
#'
join_elevation <- wrap_query_to_join(query_elevation)

#_______________________________________________________________________________
# soilclass
#_______________________________________________________________________________

#' Query the soilclass of many locations.
#'
#' This will iterate over all data rows and return the soilclass
#' inferred from the `soilmap_simple` data set on zenodo
#' (see DOI 10.5281/zenodo.3732903).
#'
#' @param data the data, in data frame format
#' @param index_column the column holding a row identifier, e.g. `idx`
#' @param coordinate_columns columns in which the coordinates are stored,
#'        e.g. `c(x, y)`
#'
#' @return soilclass_lookup a data frame with the index column and soilclass.
#'     the `soilclass` column is converted to a factor with the levels
#'     `light`, `heavy`, `peat`, and `unknown`.
#'
#' @examples
#' \dontrun{
#'    query_soilclass(test_data)
#' }
#'
#' @export
#'
query_soilclass <- function(
    data,
    index_column = "idx",
    coordinate_columns = NULL
    ) {

  if (is.null(coordinate_columns)) {
    coordinate_columns <- c("x", "y")
  }

  stopifnot(
    assertthat = require("assertthat"),
    dplyr = require("dplyr"),
    sf = require("sf")
  )

  check_common_assertions(data, index_column, coordinate_columns)

  # because this produces a lookup, we will work on distinct rows.
  data_distinct <- data[, c(index_column, coordinate_columns)] %>%
    dplyr::distinct(.keep_all = TRUE)

  source("./zenodo_helper.R")
  soilmap_raw <- load_zenodo_data("soilmap_simple")

  soilmap <- soilmap_raw %>%
    dplyr::transmute(
      soilclass = dplyr::case_when(
        stringr::str_detect(bsm_mo_tex, "V") ~ "peat",
        stringr::str_detect(bsm_mo_tex, "^U") ~ "heavy",
        stringr::str_detect(bsm_mo_tex, "[SZPLX]") ~ "light",
        stringr::str_detect(bsm_mo_tex, ".+") ~ "heavy",
        .default = "unknown"
      ) %>%
        factor()
    )


  soil_locations <- data_distinct %>%
    sf::st_as_sf(coords = coordinate_columns, crs = 31370)

  soilclass_lookup <- sf::st_join(
      soil_locations, soilmap
    ) %>%
    dplyr::mutate(soilclass = tidyr::replace_na(soilclass, "unknown")) %>%
    sf::st_drop_geometry() %>%
    distinct(.keep_all = TRUE) %>%
    dplyr::mutate_at(dplyr::vars(soilclass), as.factor)

  return(soilclass_lookup)
}


#' Query and join the soilclass of many locations.
#'
#' @inherit query_soilclass
#'
#' @export
#'
join_soilclass <- wrap_query_to_join(query_soilclass)




#_______________________________________________________________________________
# waterlevel quantiles
#_______________________________________________________________________________

#' Query waterlevel quantiles of many locations.
#'
#' This will iteratively query water levels
#' and determine quantiles per year
#'
#' @param data the data, in data frame format
#' @param index_column the column holding a row identifier, e.g. `idx`
#'
#' @return a data frame of water level quantiles (in columns),
#'         per year and location
#'
#' @examples
#' \dontrun{
#'    query_waterlevel_quantiles(test_data)
#' }
#'
#' @export
#'
query_waterlevel_quantiles <- function(
    data,
    index_column = "idx",
    n_quantiles = 20,
    db_conn = NA,
    progress = TRUE
    ) {

  stopifnot(
    assertthat = require("assertthat"),
    dplyr = require("dplyr")
  )

  check_common_assertions(data, index_column,
                          coordinate_columns = c(),
                          skip_coordinates = TRUE
                          )

  # data type
  assertthat::assert_that(
    is.numeric(n_quantiles) && (n_quantiles > 0),
    msg = paste0("Number of quantiles (`n_quantiles`) must be an integer greater than zero.")
  )

  # because this produces a lookup, we will work on distinct rows.
  indices <- sort(unique(data[[index_column]]))

  # specific helper functions

  get_trace <- function(pp_idx, show_plot = FALSE) {
    # pp_idx = 24

    peilmeting <- tbl(db_conn, "FactPeilMeting") %>%
      filter(
        PeilpuntWID == pp_idx,
      ) %>%
      mutate(is_vld = PeilmetingStatusCode == "VLD") %>%
      select(DatumWID, is_vld, mTAW) %>%
      arrange(DatumWID) %>%
      collect

    peilmeting <- peilmeting %>%
      mutate(date = as.Date(as.character(DatumWID), format = "%Y%m%d"))

    t <- peilmeting %>% pull(date)
    t0 <- t[1]
    t <- as.numeric(difftime(t, t0, units = "days"))
    y <- peilmeting %>% pull(mTAW)
    vld <- peilmeting %>% pull(is_vld)

    return(list("t" = t[vld], "y" = y[vld], "t0" = t0, "vld" = vld, "tx" = t, "yx" = y))
  }


  if (progress){
    pb <- txtProgressBar(
      min = 0, max = length(indices),
      initial = 0, style = 1
    )
  }

  # choice / storage of quantiles per year
  qntls <- seq(0.0, 1.0, length.out = 1+n_quantiles)
  qq_data <- list()

  # print(length(indices))

  for (i in 1:length(indices)) {

    if (progress) setTxtProgressBar(pb, i)
    idx <- indices[i]
    trace <- get_trace(idx, show_plot = FALSE)

    ### yearly quantiles
    qq_store <- list()
    yearvec <- lubridate::year(trace[["t0"]] + trace[["t"]])
    waterlevel <- trace[["y"]]
    for (yr in sort(unique(yearvec))){
      yearlevel <- waterlevel[yearvec == yr]
      if (length(yearlevel) < 4) next
      qq_store[[yr]] <- c(idx, yr, length(yearlevel),
        t(as.numeric(quantile(yearlevel, qntls, na.rm = TRUE)))
      )
    }
    qq_data[[i]] <- as.data.frame(do.call("rbind", qq_store))
    if (length(qq_store) > 0) {
      colnames(qq_data[[i]]) <- c(index_column, "yr", "n", paste0("q", sprintf("%03.0f", 100*qntls)))
    }

  }

  if (progress) close(pb) # close the progress bar

  # store quantiles
  qq_data <- bind_rows(qq_data)

  return(qq_data)
}


#' Query and join the waterlevel quantiles of many locations.
#'
#' @inherit query_waterlevel_quantiles
#'
#' @export
#'
join_waterlevel_quantiles <- wrap_query_to_join(query_waterlevel_quantiles)



#_______________________________________________________________________________
# distance from water body
#_______________________________________________________________________________

#' Query the minimum water distance of many locations, per cluster (SLOW!).
#'
#' This will iterate over all data clusters and return the water distances
#' of all points to their respective closest water body.
#' water sources are:
#'   - watersurfaces ("10.5281/zenodo.3386857")
#'   - habitatstreams ("10.5281/zenodo.3386245") # type 3260
#'   - watercourses ("10.5281/zenodo.4420905")
#' WARNING: the performance of this function strongly depends on clustering.
#'   It is desirable to get as few clusters as possible, yet not exceeding
#'   a range of ~5km per cluster.
#'   Default values for `cluster_dist` should work reasonably well.
#'
#' @param data the data, in data frame format
#' @param index_column the column holding a row identifier, e.g. `idx`
#' @param coordinate_columns columns in which the coordinates are stored,
#'        e.g. `c(x, y)`
#' @param cluster_column column with clusters if the data is already clustered
#'
#' @return water_lookup a data frame with the index column and water distance
#'        information:
#'        - `wata_min_dist`: distance to closest water, in meters
#'        - `wata_min_src`: which of the data sources was close
#'          (surfaces, courses, or streams)
#'        - `wata_min_idx`: index of the closest water in its data source
#'
#' @examples
#' \dontrun{
#'    query_waterdistance(test_data)
#' }
#'
#' @export
#'
query_waterdistance <- function (
    data,
    index_column = "idx",
    coordinate_columns = NULL,
    cluster_column = NA,
    progress = TRUE
    ) {

  if (is.null(coordinate_columns)) {
    coordinate_columns <- c("x", "y")
  }

  stopifnot(
    assertthat = require("assertthat"),
    dplyr = require("dplyr"),
    sf = require("sf")
  )

  check_common_assertions(data, index_column, coordinate_columns)


  if (is.na(cluster_column)) {
    # assuming that the data has not been clustered
    # (because the user did not provide a cluster column)

    data_distinct <- data[, c(index_column, coordinate_columns)] %>%
      dplyr::distinct(.keep_all = TRUE)

    # cluster range default
    cluster_dist <- 3400 # this cutoff distance seemed good for other purposes
    clusters <- query_clusters(
      data_distinct,
      index_column,
      coordinate_columns,
      characteristic_distance = cluster_dist
    )

    # join cluster index
    data_distinct <- data_distinct %>%
      dplyr::left_join(clusters,
        by = index_column,
        relationship = "one-to-one"
      )
    cluster_column <- "cluster"

  } else {
    # in case the data is already clustered
    data_distinct <-
      data[, c(index_column, coordinate_columns, cluster_column)] %>%
      dplyr::distinct(.keep_all = TRUE)

    # # below, the cluster column must NOT NECESSARILY have the name "cluster"
    # data_distinct$cluster <- data_distinct[, cluster_column]
  }

  # factors won't work here
  data_distinct[[cluster_column]] <- as.integer(data_distinct[[cluster_column]])

  # load more helpers
  source("./spatial_helpers.R")
  source("./water_sources.R")

  # query water resources
  all_wata <- load_all_water_sources()

  # search range parameters
  min_searchradius <- 1000. # minimum radius around a cluster center
  # extra safety margin around location clusters, should be > min_searchradius
  safety_margin <- min_searchradius + 500.

  # single cluster processing
  get_min_water_distances_clusterwise <- function (cluster_data) {

    # convert data subset to `sf`
    cluster_data <- sf::st_as_sf(
      cluster_data,
      coords = coordinate_columns,
      crs = 31370
    )

    if (nrow(cluster_data) == 1) {
      cr <- list()
      cr$center <- as.vector(sf::st_coordinates(cluster_data)[1,])
      cr$radius <- min_searchradius
    } else {

      # center and radius
      cr <- find_center_and_radius(sf::st_coordinates(cluster_data))

      # minimum radius
      cr$radius <- max(min_searchradius, cr$radius)
    }

    # find adjacent water...
    sub_wata <- narrow_sources_radius(
      all_wata,
      cr$center,
      radius = cr$radius * 1.01 + safety_margin
    )

    # ... or not.
    if (sum(unlist(lapply(sub_wata, FUN = nrow))) <= 0) {
      return(invisible(NULL))
    }

    # calculate water distances
    water_distances <- collect_combined_distances(
      cluster_data, sub_wata, point_index_col = index_column
    )

    # get the minimum water distance
    min_water_distance <- minimum_cross_distance(water_distances)

    # rename the columns
    colnames(min_water_distance) <-
      c(index_column, "wata_min_dist", "wata_min_src", "wata_min_idx")

    # return water distances for this cluster
    return(min_water_distance)
  } # cluster-wise water distances


  # progress bar
  if (progress) {
  pb <- txtProgressBar(
    min = 0,
    max = max(data_distinct[, cluster_column]),
    initial = 0, style = 1)
  }

  # wrapping a progress bar around the above procedure
  waterdist_query_pb <- function (cluster_idx) {

    if (progress) {
      setTxtProgressBar(pb, cluster_idx)
      # print(cluster_idx)
    }
    sub_data <- data_distinct[data_distinct[[cluster_column]] == cluster_idx, ]
    return(get_min_water_distances_clusterwise(sub_data))
  }

  # cluster-wise application of the water search
  water_lookup <- lapply(
    sort(unique(data_distinct[[cluster_column]])),
    FUN = waterdist_query_pb
    )

  if (progress) close(pb)

  # combine and return the output data
  water_lookup <- dplyr::bind_rows(water_lookup)

  # # restore index column
  # water_lookup[, index_column] <-
  #   water_lookup[, index_column] %>%
  #   mutate_at(as.integer, .vars = index_column)

  # bonus: a simple water distance class
  meters <- function(x) units::set_units(x, "m")
  water_lookup <- water_lookup %>%
    mutate(
      wata_dist_class = factor(
        dplyr::case_when(
          wata_min_dist <= meters(5) ~ "close",
          wata_min_dist <= meters(50) ~ "mid",
          .default = "far"
        )
      )
    )

  return(water_lookup)
}


#' Query and join the minimum water distance of many locations.
#'
#' @inherit query_waterdistance
#'
#' @export
#'
join_waterdistance <- wrap_query_to_join(query_waterdistance)



#_______________________________________________________________________________
# testing
#_______________________________________________________________________________

#' Test all the queries and joins above.
#'
#' Above are functions of the type `query_*` and `join_*`.
#' (The latter are a pipe-able wrapper for the first.)
#' This procedure will test all of them, and query the following information
#' for test locations:
#'   - clusters
#'   - elevation (from DHMV)
#'   - soilclass
#'   - distance to water
#'
#' @examples
#' \dontrun{
#'    source("./auxiliary_data_queries.R")
#'    test_all_lookups()
#' }
#'
#' @export
#'
test_all_lookups <- function(){
  little_data <- get_example_data()

  stopifnot(dplyr = require('dplyr')) # required for the `%>%` pipe

  much_data <- little_data %>%
    join_clusters(characteristic_distance = 32000) %>%
    remove_underpopulated_clusters(minimum_cluster_member_count = 2) %>%
    join_elevation() %>%
    join_soilclass() %>%
    join_waterdistance(cluster_column = "idx")

  dplyr::glimpse(much_data)


}


#_______________________________________________________________________________
# end of file.
