read_watercourses_src <- function () {
    source("water_sources.R")
    return(read_watercourses())
}

stopifnot(assertthat = require('assertthat'),
          n2khab = require('n2khab')
          )

n2khab_read_functions <- list(
  "habitatmap" = read_habitatmap,
  "habitatmap_stdized" = read_habitatmap_stdized,
  "habitatmap_terr" = read_habitatmap_terr,
  "habitatsprings " = read_habitatsprings,
  "habitatstreams " = read_habitatstreams ,
  "habitatquarries " = read_habitatquarries,

  "watersurfaces " = read_watersurfaces,
  "watercourses " = read_watercourses_src,
  "shallowgroundwater " = read_shallowgroundwater,

  "watersurfaces_hab " =
    function (...) read_watersurfaces_hab(..., interpreted = FALSE),
  "watercourse_100mseg " = read_watercourse_100mseg,

  "soilmap " = read_soilmap,
  "soilmap_simple " =
    function (file, ...) read_soilmap(file, simplify = TRUE, ...)
)




#' Get a dataframe which stores zenodo sources (n2khab_data).
#'
#' Assembling DOI's for common zenodo sources (n2khab_data)
#' distinguishing whether data are "raw" or processed.
#' Attributes are: "is_processed", "doi", "loading_function".
#'
#' @return data frame with "key" in rows and attributes in columns.
#'
#' @examples
#' \dontrun{
#'   zenodo_library <- get_zenodo_library()
#'   hm <- zenodo_library["habitatmap",]
#'   hm["doi"]
#' }
#'
get_zenodo_library <- function() {

  stopifnot(assertthat = require('assertthat'),
            n2khab = require('n2khab')
            )

  habitatmap <- c(FALSE, "10.5281/zenodo.3354381")
  habitatmap_stdized <- c(TRUE, "10.5281/zenodo.3355192")
  habitatmap_terr <- c(TRUE, "10.5281/zenodo.3468948")
  habitatsprings <- c(TRUE, "10.5281/zenodo.3550994")
  habitatstreams <- c(FALSE, "10.5281/zenodo.3386245")
  habitatquarries <- c(FALSE, "10.5281/zenodo.4072967")

  watersurfaces <- c(FALSE, "10.5281/zenodo.3386857")
  watercourses <- c(FALSE, "10.5281/zenodo.4420905")
  shallowgroundwater <- c(FALSE, "10.5281/zenodo.5902880")

  watersurfaces_hab <- c(TRUE, "10.5281/zenodo.3374645")
  watercourse_100mseg <- c(TRUE, "10.5281/zenodo.4452577")

  soilmap <- c(FALSE, "10.5281/zenodo.3387008")
  soilmap_simple <- c(TRUE, "10.5281/zenodo.3732903")

  zl <- t(data.frame(
    habitatmap,
    habitatmap_stdized,
    habitatmap_terr,
    habitatsprings,
    habitatstreams,
    habitatquarries,
    watersurfaces,
    watercourses,
    shallowgroundwater,
    watersurfaces_hab,
    watercourse_100mseg,
    soilmap,
    soilmap_simple
    ))

  colnames(zl) <- c("is_processed", "doi")

  return(zl)

}



#' (Down-)load zenodo data.
#'
#' Load zenodo data.
#' If it is not found, download it.
#'
#' @param key the key and folder name to fetch. Must match n2khab folder.
#' @param n2khab_data_path optionally provide the path where n2khab data
#'        is stored.
#' @param lazy boolean to indicate whether actual loading is skipped
#'        (i.e. download only).
#'
#' @return the data set selected.
#'
#' @examples
#' \dontrun{
#'   key <- "habitatmap"
#'   load_zenodo_data(key, n2khab_data_path = "C:\\R\\n2khab_data")
#' }
#'
load_zenodo_data <- function(
    key,
    n2khab_data_path = NULL,
    lazy = FALSE
    ) {

  # availability of assertthat and other packages
  stopifnot(assertthat = require('assertthat'),
            n2khab = require('n2khab')
            )

  # make sure string is passed as the "key" param.
  assert_that(is.string(key))

  # query zenodo library
  zenodo_library <- get_zenodo_library()

  # is the key in the data?
  if (!key %in% rownames(zenodo_library)) {
    stop(paste0("Key '", key, "' is not (yet) found in the zenodo list."))
  }

  # determine whether it is raw or processed data
  is_processed <- zenodo_library[key,]['is_processed']
  data_branch <- if (is_processed) "20_processed" else "10_raw"

  if (is.null(n2khab_data_path)) {
    # find n2khab data path
    tryCatch({
      n2khab_data_path <- locate_n2khab_data()
    }, error = function(e) {
      # if it is not found, create it.
      message("n2khab_data_path not found with cause:")
      message("\t", e)
      fileman_folders(root = "rproj")
    })
    # repeat for the "error" case
    n2khab_data_path <- locate_n2khab_data()
  }

  # double check that we got a path with the right structure
  assert_that(is.character(n2khab_data_path))
  fileman_folders(path = dirname(n2khab_data_path));

  # sub folder
  branch_path <- file.path(n2khab_data_path, data_branch)

  # create subfolder if it does not exist
  data_path <- file.path(branch_path, key)
  if (!file.exists(data_path)) {
    message("Creating path for ", key, ": ", data_path)
    dir.create(data_path, recursive = TRUE)
  }

  # download zenodo data
  if (length(list.files(data_path))>0) {
    message("Data '", key, "' already exists! (Skipping.)")
  } else {
    download_zenodo(
      doi = zenodo_library[key,]["doi"],
      path = data_path
    )
  }

  # optionally skip loading
  if (lazy) {
    return(invisible(NULL))
  }

  # get the loading function and load the data
  if (key %in% names(n2khab_read_functions)) {
    lfcn <- n2khab_read_functions[[key]]
    data <- lfcn(file = data_path)
    return(data)
  }

}



#' (Down-)load all known zenodo data.
#'
#' Load all zenodo data from the zenodo_library.
#' (If it is not found, download it.)
#'
#' @param n2khab_data_path optionally provide the path where n2khab data
#'        should be stored.
#' @examples
#' \dontrun{
#'   n2khab_data_path <- "C:\\R\\n2khab_data"
#'   fetch_all_zenodo_data(n2khab_data_path)
#' }
#'
fetch_all_zenodo_data <- function(n2khab_data_path = NULL) {
  zenodo_library <- get_zenodo_library()
  for (key in rownames(zenodo_library)) {
    load_zenodo_data(key, n2khab_data_path = n2khab_data_path,
      lazy = TRUE)
  }
}
