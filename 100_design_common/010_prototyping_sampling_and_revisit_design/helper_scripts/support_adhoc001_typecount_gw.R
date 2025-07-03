## Setup -----------------------------

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(lubridate)
library(sf)
library(terra)
library(n2khab)
library(googledrive)
library(readr)

# Setup for googledrive authentication. Set the appropriate env vars in
# .Renviron and make sure you ran drive_auth() interactively with these settings
# for the first run (or to renew an expired Oauth token).
# See ?gargle::gargle_options for more information.
if (Sys.getenv("GARGLE_OAUTH_EMAIL") != "") {
  options(gargle_oauth_email = Sys.getenv("GARGLE_OAUTH_EMAIL"))
}
if (Sys.getenv("GARGLE_OAUTH_CACHE") != "") {
  options(gargle_oauth_cache = Sys.getenv("GARGLE_OAUTH_CACHE"))
}

# Download and load R objects from the POC into global environment
path <- file.path(tempdir(), "objects_panflpan5.RData")
drive_download(as_id("1a42qESF5L8tfnEseHXbTn9hYR1phqS-S"), path = path)
load(path)







## Local functions --------------------------

#' Add grts_address_final and other attributes to a stratum x grts_address
#' object
#'
#' grts_address always refers to the GRTS address used in ranking and sampling,
#' but some locations may not have the targeted stratum and are linked to a
#' replacement site. This function adds the replacement site
#' (grts_address_final) and some other attributes from
#' stratum_grts_n2khab_phabcorrected_no_replacements.
#'
#' @param df Data frame holding a stratum and grts_address column.
add_assessment_data <- function(df) {
  df %>%
    inner_join(
      stratum_grts_n2khab_phabcorrected_no_replacements,
      join_by(stratum, grts_address),
      relationship = "many-to-one",
      unmatched = c("error", "drop")
    ) %>%
    mutate(
      grts_address_final = ifelse(is.na(replaced_by), grts_address, replaced_by)
    ) %>%
    relocate(grts_address_final, .after = grts_address) %>%
    select(-replaced_by)
}

#' Add point coordinate columns to a data frame with a GRTS address column
#'
#' @param df Data frame.
#' @param grts_var String. The column name in df that holds the GRTS addresses.
#' @param spatrast SpatRaster object with level 0 GRTS addresses.
#' @param spatrast_index Data frame with columns 'id' and 'grts_address',
#'   holding the cell numbers (cell IDs) for each GRTS address in `spatrast`.
#' @param spatial Logical. Should the returned object be a sf points object? If
#'   `FALSE`, a data frame is returned with x and y coordinates as columns.
#'
#' @returns An sf points object or a tibble with coordinates, depending on the
#'   `spatial` argument.
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

  coords <- xyFromCell(spatrast, grts_cells)

  df %>%
    left_join(
      tibble(grts_address = addresses, x = coords[, "x"], y = coords[, "y"]),
      join_by({{ grts_var }} == grts_address)
    ) %>%
    {
      if (isFALSE(spatial)) {
        .
      } else {
        st_as_sf(., coords = c("x", "y"), crs = crs(spatrast))
      }
    }
}






## Locations per type in groundwater monitoring ------------------

gw_type_grts <-
  scheme_moco_ps_stratum_sppost_spsamples %>%
  filter(str_detect(scheme, "^GW")) %>%
  inner_join(
    n2khab_strata,
    join_by(stratum),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  unnest(sp_poststr_samples) %>%
  add_assessment_data() %>%
  distinct(
    type,
    grts_address,
    grts_address_final
  )

# count locations per type

gw_type_grts %>%
  count(type) %>%
  write_csv("type_count_groundwater.csv")

# write as point locations

grts_mh <- read_GRTSmh()
# create a spatial index of the GRTS addresses
grts_mh_index <- tibble(
  id = seq_len(ncell(grts_mh)),
  grts_address = values(grts_mh)[, 1]
) %>%
  filter(!is.na(grts_address))

gw_type_grts %>%
  add_point_coords_grts(
    grts_var = "grts_address_final",
    spatrast = grts_mh,
    spatrast_index = grts_mh_index
  ) %>%
  write_sf("gw_type_grts.gpkg")
