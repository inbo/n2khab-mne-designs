# 2026-09-04 Writing a geopackage of the spatial sampling units of a set of
# schemes with selected sampling unit attributes & temporal attributes (until
# 2035).
#
# At the time of writing, results were made with the RData file at tag
# rep_0.18.0, obtained by running:
#
# Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::html_document2",
# params = list(save_rdata = TRUE))'

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
library(rprojroot)

projroot <- find_root(is_rstudio_project)
datapath <- file.path(projroot, "data")

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

# Download and load R objects from the REP into global environment
path <- file.path(tempdir(), "objects_panflpan5.RData")
drive_download(as_id("1a42qESF5L8tfnEseHXbTn9hYR1phqS-S"), path = path)
load(path)


# Settings ----------------------------------------------------------------

selected_schemes <- c("SOIL_03.2")
year_end <- 2035L


# Helper objects ----------------------------------------------------------

grts_mh <- read_GRTSmh()
# create a spatial index of the GRTS addresses
grts_mh_index <- tibble(
  id = seq_len(ncell(grts_mh)),
  grts_address = values(grts_mh)[, 1]
) %>%
  filter(!is.na(grts_address)) %>%
  append_masked_grts_addresses()


# Filtering and adding needed attributes ----------------------------------

scheme_type_grts_sptempattribs <-
  scheme_moco_ps_spsubset_targetfag_stratum_sppost_spsamples_calendar %>%
  # using type instead of stratum (only relevant for specific types)
  inner_join(
    n2khab_strata,
    join_by(stratum),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  mutate(year_sampling = year(date_start)) %>%
  filter(
    scheme %in% selected_schemes,
    year_sampling <= year_end
  ) %>%
  # adding domain partition
  inner_join(
    domainpart_grts_n2khab,
    join_by(grts_address),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  # eliminating duplications of sampling unit x year between panel sets
  distinct(
    scheme,
    type,
    grts_address,
    grts_address_final,
    domain_part,
    year_sampling
  ) %>%
  arrange(scheme, type, grts_address, year_sampling) %>%
  # flattening year_sampling to obtain unique spatial sampling units (type x
  # grts_address)
  summarise(
    sampling_years =
      str_flatten(sort(unique(year_sampling)), collapse = "|") %>% factor(),
    .by = c(scheme, type, grts_address, grts_address_final, domain_part)
  ) %>%
  # adding the cell centers that represent each grts_address_final (beware that
  # it depends on the involved types whether this point position actually
  # correctly represent the sampling units topologically, e.g. for lentic types
  # this point can be outside the sampling unit, hence misleading)
  add_point_coords_grts(
    grts_var = "grts_address_final",
    spatrast = grts_mh,
    spatrast_index = grts_mh_index
  ) %>%
  relocate(domain_part, .before = geometry)


# Optionally write to Geopackage ------------------------------------------

if (FALSE) {
  gpkg_path <- file.path(
    datapath,
    "binary/results/infodelivery/mnm_steekproef_meetnetselectie.gpkg"
  )
  first_scheme <- scheme_type_grts_sptempattribs$scheme[1]
  scheme_type_grts_sptempattribs %>%
    base::split(.$scheme, drop = TRUE) %>%
    walk(\(df) {
      schemename <- df$scheme[1]
      write_sf(
        df,
        gpkg_path,
        layer = schemename,
        delete_dsn = schemename == first_scheme,
        delete_layer = TRUE
      )
    })
}
