library("googledrive")
library("dplyr")
library("sf")
library("n2khab")

#### google authentification
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


# https://drive.google.com/file/d/1a42qESF5L8tfnEseHXbTn9hYR1phqS-S/view?usp=drive_link
path <- file.path(tempdir(), "objects_panflpan5.RData")
googledrive::drive_download(as_id("1a42qESF5L8tfnEseHXbTn9hYR1phqS-S"), path = path)
# https://drive.google.com/file/d/1a42qESF5L8tfnEseHXbTn9hYR1phqS-S/view?usp=drive_link

# load the data into a new environment
env_extradata <- new.env()
load(path, envir = env_extradata)

# ls(envir = env_extradata)

get_variable <- function(varname) get(varname, envir = env_extradata)
# units_locations <- get_variable("scheme_moco_ps_stratum_sppost_spsamples_sf")
# get_variable("mhq_samples")
# ? get_variable("sp_samplingframes")
#   samfra <- get_variable("sp_samplingframes")
#   samfra[[1, "sampling_frame"]]
# get_variable("samplinglocations_sf")
# get_variable("n2khab_strata")

## <https://github.com/inbo/n2khab-mne-design/blob/poc/100_design_common/010_prototyping_sampling_and_revisit_design/helper_scripts/support_dcp_and_fieldwork.R>
# scheme_moco_ps_stratum_sppost_spsamples_sf %>%
#   filter(str_detect(scheme, "^(GW)")) %>%
#   distinct(stratum, grts_address, geometry) %>%
#   # adding type metadata
#   inner_join(
#     n2khab_strata,
#     join_by(stratum),
#     relationship = "many-to-one",
#     unmatched = c("error", "drop")
#   ) %>%
#   inner_join(
#     read_types() %>%
#       select(
#         type,
#         hydr_class,
#         hydr_class_shortname
#       ),
#     join_by(type),
#     relationship = "many-to-one",
#     unmatched = c("error", "drop")
#   ) %>%
#   select(-type) %>%
#   relocate(geometry, .after = last_col())

# get_variable("mhq_scheme_category") %>%
#  left_join(
#    get_variable("samplinglocations_sf"),
#    join_by(grts_address == grts_address_final),
#    relationship = "many-to-one"
#  ) %>%
#   distinct(scheme)
sample <- get_variable("scheme_moco_ps_stratum_sppost_spsamples_sf") %>%
  # cbind(sf::st_drop_geometry(.), sf::st_coordinates(.)) %>%
  filter(stringr::str_detect(scheme, "^(GW)")) %>%
  inner_join(
    get_variable("n2khab_strata"),
    join_by(stratum),
    unmatched = c("error", "drop"),
    relationship = "many-to-many"
  ) %>%
  inner_join(
    n2khab::read_types(),
    join_by(type),
    unmatched = c("error", "drop"),
    relationship = "many-to-many"
  ) %>%
  select(stratum, grts_address, hydr_class, hydr_class_shortname, geometry)
# df: {stratum, grts_address, hydr_class, hydr_class_shortname, geometry}

sample <- sf::st_as_sf(sample)
sample <- cbind(sf::st_drop_geometry(sample), sf::st_coordinates(sample)) %>%
  filter(if_all(everything(), ~ !is.na(.x)))

store_filepath <- file.path("./cache", "spsamples_gw_sf_panflpan5.rds")
saveRDS(sample, store_filepath)
