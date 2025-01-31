library(dplyr)
library(tidyr)
library(sf)
library(mapview)
library(googledrive)
drive_deauth()

# Download and read R object
path <- file.path(tempdir(), "spsamples_sf_vmm.rds")
drive_download(as_id("1z9R4A59RucNZ0_Elvj6wetpZUkweDoD1"), path = path)
spsamples_sf_vmm <- readRDS(path)

glimpse(spsamples_sf_vmm)

# Number of stratum-location combinations per scheme
spsamples_sf_vmm %>%
  st_drop_geometry() %>%
  count(scheme, scheme_shortname, scheme_name)

# Number of stratum-location combinations per scheme and per area
spsamples_sf_vmm %>%
  st_drop_geometry() %>%
  count(area, scheme, scheme_shortname) %>%
  pivot_wider(names_from = area, values_from = n, values_fill = 0)

# Optionally apply filters, then collapsing all (selected) schemes
stratum_type_location <-
  spsamples_sf_vmm %>%
  # # optionally, apply filter to select schemes of interest:
  # filter(str_detect(scheme, "GW|SOIL")) %>%
  # # optionally, apply filter to select typeclasses of interest:
  # filter(typeclass %in% c("HS", "GR)) %>%
  distinct(pick(-starts_with("scheme")))
stratum_type_location

# Count filtered & collapsed locations per area and per stratum
stratum_type_location %>%
  st_drop_geometry() %>%
  count(area, stratum, type, type_shortname) %>%
  arrange(area, stratum)

# Select first n locations per area and stratum (if more are available)
first_n <- 5
first_n_loc_per_stratum_and_area <-
  stratum_type_location %>%
  slice_min(grts_address, n = first_n, by = c(area, stratum))
first_n_loc_per_stratum_and_area

# Check the retained number of locations per area and stratum
first_n_loc_per_stratum_and_area %>%
  st_drop_geometry() %>%
  count(area, stratum)

# Make these locations unique across strata
first_n_loc <-
  first_n_loc_per_stratum_and_area %>%
  distinct(area, grts_address, geometry) %>%
  # including stratum & type metadata without duplicating rows:
  inner_join(
    first_n_loc_per_stratum_and_area %>%
      # nest() doesn't work on sf
      st_drop_geometry() %>%
      nest(stratum_type = matches("stratum|type")),
    join_by(area, grts_address),
    relationship = "one-to-one",
    unmatched = "error"
  ) %>%
  relocate(geometry, .after = last_col())
first_n_loc

# Plot locations on a map
mapview(
  first_n_loc_per_stratum_and_area,
  zcol = "typeclass_name",
  col.regions = cols4all::c4a("carto.safe", 10),
  alpha.regions = 1,
  label = "grts_address"
)
