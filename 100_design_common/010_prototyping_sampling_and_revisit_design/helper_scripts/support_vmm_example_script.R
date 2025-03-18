library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
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
  count(scheme, scheme_shortname, scheme_name, sample_status)

# Number of stratum-location combinations per scheme and per area
spsamples_sf_vmm %>%
  st_drop_geometry() %>%
  count(area, scheme, scheme_shortname, sample_status) %>%
  pivot_wider(names_from = area, values_from = n, values_fill = 0)

# Optionally apply filters, then collapsing all (selected) schemes. In this
# process, the date_intervals variable is rebuilt, which takes rather long.
stratum_type_location <-
  spsamples_sf_vmm %>%
  # # optionally, apply filter to select schemes of interest:
  # filter(str_detect(scheme, "GW|SOIL")) %>%
  # # optionally, apply filter to select typeclasses of interest:
  # filter(typeclass %in% c("HS", "GR)) %>%
  select(-date_intervals) %>%
  unnest(date_interval_nested, keep_empty = TRUE) %>%
  distinct(pick(-starts_with("scheme"))) %>%
  arrange(date_interval) %>%
  as_tibble() %>%
  nest(
    sample_statuses = sample_status,
    date_interval_nested = date_interval
  ) %>%
  mutate(
    sample_status = map_chr(
      sample_statuses,
      function(ss) ss$sample_status %>% droplevels() %>% levels() %>% first()
    ),
    date_intervals = map_chr(
      date_interval_nested,
      function(din) str_flatten(unique(din$date_interval), collapse = ", ", na.rm = TRUE)
    )
  ) %>%
  mutate(across(where(is.character), factor)) %>%
  select(
    stratum,
    area,
    grts_address,
    sample_status,
    date_interval_nested,
    date_intervals,
    starts_with("type"),
    geometry
  ) %>%
  st_as_sf()
stratum_type_location

# Count filtered & collapsed locations per area, stratum and sample status
stratum_type_location %>%
  st_drop_geometry() %>%
  count(area, stratum, type, type_shortname, sample_status) %>%
  arrange(area, stratum)

# Select first n locations per area and stratum (if more are available)
first_n <- 5
first_n_loc_per_stratum_and_area <-
  stratum_type_location %>%
  slice_min(grts_address, n = first_n, by = c(area, stratum))
first_n_loc_per_stratum_and_area

# Check the retained number of locations per area, stratum and sample status
first_n_loc_per_stratum_and_area %>%
  st_drop_geometry() %>%
  count(area, stratum, sample_status)

# Make these locations unique across strata (once more, recalculate sample
# status and date interval columns)
first_n_loc <-
  first_n_loc_per_stratum_and_area %>%
  unnest(date_interval_nested, keep_empty = TRUE) %>%
  arrange(date_interval) %>%
  summarize(
    sample_status = sample_status %>% droplevels() %>% levels() %>% first(),
    date_intervals = str_flatten(unique(date_interval), collapse = ", ", na.rm = TRUE),
    .by = c(area, grts_address, geometry)
  ) %>%
  # including stratum & type metadata without duplicating rows:
  inner_join(
    first_n_loc_per_stratum_and_area %>%
      select(-sample_status, -date_intervals, -date_interval_nested) %>%
      # nest() doesn't work on sf
      as_tibble() %>%
      nest(stratum_type = matches("stratum|type")),
    join_by(area, grts_address, geometry),
    relationship = "one-to-one",
    unmatched = "error"
  ) %>%
  arrange(area, sample_status, grts_address) %>%
  relocate(geometry, .after = last_col())
first_n_loc

# Plot locations on a map
mapview(
  first_n_loc_per_stratum_and_area %>%
    select(-date_interval_nested),
  zcol = "typeclass_name",
  col.regions = cols4all::c4a("carto.safe", 10),
  alpha.regions = 1,
  label = "grts_address"
)
