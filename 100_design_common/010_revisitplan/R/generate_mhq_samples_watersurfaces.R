# This script writes a vc-formatted file, derived from a GeoPackage with the MHQ
# watersurfaces sample. See also
# R/generate_mhq_samples_watersurfaces_spatialunits.R.

mhq_samples_binarydatapath <- file.path(datapath, "binary/0_raw/mhq_samples")

read_sf(file.path(
  mhq_samples_binarydatapath,
  "mhq_standingwater_cycle2_2024-04-17.gpkg"
)) %>%
  st_drop_geometry() %>%
  ## some duplicate grts_addresses exist, referring to different watersurfaces,
  ## hence population units!
  # filter(grts_ranking_draw == 2393618) %>% t()
  select(grts_ranking_draw, type_all, area_class) %>%
  separate_wider_delim(
    cols = type_all,
    delim = ";",
    names_sep = ":",
    too_few = "align_start"
  ) %>%
  pivot_longer(
    cols = starts_with("type"),
    names_to = "dummy",
    values_to = "type",
    values_drop_na = TRUE
  ) %>%
  mutate(type = str_trim(type)) %>%
  mutate(
    area_class = fct_recode(
      area_class,
      "0_1" = "area <= 1 ha",
      "1_5" = "1 ha < area <= 5 ha",
      "5_50" = "5 ha < area < 50 ha",
      "50_150" = "area >= 50 ha"
    ),
    stratum = parse_factor(
      str_c(type, "_", area_class),
      levels = levels(n2khab_strata$stratum)
    ),
    grts_ranking_draw = as.integer(grts_ranking_draw)
  ) %>%
  select(stratum, grts_address = grts_ranking_draw) %>%
  arrange(stratum, grts_address) %>%
  distinct() %>%
  write_vc(
    "mhq_samples_watersurfaces",
    root = file.path(datapath, "text/intermediate"),
    sorting = c("stratum", "grts_address"),
    strict = FALSE
  )
