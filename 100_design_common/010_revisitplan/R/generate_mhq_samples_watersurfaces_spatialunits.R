# This script writes a vc-formatted file, derived from a GeoPackage with the MHQ
# watersurfaces sample. See also R/generate_mhq_samples_watersurfaces.R.

mhq_samples_binarydatapath <- file.path(datapath, "binary/0_raw/mhq_samples")

read_sf(file.path(
  mhq_samples_binarydatapath,
  "mhq_standingwater_cycle2_2024-04-17.gpkg"
)) %>%
  st_drop_geometry() %>%
  distinct(
    unit_id = polygon_id,
    grts_address = as.integer(grts_ranking_draw)
  ) %>%
  mutate(sample_support_code = factor(
    "watersurface",
    levels = levels(points_non_cell_n2khab_grts$sample_support_code)
  )) %>%
  relocate(sample_support_code) %>%
  write_vc(
    "mhq_samples_watersurfaces_spatialunits",
    root = file.path(datapath, "text/intermediate"),
    sorting = c("unit_id", "grts_address"),
    strict = FALSE
  )
