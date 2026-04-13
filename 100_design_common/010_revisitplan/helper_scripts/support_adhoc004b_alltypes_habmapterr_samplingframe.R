# Generating a GeoPackage with filtered habitatmap polygons and base sampling
# frame locations that correspond to a single type

# First run setup chunk
#
# Then run:

load(file.path(datapath, "binary/results/objects_panflpan5.RData"))

path_gpkg <- file.path(
  datapath,
  "binary/intermediate/samplingframes_gw/samplingframes_gw.gpkg"
)

strata_gw <-
  submodule_targetpops_strata %>%
  filter(str_detect(scheme, "^GW_")) %>%
  distinct(stratum)

terr_strata_gw <-
  strata_gw %>%
  inner_join(
    n2khab_strata,
    join_by(stratum),
    relationship = "one-to-one",
    unmatched = c("error", "drop")
  ) %>%
  semi_join(
    n2khab_types_expanded_properties %>%
      filter(
        str_detect(grts_join_method, "cell"),
        type != "7140_mrd"
      ),
    join_by(type)
  ) %>%
  select(stratum)

hmt <- read_habitatmap_terr(keep_aq_types = FALSE, drop_7220 = TRUE)

for (stratum_i in terr_strata_gw$stratum) {
  # generating and writing layer with habitatmap_terr polygons that contain the
  # (exact) type, including the phab value
  hmt_occ_1type <-
    hmt$habitatmap_terr_types %>%
    filter(type == stratum_i)
  hmt$habitatmap_terr_polygons %>%
    select(polygon_id) %>%
    inner_join(
      hmt_occ_1type,
      join_by(polygon_id),
      relationship = "one-to-many",
      unmatched = c("drop", "error")
    ) %>%
    # making polygon_id x type unique
    filter(
      certain == any(certain),
      .by = c(polygon_id, geom, type)
    ) %>%
    write_sf(
      path_gpkg,
      layer = str_c("habitatmap_terr_", stratum_i),
      delete_layer = TRUE
    )
}

grts_mh <- read_GRTSmh()
# create a spatial index of the GRTS addresses
grts_mh_index <- tibble(
  id = seq_len(ncell(grts_mh)),
  grts_address = values(grts_mh)[, 1]
) %>%
  filter(!is.na(grts_address))

# spatial sampling frame for groundwater schemes in pan_effectmon_flanders:
ssf_gw <-
  sp_samplingframes %>%
  semi_join(
    scheme_sampling_frame %>%
      # using the sampling frame for GW_03.3 will do for non-core GW schemes in
      # module pan_effectmon_flanders
      filter(scheme == "GW_03.3"),
    join_by(sampling_frame_id)
  ) %>%
  pluck("sampling_frame", 1)

# filtering the sampling frames, adding a simplified ranking that follows
# the (base 10, reverse) GRTS address, and writing the destination GRTS
# addresses as a spatial point layer
for (stratum_i in strata_gw$stratum) {
  ssf_gw %>%
  filter(stratum == stratum_i) %>%
  select(stratum, grts_address) %>%
  mutate(rank = rank(grts_address) %>% as.integer()) %>%
  arrange(grts_address) %>%
  add_assessment_data() %>%
  add_point_coords_grts(
    grts_var = "grts_address_final",
    spatrast = grts_mh,
    spatrast_index = grts_mh_index
  ) %>%
  write_sf(
    path_gpkg,
    layer = str_c("sampling_frame_gw ", stratum_i),
    delete_layer = TRUE
  )
}
