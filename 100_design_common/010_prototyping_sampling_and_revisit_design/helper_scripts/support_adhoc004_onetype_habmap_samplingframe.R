# Generating a GeoPackage with filtered habitatmap polygons and base sampling
# frame locations that correspond to a single type

# First run setup chunk
#
# Then run:

load(file.path(datapath, "binary/results/objects_panflpan5.RData"))

type_chosen <- "6510_hus"

path_gpkg <- file.path(
  datapath,
  "binary/intermediate/explore_one_type",
  str_c(type_chosen, ".gpkg")
)

# generating and writing layer with habitatmap_terr polygons that contain the
# (exact) type, including the phab value
hmt <- read_habitatmap_terr(keep_aq_types = FALSE, drop_7220 = TRUE)
typelev <- levels(hmt$habitatmap_terr_types$type)
hmt_occ_1type <-
  hmt$habitatmap_terr_types %>%
  filter(type == type_chosen)
hmt$habitatmap_terr_polygons %>%
  select(polygon_id) %>%
  inner_join(hmt_occ_1type, by = "polygon_id") %>%
  st_make_valid() %>%
  write_sf(path_gpkg, delete_dsn = TRUE)

grts_mh <- read_GRTSmh()
# create a spatial index of the GRTS addresses
grts_mh_index <- tibble(
  id = seq_len(ncell(grts_mh)),
  grts_address = values(grts_mh)[, 1]
) %>%
  filter(!is.na(grts_address))

# filtering the base sampling frame, adding a simplified ranking that follows
# the (base 10, reverse) GRTS address, and writing the destination GRTS
# addresses as a spatial point layer
stratum_grts_n2khab_phabcorrected_no_replacements %>%
  filter(stratum == type_chosen) %>%
  select(stratum, grts_address) %>%
  mutate(rank = rank(grts_address) %>% as.integer()) %>%
  add_assessment_data() %>%
  add_point_coords_grts(
    grts_var = "grts_address_final",
    spatrast = grts_mh,
    spatrast_index = grts_mh_index
  ) %>%
  write_sf(
    path_gpkg,
    layer = "base_sampling_frame",
    delete_layer = TRUE
  )
