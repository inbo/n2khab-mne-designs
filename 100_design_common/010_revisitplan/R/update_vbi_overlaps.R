# Generate or update a file with VBI locations that overlap the MNE spatial
# samples

# Download VBI coordinates (internally only), which are considered privacy
# sensitive, hence not public
drive_download(
  as_id("1pYvpC58-GnUvIWW96hWElUq1D5O9YCg9"),
  path = file.path(tempdir(), "coordinates.tsv")
)
drive_download(
  as_id("1qbpW73audXrDhFtGkYHUhdtrLUIAfbk_"),
  path = file.path(tempdir(), "coordinates.yml")
)
coordinates_vbi <- read_vc("coordinates", root = tempdir()) %>% as_tibble()

# Generate MNE forest sampling units as polygons ---------------

grts_mh <- read_GRTSmh()
# create a spatial index of the GRTS addresses
grts_mh_index <- tibble(
  id = seq_len(ncell(grts_mh)),
  grts_address = values(grts_mh)[, 1]
) %>%
  filter(!is.na(grts_address))

forest_units_rast <-
  scheme_moco_ps_stratum_sppost_spsamples_sf %>%
  st_drop_geometry() %>%
  mutate(is_forest = str_detect(stratum, "^9|^2180|^rbbppm")) %>%
  filter(is_forest) %>%
  pull(grts_address_final) %>%
  filter_grtsraster_by_address(spatrast = grts_mh, spatrast_index = grts_mh_index)
set.names(forest_units_rast, "grts_address_final")

forest_units_polygon <-
  forest_units_rast %>%
  as.polygons(aggregate = FALSE) %>%
  st_as_sf() %>%
  # to prefer the tibble approach in sf, we need to convert forth and back
  as_tibble() %>%
  # it appears that the CRS is actually retrieved from the tibble, but I don't
  # understand how (so the crs argument below isn't needed)
  st_as_sf(crs = "EPSG:31370", agr = "identity") %>%
  mutate(grts_address_final = as.integer(grts_address_final))

# Filter and write the few coordinates that overlap MNE locations ----------

vbi_buffers <-
  coordinates_vbi %>%
  filter(type_coord == "ingemeten coo") %>%
  filter(vbi_cycle == max(vbi_cycle), .by = plot_id) %>%
  select(plot_id, x, y) %>%
  mutate(plot_id = as.integer(plot_id)) %>%
  st_as_sf(
    coords = c("x", "y"),
    remove = FALSE,
    crs = 31370,
    agr = "identity"
  ) %>%
  st_buffer(18)

vbi_buffers %>%
  st_join(
    forest_units_polygon %>%
      rename(grts_address_overlapped_cell = grts_address_final),
    left = FALSE
  ) %>%
  st_drop_geometry() %>%
  arrange(plot_id, grts_address_overlapped_cell) %>%
  write_vc(
    "vbi_overlaps",
    root = file.path(datapath, "text/raw"),
    sorting = c("plot_id", "grts_address_overlapped_cell"),
    digits = 6,
    strict = FALSE
  )
