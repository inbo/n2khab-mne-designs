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

# Filter and write the few coordinates that overlap MNE locations
coordinates_vbi %>%
  filter(type_coord == "ingemeten coo") %>%
  filter(vbi_cycle == max(vbi_cycle), .by = plot_id) %>%
  select(plot_id, x, y) %>%
  mutate(
    plot_id = as.integer(plot_id),
    grts_address = extract(grts_mh, select(., x, y)) %>%
      pull(GRTSmaster_habitats)
  ) %>%
  semi_join(
    scheme_moco_ps_stratum_sppost_spsamples_sf %>%
      st_drop_geometry() %>%
      mutate(is_forest = str_detect(stratum, "^9|^2180|^rbbppm")) %>%
      filter(is_forest),
    join_by(grts_address == grts_address_final),
  ) %>%
  arrange(plot_id, grts_address) %>%
  write_vc(
    "vbi_overlaps",
    root = file.path(datapath, "text/raw"),
    sorting = c("plot_id", "grts_address"),
    digits = 6
  )

