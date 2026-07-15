## Evaluating the dataset of legacy watersample points for lentic types

# First run setup chunk
#
# Then run:

load(file.path(datapath, "binary/results/objects_panflpan5.RData"))

legacy_watersamplepoints_lentic <-
  read_vc(
    file = "legacy_watersamplepoints_lentic",
    root = file.path(datapath, "text/raw")
  ) %>%
  as_tibble()

legacy_watersamplepoints_lentic %>%
  # one records appears to have missing coordinates
  filter(!is.na(x), !is.na(y)) %>%
  st_as_sf(coords = c("x", "y"), crs = 31370) %>%
  rename(polygon_id_pt = polygon_id) %>%
  st_join(stratum_grts_spsamples_lentic_sf, left = FALSE) %>%
  st_drop_geometry() %>%
  count(
    is.na(polygon_id_pt),
    polygon_id_pt == polygon_id,
    year(active_in_db_till) > 9000
  )

