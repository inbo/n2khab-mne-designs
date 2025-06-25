# This code is to support an ad-hoc question by a colleague: how many locations
# are involved in the groundwater monitoring, per type?

# First run setup chunk
#
# Then run:

load(file.path(datapath, "binary/results/objects_panflpan5.RData"))


gw_type_grts <-
  scheme_moco_ps_stratum_sppost_spsamples %>%
  filter(str_detect(scheme, "^GW")) %>%
  inner_join(
    n2khab_strata,
    join_by(stratum),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  unnest(sp_poststr_samples) %>%
  add_assessment_data() %>%
  distinct(
    type,
    grts_address,
    grts_address_final
  )

gw_type_grts %>%
  count(type) %>%
  write_csv("type_count_groundwater.csv")


grts_mh <- read_GRTSmh()
# create a spatial index of the GRTS addresses
grts_mh_index <- tibble(
  id = seq_len(ncell(grts_mh)),
  grts_address = values(grts_mh)[, 1]
) %>%
  filter(!is.na(grts_address))

gw_type_grts %>%
  add_point_coords_grts(
    grts_var = "grts_address_final",
    spatrast = grts_mh,
    spatrast_index = grts_mh_index
  ) %>%
  write_sf("gw_type_grts.gpkg")
