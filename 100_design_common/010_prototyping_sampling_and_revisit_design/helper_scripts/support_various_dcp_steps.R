# This code is to support ad-hoc steps and building blocks in making a data
# collection plan and not to be considered part of the workflow. Just like the
# other helper scripts.

# This code requires availability of the following objects:
# - scheme_moco_ps_stratum_sppost_spsamples_sf

# First run setup chunk
#
# Then run:

load(file.path(datapath, "binary/results/objects_panflpan5.RData"))

spsamples_gw_sf_panflpan5 <-
  scheme_moco_ps_stratum_sppost_spsamples_sf %>%
  filter(str_detect(scheme, "^(GW)")) %>%
  distinct(stratum, grts_address, geometry) %>%
  # adding type metadata
  inner_join(
    n2khab_strata,
    join_by(stratum),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  inner_join(
    read_types() %>%
      select(
        type,
        hydr_class,
        hydr_class_shortname
      ),
    join_by(type),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  select(-type) %>%
  relocate(geometry, .after = last_col())

saveRDS(
  spsamples_gw_sf_panflpan5,
  file.path(datapath, "binary/intermediate/spsamples_gw_sf_panflpan5.rds")
)

write_sf(
  spsamples_gw_sf_panflpan5,
  file.path(datapath, "binary/intermediate/spsamples_gw_sf_panflpan5.gpkg")
)
