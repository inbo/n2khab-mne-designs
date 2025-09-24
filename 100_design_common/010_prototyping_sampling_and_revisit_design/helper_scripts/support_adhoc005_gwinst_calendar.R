# Generating a GeoPackage with the calendar for GWINST FAGs (loosely inspired by
# support_adhoc001_typecount_gw.R script)

# First run setup chunk
#
# Then run the code (see support script) needed to create
# scheme_moco_ps_stratum_targetpanel_spsamples

# check that this exists:
glimpse(scheme_moco_ps_stratum_targetpanel_spsamples)

gwinst_planning <-
  fag_stratum_grts_calendar %>%
  filter(str_detect(field_activity_group, "^GWINST")) %>%
  unnest(scheme_moco_ps) %>%
  select(
    scheme,
    module_combo_code,
    panel_set,
    stratum,
    grts_address,
    grts_address_final,
    starts_with("date"),
    field_activity_group,
  ) %>%
  # adding location attributes
  inner_join(
    scheme_moco_ps_stratum_targetpanel_spsamples %>%
      select(
        scheme,
        module_combo_code,
        panel_set,
        stratum,
        grts_join_method,
        grts_address,
        grts_address_final,
        # retaining 3 cols that drive subsampling location(s) in the unit:
        is_forest,
        in_mhq_samples,
        last_type_assessment_in_field,
        domain_part,
        targetpanel
      ) %>%
      # deduplicating 7220:
      distinct(),
    join_by(
      scheme,
      module_combo_code,
      panel_set,
      stratum,
      grts_address,
      grts_address_final
    ),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  relocate(grts_address_final:domain_part, .after = grts_address) %>%
  select(-module_combo_code) %>%
  summarize(
    date_start_earliest_visit = min(date_start),
    date_end_earliest_visit = min(date_end),
    .by = !starts_with("date")
  ) %>%
  mutate(scheme_ps_targetpanel = str_glue(
    "{ scheme }:PS{ panel_set }{ targetpanel }"
  )) %>%
  select(-scheme, -panel_set, -targetpanel) %>%
  nest(scheme_ps_targetpanels = scheme_ps_targetpanel) %>%
  mutate(
    scheme_ps_targetpanels = map_chr(scheme_ps_targetpanels, \(df) {
      str_flatten(
        unique(df$scheme_ps_targetpanel),
        collapse = " | "
      )
    }) %>%
      factor()
  ) %>%
  arrange(date_start_earliest_visit, stratum, grts_address) %>%
  relocate(scheme_ps_targetpanels) %>%
  relocate(starts_with("date"), .after = scheme_ps_targetpanels)

# write as point locations

grts_mh <- read_GRTSmh()
# create a spatial index of the GRTS addresses
grts_mh_index <- tibble(
  id = seq_len(ncell(grts_mh)),
  grts_address = values(grts_mh)[, 1]
) %>%
  filter(!is.na(grts_address))

gpkg_path <- file.path(datapath, "binary/results/gwinst_planning.gpkg")

gwinst_planning %>%
  add_point_coords_grts(
    grts_var = "grts_address_final",
    spatrast = grts_mh,
    spatrast_index = grts_mh_index
  ) %>%
  write_sf(
    gpkg_path,
    layer = "gwinst_planning",
    delete_dsn = TRUE
  )


