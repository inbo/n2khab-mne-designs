# Generating a GeoPackage with the calendar for LOCEVAL FAGs (loosely inspired
# by support_adhoc009_samplesizes_per_sacprovince.R script)

# First run setup chunk
#
# Then run the code (see support script) needed to create:
# - scheme_moco_ps_stratum_targetpanel_spsamples
# - units_cell_polygon_stratum_attribs

# check that this exists:
glimpse(scheme_moco_ps_stratum_targetpanel_spsamples)
glimpse(units_cell_polygon_stratum_attribs)

loceval_planning <-
  fag_stratum_grts_calendar %>%
  filter(str_detect(field_activity_group, "LOCEVAL")) %>%
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
        last_type_assessment,
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
      factor(),
    wait_watersurface = str_detect(stratum, "^31|^2190_a"),
    wait_3260 = stratum == "3260",
    wait_7220 = str_detect(stratum, "^7220"),
    wait_floating = stratum == "7140_mrd",
    wait_any = if_any(starts_with("wait"))
  ) %>%
  relocate(wait_any, .before = wait_watersurface) %>%
  arrange(
    date_start,
    wait_watersurface,
    wait_3260,
    wait_7220,
    wait_floating,
    wait_any,
    stratum,
    grts_address
  ) %>%
  relocate(scheme_ps_targetpanels) %>%
  relocate(starts_with("date"), .after = scheme_ps_targetpanels)

# subset of years 2029-2030 (this approximates locations to be visited in 2026
# after resetting the revisit design to the lowest GRTS addresses)

loceval_planning_2930 <-
  loceval_planning %>%
  filter(year(date_start) %in% c(2029, 2030))

# write as point locations

grts_mh <- read_GRTSmh()
# create a spatial index of the GRTS addresses
grts_mh_index <- tibble(
  id = seq_len(ncell(grts_mh)),
  grts_address = values(grts_mh)[, 1]
) %>%
  filter(!is.na(grts_address))

gpkg_path <- file.path(datapath, "binary/results/loceval_planning.gpkg")

loceval_planning %>%
  add_point_coords_grts(
    grts_var = "grts_address_final",
    spatrast = grts_mh,
    spatrast_index = grts_mh_index
  ) %>%
  write_sf(
    gpkg_path,
    layer = "loceval_planning",
    delete_dsn = TRUE
  )

loceval_planning_2930 %>%
  add_point_coords_grts(
    grts_var = "grts_address_final",
    spatrast = grts_mh,
    spatrast_index = grts_mh_index
  ) %>%
  write_sf(
    gpkg_path,
    layer = "loceval_planning_2930",
    delete_layer = TRUE
  )

units_cell_polygon_stratum_attribs %>%
  write_sf(
    gpkg_path,
    layer = "units_cell_polygon_stratum_attribs",
    delete_layer = TRUE
  )

