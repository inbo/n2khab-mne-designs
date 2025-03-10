# First run setup chunk
#
# Then run:

load(file.path(datapath, "binary/results/objects_panflpan5.RData"))

# # Checking distribution of sp_poststratum containing "Flanders"
# scheme_moco_ps_stratum_sppost_spsamples_sf %>%
#   st_drop_geometry() %>%
#   distinct(stratum, sp_poststratum) %>%
#   filter(str_detect(sp_poststratum, "Flanders")) %>%
#   count(stratum) %>%
#   # no strata have the mix of both:
#   filter(n > 1)

spsamples_sf_vmm <-
  scheme_moco_ps_stratum_sppost_spsamples_sf %>%
  filter(str_detect(scheme, "^(GW|SURF|SOIL)")) %>%
  select(-grts_address_final) %>%
  # join date intervals of target FAGs, for the locations in the sample
  left_join(
    scheme_moco_ps_spsubset_targetfag_stratum_sppost_spsamples_calendar %>%
      arrange(date_interval) %>%
      summarize(
        date_intervals = str_flatten(
          date_interval %>% unique(),
          collapse = ", "
        ),
        .by = c(
          scheme,
          module_combo_code,
          panel_split,
          sp_poststratum,
          stratum,
          grts_address
        )
      ),
    join_by(
      scheme,
      module_combo_code,
      panel_split,
      sp_poststratum,
      stratum,
      grts_address
    ),
    relationship = "one-to-one"
  ) %>%
  select(-module_combo_code, -panel_split, -typelevel_certain, -assessed_in_field) %>%
  mutate(date_intervals = factor(date_intervals)) %>%
  # convert 'Flanders' to 'Flanders_remainder' (since 'Flanders' is for strata
  # that are only outside of the 5 SACs):
  mutate(sp_poststratum = fct_recode(
    sp_poststratum,
    Flanders_remainder = "Flanders"
  )) %>%
  rename(area = sp_poststratum) %>%
  # only core schemes
  semi_join(
    module_domain_schemes %>%
      distinct(scheme, is_core_scheme) %>%
      filter(is_core_scheme),
    join_by(scheme)
  ) %>%
  # no SURF schemes
  filter(!str_detect(scheme, "^SURF")) %>%
  # adding scheme metadata
  inner_join(
    read_schemes(lang = "nl") %>%
      select(scheme, scheme_name, scheme_shortname),
    join_by(scheme),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  relocate(starts_with("scheme")) %>%
  # adding type metadata
  inner_join(
    n2khab_strata,
    join_by(stratum),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  inner_join(
    read_types(lang = "nl") %>%
      select(
        type,
        typelevel,
        main_type,
        type_name,
        type_shortname,
        typeclass,
        typeclass_name,
        hydr_class
      ),
    join_by(type),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  # drop aquatic types
  filter(hydr_class != "HC3") %>%
  select(-hydr_class) %>%
  relocate(geometry, .after = last_col())

saveRDS(
  spsamples_sf_vmm,
  file.path(datapath, "binary/results/spsamples_sf_vmm.rds")
)

write_sf(
  spsamples_sf_vmm,
  file.path(datapath, "binary/results/spsamples_sf_vmm.gpkg")
)
