# Explore spatial sample sizes

# First run setup chunk
#
# Then run:

load(file.path(datapath, "binary/results/objects_panflpan5.RData"))

## Explore aquatic sample sizes in MNE and MHQ-Flanders

module_domain_scheme_ps_stratum_sample_size %>%
  inner_join(
    n2khab_types_expanded_properties %>%
      mutate(is_strictly_aquatic = hydr_class == "HC3") %>%
      select(type, is_strictly_aquatic),
    join_by(type),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  filter(
    str_detect(module, "effectmon"),
    is_strictly_aquatic,
    !is.na(sp_sample_size_all_panels_type),
    !str_detect(scheme, "^HQ")
  ) %>%
  summarize(
    sp_sample_size_all_panels_type = sum(sp_sample_size_all_panels_stratum) %>% round,
    .by = c(type, scheme, panel_set)
  ) %>%
  arrange(type, scheme) %>%
  print(n = Inf)

mhq_samples %>%
  add_col_is_strictly_aquatic(n2khab_strata, n2khab_types_expanded_properties) %>%
  filter(is_strictly_aquatic) %>%
  inner_join(n2khab_strata, join_by(stratum)) %>%
  count(type)
