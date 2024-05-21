# This code is to support intermediate discussions and not to be considered part
# of the workflow. Just like the other helper scripts.

# This code requires availability of the following objects:
# - n2khab_types_expanded_properties
# - schemes
# - non_core_types

if (Sys.getenv("GARGLE_OAUTH_EMAIL") != "") {
  options(gargle_oauth_email = Sys.getenv("GARGLE_OAUTH_EMAIL"))
}
if (Sys.getenv("GARGLE_OAUTH_CACHE") != "") {
  options(gargle_oauth_cache = Sys.getenv("GARGLE_OAUTH_CACHE"))
}

library(googlesheets4)

gs_id <- "1vqxRmWVuc39HCF15K6xZhJEa5ls7Z7uF_xsEr_swQdk"
n2khab_types_expanded_properties %>%
  select(type, grts_join_method, sample_support) %>%
  inner_join(
    read_types(lang = lang) %>%
      select(type, type_shortname),
    by = "type"
  ) %>%
  relocate(type_shortname, .after = type) %>%
  write_sheet(
    ss = gs_id,
    sheet = "type_properties"
  )

schemes %>%
  select(-spatial_restriction) %>%
  inner_join(
    read_schemes(lang = lang) %>%
      select(
        scheme,
        # attribute_1,
        attribute_1_name,
        # attribute_2,
        attribute_2_name
      ),
    by = "scheme"
  ) %>%
  relocate(attribute_3, tag_1, .after = last_col()) %>%
  write_sheet(gs_id, "scheme_properties")

non_core_types %>%
  write_sheet(gs_id, "non_core_types")

# following is no longer used in the gsheet:
domain_scheme_stats %>%
  write_sheet(gs_id, "domain_scheme_stats")

# Below code requires the availability of:
# - module_domains
# - domain_type_nunits
# - targetpops
# - n2khab_strata
# - mod_dom_scheme_ssf_stratum_nunits
# - non_core_types_per_module_and_compartment

module_domains %>%
  filter(sample_size_predetermined) %>%
  semi_join(domain_type_nunits, ., by = "domain") %>%
  arrange(type) %>%
  semi_join(targetpops %>% distinct(type), by = "type") %>%
  pivot_wider(names_from = domain, values_from = nunits) %>%
  write_sheet(
    ss = gs_id,
    sheet = "domain_type_nunits"
  )

module_targetpops %>%
  distinct(module, type) %>%
  inner_join(
    module_domains %>%
      filter(sample_size_predetermined),
    by = "module",
    relationship = "many-to-many"
  ) %>%
  inner_join(domain_type_nunits, by = c("domain", "type")) %>%
  summarize(nunits = sum(nunits), .by = c(module, type)) %>%
  arrange(type) %>%
  pivot_wider(names_from = module, values_from = nunits) %>%
  write_sheet(
    ss = gs_id,
    sheet = "module_type_nunits"
  )

module_domains %>%
  filter(sample_size_predetermined) %>%
  semi_join(module_domain_scheme_typestats, ., by = "domain") %>%
  arrange(module, domain, scheme) %>%
  write_sheet(
    ss = gs_id,
    sheet = "module_domain_scheme_stats"
  )

module_domains %>%
  filter(sample_size_predetermined) %>%
  semi_join(mod_dom_scheme_ssf_stratum_nunits, ., by = "domain") %>%
  inner_join(n2khab_strata, by = "stratum") %>%
  summarize(nunits = sum(nunits), .by = c(module, scheme, type)) %>%
  arrange(scheme, type) %>%
  pivot_wider(names_from = module, values_from = nunits) %>%
  write_sheet(
    ss = gs_id,
    sheet = "scheme_type_nunits_per_module"
  )

module_domains %>%
  filter(sample_size_predetermined) %>%
  semi_join(mod_dom_scheme_ssf_stratum_nunits, ., by = "domain") %>%
  inner_join(n2khab_strata, by = "stratum") %>%
  summarize(nunits = sum(nunits), .by = c(module, domain, scheme, type)) %>%
  arrange(module, scheme, type) %>%
  pivot_wider(names_from = domain, values_from = nunits) %>%
  write_sheet(
    ss = gs_id,
    sheet = "scheme_type_nunits_per_mod&dom"
  )

non_core_types_per_module_and_compartment %>%
  inner_join(
    read_types(lang = lang) %>%
      select(type, type_shortname),
    by = "type"
  ) %>%
  inner_join(
    read_scheme_types(lang = lang) %>%
      select(-typegroup_name) %>%
      mutate(group_size = n(), .by = c(scheme, typegroup)),
    by = c("scheme", "type")
  ) %>%
  write_sheet(
    ss = gs_id,
    sheet = "non_core_types_per_mod&comp"
  )




# Below code requires availability of:
# - module_domain_scheme_designattr
# - mhq_mod_dom_type_no_sample

module_domain_scheme_designattr %>%
  select(module, domain, scheme, cycle_duration_y, type_count, sp_sample_size_all_panels) %>%
  mutate(yearly_sample_size = round(sp_sample_size_all_panels / cycle_duration_y)) %>%
  write_sheet(
    ss = gs_id,
    sheet = "module_domain_scheme_design_spatial"
  )

module_domain_scheme_designattr %>%
  distinct(pick(module, scheme, targetvar_temporal_resolution:panel_count)) %>%
  mutate(across(where(is.period), as.character)) %>%
  arrange(module, scheme) %>%
  write_sheet(
    ss = gs_id,
    sheet = "mod_scheme_properties_temporal&revisit"
  )

mhq_mod_dom_type_no_sample %>%
  pivot_wider(names_from = domain, values_from = nunits) %>%
  arrange(module, type) %>%
  write_sheet(
    ss = gs_id,
    sheet = "MHQ_mod_dom_type_NOTSAMPLED_nunits"
  )
