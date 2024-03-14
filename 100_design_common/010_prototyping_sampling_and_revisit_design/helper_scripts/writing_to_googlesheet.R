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
    sheet = "type_properties")

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
# - n2khab_strata
# - scheme_ssf_domain_stratum_nunits
# - non_core_types_per_domain_and_compartment

module_domains %>%
  filter(sample_size_predetermined) %>%
  semi_join(domain_type_nunits, ., by = "domain") %>%
  arrange(type) %>%
  pivot_wider(names_from = domain, values_from = nunits) %>%
  write_sheet(
    ss = gs_id,
    sheet = "domain_type_nunits")

module_domains %>%
  filter(sample_size_predetermined) %>%
  inner_join(domain_type_nunits, by = "domain", relationship = "many-to-many") %>%
  summarize(nunits = sum(nunits), .by = c(module, type)) %>%
  arrange(type) %>%
  pivot_wider(names_from = module, values_from = nunits) %>%
  write_sheet(
    ss = gs_id,
    sheet = "module_type_nunits")

module_domains %>%
  filter(sample_size_predetermined) %>%
  semi_join(domain_scheme_typestats, ., by = "domain") %>%
  filter(domain != "Flanders" | !str_detect(scheme, "^HQ")) %>%
  arrange(domain, scheme) %>%
  write_sheet(
    ss = gs_id,
    sheet = "domain_scheme_stats")


module_domains %>%
  filter(sample_size_predetermined) %>%
  inner_join(
    scheme_ssf_domain_stratum_nunits,
    by = "domain",
    relationship = "many-to-many"
  ) %>%
  filter(domain != "Flanders" | !str_detect(scheme, "^HQ")) %>%
  inner_join(n2khab_strata, by = "stratum") %>%
  summarize(nunits = sum(nunits), .by = c(module, scheme, type)) %>%
  arrange(scheme, type) %>%
  pivot_wider(names_from = module, values_from = nunits) %>%
  write_sheet(
    ss = gs_id,
    sheet = "scheme_type_nunits_per_module")


module_domains %>%
  filter(sample_size_predetermined) %>%
  semi_join(scheme_ssf_domain_stratum_nunits, ., by = "domain") %>%
  filter(domain != "Flanders" | !str_detect(scheme, "^HQ")) %>%
  inner_join(n2khab_strata, by = "stratum") %>%
  summarize(nunits = sum(nunits), .by = c(domain, scheme, type)) %>%
  arrange(scheme, type) %>%
  pivot_wider(names_from = domain, values_from = nunits) %>%
  write_sheet(
    ss = gs_id,
    sheet = "scheme_type_nunits_per_domain")

non_core_types_per_domain_and_compartment %>%
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
    sheet = "non_core_types_per_dom&comp")

