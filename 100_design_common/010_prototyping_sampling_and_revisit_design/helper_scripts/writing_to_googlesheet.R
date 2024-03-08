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

domain_scheme_stats %>%
  write_sheet(gs_id, "domain_scheme_stats")

module_domains %>%
  filter(sample_size_predetermined) %>%
  semi_join(domain_stratum_nunits, ., by = "domain") %>%
  inner_join(n2khab_strata, by = "stratum") %>%
  summarize(nunits = sum(nunits), .by = c(domain, type)) %>%
  arrange(type) %>%
  pivot_wider(names_from = domain, values_from = nunits) %>%
  write_sheet(
    ss = gs_id,
    sheet = "domain_type_nunits")

module_domains %>%
  filter(sample_size_predetermined) %>%
  inner_join(domain_stratum_nunits, by = "domain", relationship = "many-to-many") %>%
  inner_join(n2khab_strata, by = "stratum") %>%
  summarize(nunits = sum(nunits), .by = c(module, type)) %>%
  arrange(type) %>%
  pivot_wider(names_from = module, values_from = nunits) %>%
  write_sheet(
    ss = gs_id,
    sheet = "module_type_nunits")

module_domains %>%
  filter(sample_size_predetermined) %>%
  semi_join(scheme_ssf_domain_stratum_nunits, ., by = "domain") %>%
  filter(domain != "Flanders" | !str_detect(scheme, "^HQ")) %>%
  inner_join(n2khab_strata, by = "stratum") %>%
  summarize(nunits = sum(nunits), .by = c(domain, scheme, type)) %>%
  summarize(
    type_count = n(),
    type_nunits_min = min(nunits),
    type_nunits_q1 = quantile(nunits, 0.25),
    type_nunits_q2 = quantile(nunits, 0.5),
    type_nunits_q3 = quantile(nunits, 0.75),
    type_nunits_max = max(nunits),
    .by = c(domain, scheme)
  ) %>%
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


