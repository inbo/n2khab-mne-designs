# This code is to support intermediate discussions and not to be considered part
# of the workflow. Just like the other helper scripts.

if (Sys.getenv("GARGLE_OAUTH_EMAIL") != "") {
  options(gargle_oauth_email = Sys.getenv("GARGLE_OAUTH_EMAIL"))
}
if (Sys.getenv("GARGLE_OAUTH_CACHE") != "") {
  options(gargle_oauth_cache = Sys.getenv("GARGLE_OAUTH_CACHE"))
}

library(googlesheets4)

gs_id <- "1vqxRmWVuc39HCF15K6xZhJEa5ls7Z7uF_xsEr_swQdk"

# Below code requires availability of the following objects:
# - n2khab_types_expanded_properties
# - schemes
# - non_core_types

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
# - module_domain_scheme_stratum_sample_size

module_domain_scheme_designattr %>%
  select(
    module,
    domain,
    scheme,
    cycle_duration_y,
    type_count,
    sp_sample_size_all_panels
  ) %>%
  mutate(
    yearly_sample_size = round(sp_sample_size_all_panels / cycle_duration_y),
    sp_sample_size_all_panels = round(sp_sample_size_all_panels)
  ) %>%
  write_sheet(
    ss = gs_id,
    sheet = "mod_dom_scheme_design_spatial_before_FPC_redistrib"
  )

module_domain_scheme_stratum_sample_size %>%
  distinct(
    module,
    domain,
    scheme,
    cycle_duration_y,
    type_count,
    sp_sample_size_all_panels
  ) %>%
  mutate(yearly_sample_size = round(sp_sample_size_all_panels / cycle_duration_y)) %>%
  write_sheet(
    ss = gs_id,
    sheet = "mod_dom_scheme_design_spatial_after_FPC_redistrib"
  )

module_domain_scheme_stratum_sample_size %>%
  summarize(
    nunits = sum(nunits),
    .by = c(
      module,
      domain,
      scheme,
      cycle_duration_y,
      type,
      sp_sample_size_all_panels_type
    )
  ) %>%
  mutate(
    yearly_sample_size = round(sp_sample_size_all_panels_type / cycle_duration_y, 1),
    fraction_sampled = sp_sample_size_all_panels_type / nunits
  ) %>%
  write_sheet(
    ss = gs_id,
    sheet = "mod_dom_scheme_type_sample_sizes"
  )

module_domain_scheme_stratum_sample_size %>%
  select(
    module,
    domain,
    scheme,
    cycle_duration_y,
    type,
    stratum,
    sp_sample_size_all_panels_stratum,
    nunits
  ) %>%
  mutate(
    yearly_sample_size = round(sp_sample_size_all_panels_stratum / cycle_duration_y, 1),
    fraction_sampled = sp_sample_size_all_panels_stratum / nunits
  ) %>%
  write_sheet(
    ss = gs_id,
    sheet = "mod_dom_scheme_stratum_sample_sizes"
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



# Below code requires availability of:
# - mod_scheme_actseq_fag
# - compartment_paneldesign_fags

mod_scheme_actseq_fag %>%
  mutate(dummy = "X") %>%
  pivot_wider(names_from = module, values_from = dummy) %>%
  arrange(scheme, is_core_scheme, activity_sequence, rank) %>%
  write_sheet(
    ss = gs_id,
    sheet = "mod_scheme_actseq_fag"
  )

compartment_paneldesign_fags %>%
  write_sheet(
    ss = gs_id,
    sheet = "compartment_paneldesign_FAGs"
  )



# Below code requires availability of:
# - scheme_domain_fag_panel_calendar
# - scheme_domain_fag_stratum_spsamples_calendar
# This code was tailored specifically for module pan_effectmon_flanders !!

scheme_domain_fag_panel_calendar %>%
  select(domain, field_activity_group, panel, date_interval) %>%
  summarize(
    panels = str_c(panel, collapse = "\n"),
    .by = c(domain, field_activity_group, date_interval)
  ) %>%
  pivot_wider(names_from = field_activity_group, values_from = panels) %>%
  arrange(date_interval) %>%
  mutate(date_interval = as.character(date_interval)) %>%
  write_sheet(
    ss = gs_id,
    sheet = "dom_panel_calendar_PAN_PHASE_1_FLANDERS"
  )

scheme_domain_fag_stratum_spsamples_calendar %>%
  distinct(domain, field_activity_group, spsunit_groups, date_interval) %>%
  summarize(
    spsunit_groups = str_c(spsunit_groups, collapse = "\n"),
    .by = c(domain, field_activity_group, date_interval)
  ) %>%
  pivot_wider(names_from = field_activity_group, values_from = spsunit_groups) %>%
  arrange(date_interval) %>%
  mutate(date_interval = as.character(date_interval)) %>%
  write_sheet(
    ss = gs_id,
    sheet = "dom_spsunitgroup_calendar_PAN_PHASE_1_FLANDERS"
  )





# Below code requires availability of:
# - scheme_moco_ps_stratum_sppost_genericpanelrelations
# - scheme_moco_ps_spsubset_fag_stratum_sppost_spsamples_calendar
# - fag_stratum_grts_calendar

scheme_moco_ps_stratum_sppost_genericpanelrelations %>%
  distinct(scheme, module_combo_code, panel_split, generic_panels) %>%
  unnest(generic_panels) %>%
  rename(location_set = id) %>%
  rename_with(
    \(x) str_replace(x, "generic", "panelnr_")
  ) %>%
  rename_with(
    \(x) str_replace(x, "autopanels", "panels_autocontinuous_meas")
  ) %>%
  rename_with(
    \(x) str_replace(x, "fastalignpanels", "panels_gwgaugeinstall")
  ) %>%
  pivot_longer(cols = starts_with("panelnr"), names_to = "generic", values_to = "panelnr") %>%
  mutate(nr = str_extract(generic, "\\d+") %>% as.integer()) %>%
  arrange(desc(nr)) %>%
  select(-nr) %>%
  pivot_wider(names_from = generic, values_from = panelnr) %>%
  write_sheet(
    ss = gs_id,
    sheet = "generic_panel_relations"
  )

scheme_moco_ps_spsubset_fag_stratum_sppost_spsamples_calendar %>%
  # don't include ADHOC FAGs:
  filter(notation_paneldesign != "1panelof1m(PUR)") %>%
  count(
    scheme,
    module_combo_code,
    panel_split,
    in_aquatic_subset,
    notation_paneldesign,
    panel,
    date_start,
    date_end,
    date_interval
  ) %>%
  mutate(panel_n = str_c("(", n, ") ", panel)) %>%
  summarize(
    panels_n = str_c(panel_n, collapse = "\n"),
    .by = c(
      notation_paneldesign,
      date_start,
      date_end,
      date_interval
    )
  ) %>%
  pivot_wider(names_from = notation_paneldesign, values_from = panels_n) %>%
  arrange(date_start, date_end) %>%
  select(-date_start, -date_end) %>%
  mutate(date_interval = as.character(date_interval)) %>%
  write_sheet(
    ss = gs_id,
    sheet = "panel_calendar"
  )

fag_stratum_grts_calendar %>%
  count(date_interval, rank, field_activity_group) %>%
  pivot_wider(
    names_from = field_activity_group,
    values_from = n,
    names_sort = TRUE
  ) %>%
  arrange(date_interval, rank) %>%
  mutate(date_interval = as.character(date_interval)) %>%
  write_sheet(
    ss = gs_id,
    sheet = "FAG_calendar"
  )
