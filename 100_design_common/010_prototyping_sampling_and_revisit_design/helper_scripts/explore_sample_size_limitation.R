# Exploring sample size limitation results

# First run setup chunk (index.Rmd), then continue


# SETUP -------------------------------------------------------------------

scenario_name <- params$sample_adjustment_scenario

path_ref <- file.path(datapath, "binary/results/objects_panflpan5.RData")
path_new <- file.path(
  datapath,
  str_glue("binary/results/objects_panflpan5_{scenario_name}.RData")
)
path_gpkg <- file.path(datapath, "binary/intermediate/spatial_sampling.gpkg")

prepare_lazy_get(path_ref, new_envir_name = "ref")
prepare_lazy_get(path_new, new_envir_name = scenario_name)

sps_ref <- get("scheme_moco_ps_stratum_sppost_spsamples_sf", envir = ref)
sps_new <- get(
  "scheme_moco_ps_stratum_sppost_spsamples_sf",
  envir = eval(str2lang(scenario_name))
)
ssizes_ref <- get("module_domain_scheme_ps_stratum_sample_size", envir = ref)
ssizes_new <- get(
  "module_domain_scheme_ps_stratum_sample_size",
  envir = eval(str2lang(scenario_name))
)
cal_ref <- get("fag_stratum_grts_calendar", envir = ref)
cal_new <- get(
  "fag_stratum_grts_calendar",
  envir = eval(str2lang(scenario_name))
)


moco_ssizes_new <- get(
  "scheme_moco_ps_dom_stratum_sample_size",
  envir = eval(str2lang(scenario_name))
)
targetsizes_ref <- get("module_domain_scheme_ps_stratum_target_sample_size", envir = ref)
targetsizes_new <- get(
  "module_domain_scheme_ps_stratum_target_sample_size",
  envir = eval(str2lang(scenario_name))
)

type_properties <- get("n2khab_types_expanded_properties", envir = ref)
mhq_scheme_category <- get("mhq_scheme_category", envir = ref)
n2khab_strata <- get("n2khab_strata", envir = ref)
dom_scheme_stratum_nunits <-
  get("submod_dom_scheme_ssf_stratum_nunits", envir = ref) %>%
  distinct(domain, scheme, stratum, nunits)

cell_types <-
  type_properties %>%
  filter(
    str_detect(grts_join_method, "cell"),
    type != "7140_mrd"
  )
non_cell_types <-
  type_properties %>%
  filter(
    !str_detect(grts_join_method, "cell") | type == "7140_mrd"
  )



# COMPARING SAMPLE SIZES --------------------------------------------------

## Total sample sizes per scheme -------------------------------------------

get_total_sample_sizes <- function(df, types = NULL) {
  df1 <- df %>%
    st_drop_geometry() %>%
    distinct(scheme, module_combo_code, stratum, grts_address)
  if (is.null(types)) {
    df1 %>%
      count(scheme, module_combo_code)
  } else {
    df1 %>%
      inner_join(
        n2khab_strata,
        join_by(stratum),
        relationship = "many-to-one",
        unmatched = c("error", "drop")
      ) %>%
      semi_join(types, join_by(type)) %>%
      count(scheme, module_combo_code)
  }
}

get_total_sample_sizes(sps_ref)
get_total_sample_sizes(sps_new)

## Yearly sample sizes per aggregated scheme --------------------------------

aggregate_sample_size(
  ssizes_ref,
  "sp_sample_size_all_panels_stratum",
  mhq_scheme_category
) %>%
  print(n = Inf)

aggregate_sample_size(
  ssizes_new,
  "sp_sample_size_all_panels_stratum",
  mhq_scheme_category
) %>%
  print(n = Inf)


## Stratum sample sizes for cell types --------------------------------

compare_ssizes_per_stratum <- function(df, dfref = ssizes_ref) {
  df %>%
  semi_join(cell_types, join_by(stratum == type)) %>%
  # sum sample sizes over panel sets, limited by nunits
  summarize(
    ssize_stratum_altered = pmin(
      sum(sp_sample_size_all_panels_stratum),
      first(nunits)
    ),
    .by = c(module, domain, scheme, stratum, nunits, spss_stratum_truncated)
  ) %>%
  inner_join(
    dfref %>%
      semi_join(cell_types, join_by(stratum == type)) %>%
      # sum sample sizes over panel sets, limited by nunits
      summarize(
        ssize_stratum = pmin(
          sum(sp_sample_size_all_panels_stratum),
          first(nunits)
        ),
        .by = c(module, domain, scheme, stratum)
      ),
    join_by(module, domain, scheme, stratum),
    relationship = "one-to-one",
    unmatched = "error"
  ) %>%
  filter(!is.na(ssize_stratum_altered))
}

# counting planned vs obtained sample size differences
compare_ssizes_per_stratum(ssizes_new) %>%
  mutate(ssize_differs = ssize_stratum_altered != ssize_stratum) %>%
  count(spss_stratum_truncated, ssize_differs)

# counting obtained sample size differences
compare_ssizes_per_stratum(ssizes_new) %>%
  mutate(ssize_differs = ssize_stratum_altered != ssize_stratum) %>%
  count(ssize_differs)

# quantiles of sample size differences
compare_ssizes_per_stratum(ssizes_new) %>%
  mutate(ssize_diff = ssize_stratum_altered - ssize_stratum) %>%
  pull(ssize_diff) %>%
  quantile(seq(0, 1, 0.1))

# quantiles of relative sample size differences
compare_ssizes_per_stratum(ssizes_new) %>%
  mutate(
    ssize_diff_rel = round(
      (ssize_stratum_altered - ssize_stratum) / ssize_stratum,
      2
    )) %>%
  pull(ssize_diff_rel) %>%
  quantile(seq(0, 1, 0.1))

# investigate unplanned but obtained sample size differences (they are always
# lower): this is the consequence of the lower target sample size ranges in the
# new (smaller) precision pools (i.e. after excluding strata with sample size
# limitation), leading to lower coef for infinite sample size and consequently
# lower FPC-corrected sample size. The diference is limited though.
compare_ssizes_per_stratum(ssizes_new) %>%
  mutate(ssize_differs = ssize_stratum_altered != ssize_stratum) %>%
  filter(!spss_stratum_truncated, ssize_differs)

# graph of relative sample size decrease per stratum, distinguishing planned vs
# unplanned sample size limitation
compare_ssizes_per_stratum(ssizes_new) %>%
  mutate(ssize_diff_rel = (ssize_stratum_altered - ssize_stratum) / ssize_stratum) %>%
  ggplot(aes(x = ssize_diff_rel, colour = spss_stratum_truncated)) +
  geom_density()

# graph of relative sample size decrease per stratum
compare_ssizes_per_stratum(ssizes_new) %>%
  mutate(ssize_diff_rel = (ssize_stratum_altered - ssize_stratum) / ssize_stratum) %>%
  ggplot(aes(x = ssize_diff_rel)) +
  geom_density()

# graph of relative sample size decrease per stratum for the PLANNED & the
# LARGER decreases (groundwater)
compare_ssizes_per_stratum(ssizes_new) %>%
  mutate(
    ssize_diff_rel = (ssize_stratum_altered - ssize_stratum) / ssize_stratum,
    compartment = str_match(scheme, "^(\\w+)_")[, 2],
    stratum_domain = str_c(stratum, "_", domain) %>% fct_rev()
  ) %>%
  select(-scheme) %>%
  filter(!is.na(compartment)) %>%
  filter(compartment == "GW") %>%
  filter(spss_stratum_truncated | abs(ssize_diff_rel) > 0.05) %>%
  distinct() %>%
  ggplot(aes(x = stratum_domain, y = ssize_diff_rel, fill = spss_stratum_truncated)) +
  geom_col() +
  facet_wrap(~compartment) +
  coord_flip()


# comparing and checking unplanned differences in target sample size: apart from
# left-out-types from the densification submodules, differences have to do with
# the different weights that the domains get in each scheme, within the
# densification submodule in pan5; hence this is restricted to the types that
# are densified.

targetsizes_new %>%
  select(
    module,
    domain,
    scheme,
    panel_set,
    stratum,
    nunits,
    targsize_stratum_altered = sp_sample_size_all_panels_stratum,
    spss_stratum_truncated
  ) %>%
  inner_join(
    targetsizes_ref %>%
      select(
        module,
        domain,
        scheme,
        panel_set,
        stratum,
        targsize_stratum = sp_sample_size_all_panels_stratum
      ),
    join_by(module, domain, scheme, panel_set, stratum),
    relationship = "one-to-one",
    unmatched = "error"
  ) %>%
  filter(!spss_stratum_truncated) %>%
  mutate(
    targsize_differs = targsize_stratum_altered != targsize_stratum,
    targsize_diff_rel = (targsize_stratum_altered - targsize_stratum) / targsize_stratum
  ) %>%
  filter(targsize_differs) %>%
  summarize(
    targsize_diff_rel_min = min(targsize_diff_rel),
    targsize_diff_rel_max = max(targsize_diff_rel),
    .by = c(scheme, domain, stratum)
  ) %>%
  arrange(scheme, stratum, domain) %>%
  print(n = Inf)

get(
  "submodule_domain_scheme_ps_designattr",
  envir = eval(str2lang(scenario_name))
) %>%
  select(
    module,
    submodule,
    domain,
    scheme,
    type_count_2 = type_count,
    panel_set,
    ssize_all_2 = sp_sample_size_all_panels,
    ssize_type_2 = sp_sample_size_all_panels_type
  ) %>%
  inner_join(
    ref$submodule_domain_scheme_ps_designattr %>%
      select(
        module,
        submodule,
        domain,
        scheme,
        type_count = type_count,
        panel_set,
        ssize_all = sp_sample_size_all_panels,
        ssize_type = sp_sample_size_all_panels_type
      ),
    join_by(module, submodule, domain, scheme, panel_set),
    relationship = "one-to-one",
    unmatched = "error"
  ) %>%
  filter(ssize_type_2 != ssize_type)


## Total sample size for non-cell types --------------------------------

get_total_sample_sizes(sps_ref, non_cell_types)
get_total_sample_sizes(sps_new, non_cell_types)

# COMPARE FAG CALENDARS ---------------------------------------------------

# LOCEVAL FAGs for cell types currently scheduled in 2025
loceval_2025_ref <-
  cal_ref %>%
  filter(
    str_detect(field_activity_group, "LOCEVAL"),
    year(date_start) < 2026
  ) %>%
  semi_join(cell_types, join_by(stratum == type)) %>%
  select(stratum, grts_address, grts_address_final)

# LOCEVAL FAGs for cell types currently scheduled in 2025 but nowhere in the new
# FAG calendar
cal_new %>%
  filter(str_detect(field_activity_group, "LOCEVAL")) %>%
  semi_join(cell_types, join_by(stratum == type)) %>%
  distinct(stratum, grts_address) %>%
  anti_join(loceval_2025_ref, ., join_by(stratum, grts_address))

# LOCEVAL FAGs for cell types currently scheduled in 2025 but nowhere in the new
# FAG calendar partim GW
loceval_2025_ref_missing_gw <-
  cal_new %>%
  filter(str_detect(field_activity_group, "^GW")) %>%
  semi_join(cell_types, join_by(stratum == type)) %>%
  distinct(stratum, grts_address) %>%
  anti_join(loceval_2025_ref, ., join_by(stratum, grts_address))
loceval_2025_ref_missing_gw
loceval_2025_ref_missing_gw %>%
  count(stratum) %>%
  print(n = Inf)

# LOCEVAL FAGs for cell types scheduled in 2025 in the new FAG calendar but
# absent from current 2025 schedule
cal_new %>%
  filter(
    str_detect(field_activity_group, "LOCEVAL"),
    year(date_start) < 2026
  ) %>%
  semi_join(cell_types, join_by(stratum == type)) %>%
  distinct(stratum, grts_address, grts_address_final) %>%
  anti_join(loceval_2025_ref, join_by(stratum, grts_address))

# LOCEVAL FAGs for cell types currently scheduled in 2025 AND present in the new
# FAG calendar: counting the corresponding LOCEVALs per year in the new FAG
# calendar
cal_new %>%
  filter(str_detect(field_activity_group, "LOCEVAL")) %>%
  semi_join(cell_types, join_by(stratum == type)) %>%
  semi_join(loceval_2025_ref, join_by(stratum, grts_address)) %>%
  mutate(year_start = year(date_start)) %>%
  count(year_start)




# COMPARE MHQ OVERLAP -----------------------------------------------------

# we can see that the number of MHQ units in the sample decreases, although the
# relative proportion slightly increases

show_mhq_overlap <- function(df) {
  df %>%
    st_drop_geometry() %>%
    semi_join(cell_types, join_by(stratum == type)) %>%
    mutate(compartment = str_match(scheme, "^(\\w+)_")[, 2]) %>%
    filter(!is.na(compartment)) %>%
    distinct(compartment, stratum, grts_address, assessed_in_field) %>%
    summarize(
      count_is_mhq = sum(assessed_in_field),
      proportion_is_mhq = round(sum(assessed_in_field) / n(), 2),
      .by = compartment
    )
}

show_mhq_overlap(sps_ref)
show_mhq_overlap(sps_new)



# INVESTIGATE INTEGRATED SAMPLE PROPORTIONS PER DOMAIN -----------------------

# some overshooting of the sample proportion limit is possible due to:
# - 'independency' of densification in included domains
# - (few cases:) increasing zero- or one-sized strata to two units at Flemish
# level


get_integrated_ssizes_per_domain <- function(df,
                                             max_sample_prop,
                                             regex_compartment = "^GW") {
  df %>%
    st_drop_geometry() %>%
    distinct(scheme, sp_poststratum, stratum, grts_address) %>%
    count(scheme, sp_poststratum, stratum) %>%
    filter(str_detect(sp_poststratum, "^BE")) %>%
    rename(domain = sp_poststratum) %>%
    bind_rows(
      df %>%
        st_drop_geometry() %>%
        distinct(scheme, sp_poststratum, stratum, grts_address) %>%
        count(scheme, stratum) %>%
        mutate(domain = "Flanders")
    ) %>%
    semi_join(cell_types, join_by(stratum == type)) %>%
    mutate(
      domain = factor(domain, levels = levels(dom_scheme_stratum_nunits$domain))
    ) %>%
    filter(str_detect(scheme, regex_compartment)) %>%
    inner_join(
      dom_scheme_stratum_nunits,
      join_by(scheme, domain, stratum),
      relationship = "one-to-one",
      unmatched = c("error", "drop")
    ) %>%
    mutate(sample_prop = round(n / nunits, 2)) %>%
    filter(sample_prop > max_sample_prop)
}

get_integrated_ssizes_per_domain(sps_new, 0.2, "^GW")

# checking & comparing
ssizes_new %>%
  filter(scheme == "GW_03.3", stratum == "7140_base") %>%
  select(
    module,
    domain,
    scheme,
    panel_set,
    stratum,
    nunits,
    sp_sample_size_all_panels_stratum,
    spss_stratum_truncated
  )
moco_ssizes_new %>%
  filter(scheme == "GW_03.3", stratum == "7140_base") %>%
  select(
    module_combo_code,
    domain,
    scheme,
    panel_set,
    stratum,
    sp_sample_size_all_panels_stratum
  )


# WRITING GPKG LAYERS -----------------------------------------------------

write_to_gpkg_layer <- function(df, layername, regex_compartment = "^GW", types = NULL) {
  df1 <- df %>% filter(str_detect(scheme, regex_compartment))
  if (is.null(types)) {
    df1 <-
      df1 %>%
      semi_join(cell_types, join_by(stratum == type))
  } else {
    df1 <-
      df1 %>%
      inner_join(
        n2khab_strata,
        join_by(stratum),
        relationship = "many-to-one",
        unmatched = c("error", "drop")
      ) %>%
      semi_join(types, join_by(type))
  }
  df1 %>%
    distinct(scheme, module_combo_code, stratum, grts_address, grts_address_final, geometry) %>%
    mutate(compartment = str_match(scheme, "^(\\w+)_")[, 2]) %>%
    select(compartment, stratum, grts_address, grts_address_final) %>%
    group_by(compartment, grts_address, grts_address_final, geometry) %>%
    summarize(
      strata = str_flatten(sort(unique(stratum)), collapse = " | "),
      n_strata = length(unique(stratum)),
      .groups = "drop"
    ) %>%
    write_sf(path_gpkg, layer = layername, delete_layer = TRUE)
}

write_to_gpkg_layer(sps_ref, "sps_ref")
write_to_gpkg_layer(sps_new, str_c("GW_", scenario_name))
write_to_gpkg_layer(
  sps_new,
  str_c("GW_NONCELL_", scenario_name),
  types = non_cell_types
)
write_to_gpkg_layer(sps_new, str_c("SOIL_", scenario_name), "^SOIL")
