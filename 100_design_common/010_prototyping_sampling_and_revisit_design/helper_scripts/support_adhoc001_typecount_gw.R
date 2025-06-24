# This code is to support an ad-hoc question by a colleague: how many locations
# are involved in the groundwater monitoring, per type?

# First run setup chunk
#
# Then run:

load(file.path(datapath, "binary/results/objects_panflpan5.RData"))


scheme_moco_ps_stratum_sppost_spsamples %>%
  filter(str_detect(scheme, "^GW")) %>%
  inner_join(
    n2khab_strata,
    join_by(stratum),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  unnest(sp_poststr_samples) %>%
  select(
    -stratum,
    -scheme,
    -module_combo_code,
    -panel_set,
    -sp_poststratum,
    -sample_status
  ) %>%
  distinct() %>%
  count(type) %>%
  write_csv("type_count_groundwater.csv")
