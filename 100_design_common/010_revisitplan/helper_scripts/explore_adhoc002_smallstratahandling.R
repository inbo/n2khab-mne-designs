# comparing different ways of 'rescuing' small strata
#
# .chain refers to the code chunk where
# module_domain_scheme_ps_stratum_sample_size is rewritten, where the diff = ...
# line in mutate() needs to be uncommented and sp_sample_size_all_panels_stratum
# must be replaced by sp_sample_size_all_panels_stratum_2

.chain %>%
  filter(diff != 0) %>%
  summarize(
    ssize_increase = sum(diff),
    .by = c(module, scheme, panel_set)
  ) %>%
  arrange(module, scheme) %>%
  print(n = Inf)



# (
#   sp_sample_size_all_panels_stratum %in% 0:1 &
#     sp_sample_size_all_panels_stratum < nunits &
#     (domain == "Flanders" | !is_watersurface & panel_set == 1)
# ) |
#   (
#     is_watersurface &
#       domain != "Flanders" &
#       panel_set == 1 &
#       sp_sample_size_all_panels_type %in% 0:1 &
#       (nunits_cum <= 2 | nunits_largest)
#   ),

# A tibble: 9 × 4
# module                        scheme           panel_set ssize_increase
# <fct>                         <fct>                <int>          <int>
#   1 pan_effectmon_flanders        GW_03.3                  1              1
# 2 pan_effectmon_flanders        GW_03.3                  2              7
# 3 pan_effectmon_flanders        SOIL_03.2                1              1
# 4 pan_effectmon_flanders        SOIL_03.2                2              2
# 5 pan_effectmon_flanders        SURF_03.4_lentic         2              1
# 6 pan_effectmon_sac5_custommeas GW_03.3                  1             26
# 7 pan_effectmon_sac5_custommeas SOIL_03.2                1             17
# 8 pan_effectmon_sac5_custommeas HQ3110                   1              1
# 9 pan_effectmon_sac5_custommeas HQ3140                   1              1
#
.chain %>%
  filter(diff != 0) -> orig





#
# (
# sp_sample_size_all_panels_stratum %in% 0:1 &
#   sp_sample_size_all_panels_stratum < nunits &
#   (domain == "Flanders" | panel_set == 1)
# )
#
# A tibble: 13 × 4
# module                        scheme           panel_set ssize_increase
# <fct>                         <fct>                <int>          <int>
#   1 pan_effectmon_flanders        GW_03.3                  1              1
# 2 pan_effectmon_flanders        GW_03.3                  2              7
# 3 pan_effectmon_flanders        SOIL_03.2                1              1
# 4 pan_effectmon_flanders        SOIL_03.2                2              2
# 5 pan_effectmon_flanders        SURF_03.4_lentic         2              1
# 6 pan_effectmon_sac5_custommeas GW_03.3                  1             53
# 7 pan_effectmon_sac5_custommeas SOIL_03.2                1             17
# 8 pan_effectmon_sac5_custommeas SURF_03.4_lentic         1              7
# 9 pan_effectmon_sac5_custommeas HQ3110                   1              1
# 10 pan_effectmon_sac5_custommeas HQ3130                   1             18
# 11 pan_effectmon_sac5_custommeas HQ3140                   1              2
# 12 pan_effectmon_sac5_custommeas HQ3150                   1              4
# 13 pan_effectmon_sac5_custommeas HQ3160                   1              4
#

.chain %>%
  filter(diff != 0) -> extended











extended %>%
  anti_join(orig, join_by(module, domain, scheme, panel_set, stratum)) %>%
  count(module, scheme, stratum) %>%
  print(n = Inf)

# A tibble: 29 × 4
# module                        scheme           stratum           n
# <fct>                         <fct>            <fct>         <int>
#   1 pan_effectmon_sac5_custommeas GW_03.3          3130_aom_0_1      2
# 2 pan_effectmon_sac5_custommeas GW_03.3          3130_aom_1_5      4
# 3 pan_effectmon_sac5_custommeas GW_03.3          3130_aom_5_50     4
# 4 pan_effectmon_sac5_custommeas GW_03.3          3130_na_0_1       3
# 5 pan_effectmon_sac5_custommeas GW_03.3          3130_na_1_5       3
# 6 pan_effectmon_sac5_custommeas GW_03.3          3130_na_5_50      2
# 7 pan_effectmon_sac5_custommeas GW_03.3          3140_0_1          1
# 8 pan_effectmon_sac5_custommeas GW_03.3          3150_0_1          1
# 9 pan_effectmon_sac5_custommeas GW_03.3          3150_1_5          2
# 10 pan_effectmon_sac5_custommeas GW_03.3          3150_5_50         1
# 11 pan_effectmon_sac5_custommeas GW_03.3          3160_1_5          3
# 12 pan_effectmon_sac5_custommeas GW_03.3          3160_5_50         1
# 13 pan_effectmon_sac5_custommeas SURF_03.4_lentic 3130_aom_1_5      1
# 14 pan_effectmon_sac5_custommeas SURF_03.4_lentic 3130_aom_5_50     2
# 15 pan_effectmon_sac5_custommeas SURF_03.4_lentic 3130_na_0_1       1
# 16 pan_effectmon_sac5_custommeas SURF_03.4_lentic 3130_na_5_50      2
# 17 pan_effectmon_sac5_custommeas SURF_03.4_lentic 3160_5_50         1
# 18 pan_effectmon_sac5_custommeas HQ3130           3130_aom_0_1      2
# 19 pan_effectmon_sac5_custommeas HQ3130           3130_aom_1_5      4
# 20 pan_effectmon_sac5_custommeas HQ3130           3130_aom_5_50     4
# 21 pan_effectmon_sac5_custommeas HQ3130           3130_na_0_1       3
# 22 pan_effectmon_sac5_custommeas HQ3130           3130_na_1_5       3
# 23 pan_effectmon_sac5_custommeas HQ3130           3130_na_5_50      2
# 24 pan_effectmon_sac5_custommeas HQ3140           3140_0_1          1
# 25 pan_effectmon_sac5_custommeas HQ3150           3150_0_1          1
# 26 pan_effectmon_sac5_custommeas HQ3150           3150_1_5          2
# 27 pan_effectmon_sac5_custommeas HQ3150           3150_5_50         1
# 28 pan_effectmon_sac5_custommeas HQ3160           3160_1_5          3
# 29 pan_effectmon_sac5_custommeas HQ3160           3160_5_50         1

extended %>%
  anti_join(orig, join_by(module, domain, scheme, panel_set, stratum)) %>%
  summarize(
    ssize_increase = sum(diff),
    .by = c(module, scheme, panel_set)
  ) %>%
  arrange(module, scheme) %>%
  print(n = Inf)

# A tibble: 6 × 4
# module                        scheme           panel_set ssize_increase
# <fct>                         <fct>                <int>          <int>
#   1 pan_effectmon_sac5_custommeas GW_03.3                  1             27
# 2 pan_effectmon_sac5_custommeas SURF_03.4_lentic         1              7
# 3 pan_effectmon_sac5_custommeas HQ3130                   1             18
# 4 pan_effectmon_sac5_custommeas HQ3140                   1              1
# 5 pan_effectmon_sac5_custommeas HQ3150                   1              4
# 6 pan_effectmon_sac5_custommeas HQ3160                   1              4







