# This code requires availability of the following objects:
# - wsh_pol_area_class
# - habquarries_area_class
# - module_domain_scheme_stratum_target_sample_size

library(ggplot2)

## Watersurfaces

wsh_strata_area <-
  wsh_pol_area_class %>%
  summarize(
    area = sum(area) %>% as.numeric(),
    .by = c(type, stratum)
  ) %>%
  extract_interval_limits() %>%
  mutate(int = factor(stratum)) %>%
  unite(stratum, c(type, lower, upper), remove = FALSE, na.rm = TRUE) %>%
  select(type, stratum, int, area) %>%
  mutate(stratum = factor(stratum))

module_domain_scheme_stratum_target_sample_size %>%
  filter(
    module == "mne2024_mbaa_mne_phase_1",
    domain == "Flanders",
    scheme == "SURF_03.4_lentic"
  ) %>%
  select(scheme, type, stratum, nunits, matches("sp_sample_.+(type|stratum)")) %>%
  rename(
    ss_type = sp_sample_size_all_panels_type
  ) %>%
  inner_join(wsh_strata_area, join_by(type, stratum)) %>%
  relocate(int, area, .after = stratum) %>%
  mutate(
    ss_stratum_nunits = ss_type * nunits / sum(nunits),
    ss_stratum_sqrt_nunits = ss_type * sqrt(nunits) / sum(sqrt(nunits)),
    ss_stratum_4throot_nunits = ss_type * nunits^0.25 / sum(nunits^0.25),
    ss_stratum_0 = ss_type / n(),
    ss_stratum_area = ss_type * area / sum(area),
    ss_stratum_sqrt_area = ss_type * sqrt(area) / sum(sqrt(area)),
    ss_stratum_4throot_area = ss_type * area^0.25 / sum(area^0.25),
    .by = c(scheme, type)
  ) %>%
  pivot_longer(
    starts_with("ss_stratum"),
    names_to = "method",
    values_to = "target_sample_size"
  ) %>%
  mutate(
    method = factor(method) %>%
      fct_relevel(
        "ss_stratum_area",
        "ss_stratum_sqrt_area",
        "ss_stratum_4throot_area",
        "ss_stratum_0",
        "ss_stratum_4throot_nunits",
        "ss_stratum_sqrt_nunits"
      )
  ) %>%
  ggplot(aes(x = method, y = target_sample_size, fill = fct_rev(int))) +
  geom_col(position = "stack") +
  facet_grid(scheme ~ type, scales = "free_x") +
  coord_flip()

## Quarries

habquarries_strata_area <-
  habquarries_area_class %>%
  summarize(
    area = sum(area) %>% as.numeric(),
    .by = c(type, stratum)
  ) %>%
  extract_interval_limits() %>%
  mutate(int = factor(stratum)) %>%
  unite(stratum, c(type, lower, upper), remove = FALSE, na.rm = TRUE) %>%
  select(type, stratum, int, area) %>%
  mutate(stratum = factor(stratum))

module_domain_scheme_stratum_target_sample_size %>%
  filter(
    module == "mne2024_mbaa_mne_phase_1",
    domain == "Flanders",
  ) %>%
  select(scheme, type, stratum, nunits, matches("sp_sample_.+(type|stratum)")) %>%
  rename(
    ss_type = sp_sample_size_all_panels_type
  ) %>%
  inner_join(habquarries_strata_area, join_by(type, stratum)) %>%
  relocate(int, area, .after = stratum) %>%
  mutate(
    ss_stratum_nunits = ss_type * nunits / sum(nunits),
    ss_stratum_sqrt_nunits = ss_type * sqrt(nunits) / sum(sqrt(nunits)),
    ss_stratum_4throot_nunits = ss_type * nunits^0.25 / sum(nunits^0.25),
    ss_stratum_0 = ss_type / n(),
    ss_stratum_area = ss_type * area / sum(area),
    ss_stratum_sqrt_area = ss_type * sqrt(area) / sum(sqrt(area)),
    ss_stratum_4throot_area = ss_type * area^0.25 / sum(area^0.25),
    .by = c(scheme, type)
  ) %>%
  pivot_longer(
    starts_with("ss_stratum"),
    names_to = "method",
    values_to = "target_sample_size"
  ) %>%
  mutate(
    method = factor(method) %>%
      fct_relevel(
        "ss_stratum_area",
        "ss_stratum_sqrt_area",
        "ss_stratum_4throot_area",
        "ss_stratum_0",
        "ss_stratum_4throot_nunits",
        "ss_stratum_sqrt_nunits"
      )
  ) %>%
  ggplot(aes(x = method, y = target_sample_size, fill = fct_rev(int))) +
  geom_col(position = "stack") +
  facet_grid(scheme ~ type, scales = "free_x") +
  coord_flip()

## Based on this exploration, a choice is made to use the 4th root of the number
## of units (nunits^0.25) as a compromise
