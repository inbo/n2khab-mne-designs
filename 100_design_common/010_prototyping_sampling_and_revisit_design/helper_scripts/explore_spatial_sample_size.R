# Explore spatial sample sizes

# First run setup chunk
#
# Then run:

load(file.path(datapath, "binary/results/objects_panflpan5.RData"))

library(ggplot2)

## Explore aquatic sample sizes in MNE (panfl_pan5) and MHQ-Flanders

counts_aquatic <-
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
  arrange(type, scheme)

counts_aquatic %>%
  print(n = Inf)

mhq_aq <-
  mhq_samples %>%
  add_col_is_strictly_aquatic(n2khab_strata, n2khab_types_expanded_properties) %>%
  filter(is_strictly_aquatic) %>%
  inner_join(n2khab_strata, join_by(stratum)) %>%
  count(type, name = "sp_sample_size_all_panels_type")
mhq_aq

samplesizes_aq_plot <-
  counts_aquatic %>%
  bind_rows(
    mhq_aq %>%
      mutate(
        scheme = "MHQ",
        panel_set = 1L
      ) %>%
      relocate(type, scheme, panel_set)
  ) %>%
  mutate(
    panel_set = factor(panel_set) %>% fct_rev,
    scheme = factor(scheme, levels = c(levels(counts_aquatic$scheme), "MHQ"))
  ) %>%
  ggplot(aes(x = scheme, fill = panel_set, y = sp_sample_size_all_panels_type)) +
  geom_col() +
  facet_wrap(~type) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

samplesizes_aq_plot

ggsave(
  "plots/samplesizes_aq_plot.png",
  samplesizes_aq_plot,
  width = 8,
  height = 6
)

## Explore aquatic sample sizes in MNE (panfl_pan5_pan2124) and MHQ-Flanders

counts_aquatic_withpan2124 <-
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
    str_detect(module, "effectmon|transmon"),
    is_strictly_aquatic,
    !is.na(sp_sample_size_all_panels_type),
    !str_detect(scheme, "^HQ")
  ) %>%
  summarize(
    sp_sample_size_all_panels_type = sum(sp_sample_size_all_panels_stratum) %>% round,
    .by = c(type, scheme, panel_set)
  ) %>%
  arrange(type, scheme)

counts_aquatic_withpan2124 %>%
  print(n = Inf)

mhq_aq <-
  mhq_samples %>%
  add_col_is_strictly_aquatic(n2khab_strata, n2khab_types_expanded_properties) %>%
  filter(is_strictly_aquatic) %>%
  inner_join(n2khab_strata, join_by(stratum)) %>%
  count(type, name = "sp_sample_size_all_panels_type")
mhq_aq

samplesizes_aq_plot_withpan2124 <-
  counts_aquatic_withpan2124 %>%
  bind_rows(
    mhq_aq %>%
      mutate(
        scheme = "MHQ",
        panel_set = 1L
      ) %>%
      relocate(type, scheme, panel_set)
  ) %>%
  mutate(
    panel_set = factor(panel_set) %>% fct_rev,
    scheme = factor(scheme, levels = c(levels(counts_aquatic$scheme), "MHQ"))
  ) %>%
  ggplot(aes(x = scheme, fill = panel_set, y = sp_sample_size_all_panels_type)) +
  geom_col() +
  facet_wrap(~type) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

samplesizes_aq_plot_withpan2124

ggsave(
  "plots/samplesizes_aq_plot_withpan2124.png",
  samplesizes_aq_plot_withpan2124,
  width = 8,
  height = 6
)

## Explore terrestrial sample sizes in MNE (panfl_pan5) and MHQ-Flanders

counts_terrestrial <-
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
    !is_strictly_aquatic,
    scheme %in% c("GW_03.3", "SOIL_03.2"),
    !is.na(sp_sample_size_all_panels_type),
    !str_detect(scheme, "^HQ")
  ) %>%
  summarize(
    sp_sample_size_all_panels_type = sum(sp_sample_size_all_panels_stratum) %>% round,
    .by = c(type, scheme, panel_set)
  ) %>%
  arrange(type, scheme)

counts_terrestrial

mhq_terr <-
  mhq_samples %>%
  add_col_is_strictly_aquatic(n2khab_strata, n2khab_types_expanded_properties) %>%
  filter(!is_strictly_aquatic) %>%
  inner_join(n2khab_strata, join_by(stratum)) %>%
  count(type, name = "sp_sample_size_all_panels_type")
mhq_terr

samplesizes_terr_plot <-
  counts_terrestrial %>%
  bind_rows(
    mhq_terr %>%
      mutate(
        scheme = "MHQ",
        panel_set = 1L
      ) %>%
      relocate(type, scheme, panel_set)
  ) %>%
  mutate(
    panel_set = factor(panel_set) %>% fct_rev,
    scheme = factor(scheme, levels = c(levels(counts_terrestrial$scheme), "MHQ"))
  ) %>%
  ggplot(aes(x = scheme, fill = panel_set, y = sp_sample_size_all_panels_type)) +
  geom_col() +
  facet_wrap(~type) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

samplesizes_terr_plot

ggsave(
  "plots/samplesizes_terr_plot.png",
  samplesizes_terr_plot,
  width = 12,
  height = 10
)


