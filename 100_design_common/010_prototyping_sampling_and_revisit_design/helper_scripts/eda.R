library(ggplot2)
inner_join(
  domain_stratum_ncells,
  domain_stratum_nunits,
  by = c("domain", "stratum")
) %>%
  inner_join(n2khab_strata, by = "stratum") %>%
  inner_join(
    n2khab_types_expanded_properties %>%
      select(type, sample_support_code),
    by = "type"
  ) %>%
  ggplot(aes(x = nunits, y = ncells, colour = sample_support_code)) +
  geom_abline(slope = 1) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()
