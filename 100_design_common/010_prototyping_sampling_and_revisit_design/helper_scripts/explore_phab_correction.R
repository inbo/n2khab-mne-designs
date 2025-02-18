library(ggplot2)

# object where popsize is determined by cell count after bernouilli sampling
domain_stratum_cellcenter_nunits <-
  domain_grts_n2khab %>%
  inner_join(
    stratum_grts_n2khab_phabcorrected,
    join_by(grts_address),
    relationship = "many-to-many",
    unmatched = c("drop", "drop")
  ) %>%
  semi_join(
    n2khab_types_expanded_properties %>%
      filter(sample_support_code == "cell_conditioned_on_center"),
    join_by(stratum == type)
  ) %>%
  count(domain, stratum, name = "nunits")

# comparing this with uncorrected population size in collapsed base sampling
# frame (grand total):

stratum_grts_phab_cellcenter_n2khab_collapsed %>%
  count(stratum, name = "nunits_uncorrected") %>%
  inner_join(
    domain_stratum_cellcenter_nunits %>%
      filter(domain == "Flanders") %>%
      rename(nunits_phabcorrected = nunits),
    join_by(stratum),
    relationship = "one-to-one",
    unmatched = "error"
  ) %>%
  mutate(fraction_kept = nunits_phabcorrected / nunits_uncorrected) %>%
  ggplot(aes(x = fraction_kept)) +
  geom_density()

# object where popsize is determined by phab-corrected summation of original
# cells (base sampling frame)

domain_stratum_cellcenter_nunits_calculated <-
  domain_grts_n2khab %>%
  inner_join(
    stratum_grts_phab_cellcenter_n2khab_collapsed,
    join_by(grts_address),
    relationship = "many-to-many",
    unmatched = c("drop", "drop")
  ) %>%
  summarize(
    nunits = sum(phab) / 100,
    .by = c(domain, stratum)
  ) %>%
  arrange(domain, stratum)

# comparing both phab-corrected objects:

domain_stratum_cellcenter_nunits_calculated %>%
  anti_join(domain_stratum_cellcenter_nunits, join_by(domain, stratum))

diffs %>%
  ggplot(aes(x = reldiff)) +
  geom_density() +
  facet_wrap(~domain, scales = "free_y")

diffs %>%
  ggplot(aes(x = absdiff)) +
  geom_density() +
  facet_wrap(~domain, scales = "free_y")
