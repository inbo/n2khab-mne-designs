library(ggplot2)


# MHQ-derived components --------------------------------------------------

valid_extra_popunits_cell_all_n2khab %>%
  inner_join(
    valid_extra_popunits_cell_all_n2khab,
    join_by(grts_address_drawn == grts_address),
    relationship = "many-to-many",
    unmatched = "drop",
    keep = TRUE
  ) %>%
  filter(grts_address_drawn.x != grts_address.x)

# replacements

valid_extra_popunits_cell_all_n2khab %>%
  filter(grts_address_drawn != grts_address)

# non-replacements

valid_extra_popunits_cell_all_n2khab %>%
  filter(grts_address_drawn == grts_address)

# phab correction ---------------------------------------------------------


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

# comparing mhq_terr sample with stratum_grts_n2khab_phabcorrected

mhq_terr_datapath <- file.path(dirname(gitroot), "n2khab-sample-admin/data/mhq_terr/rapportage2025")
mhq_terr_assessments <-
  read_vc("mhq_terr_assessments", root = mhq_terr_datapath) %>%
  as_tibble()
mhq_terr_popunits <-
  read_vc("mhq_terr_popunits", root = mhq_terr_datapath) %>%
  as_tibble()
mhq_assessed_locations <-
  mhq_terr_popunits %>%
  select(point_code, grts_ranking_draw, type) %>%
  semi_join(
    mhq_terr_assessments %>%
      filter(is_present) %>%
      distinct(point_code, type),
    join_by(point_code, type)
  ) %>%
  mutate(grts_ranking_draw = as.integer(grts_ranking_draw)) %>%
  select(grts_ranking_draw, type) %>%
  distinct()

mhq_assessed_locations %>%
  filter(type %in% stratum_grts_n2khab_phabcorrected$stratum) %>%
  left_join(
    stratum_grts_n2khab_phabcorrected %>%
      mutate(match = TRUE),
    join_by(
      grts_ranking_draw == grts_address,
      type == stratum
    ),
    relationship = "one-to-one"
  ) %>%
  summarize(
    n_match = n(),
    prop_match = sum(match, na.rm = TRUE) / n(),
    .by = type
  ) %>%
  arrange(desc(prop_match)) %>%
  print(n = Inf)

