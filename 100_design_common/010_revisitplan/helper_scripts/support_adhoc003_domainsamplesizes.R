# Calculating and plotting the degree of sample densification in the included
# domains, in groundwater schemes.

# Note that the code actually supports all schemes if the filter on scheme is
# dropped.

# First run setup chunk
#
# Then run:

load(file.path(datapath, "binary/results/objects_panflpan5.RData"))

relative_sample_sizes_per_domain <-
  scheme_moco_ps_stratum_dom_spsamples %>%
  # considering groundwater schemes only:
  filter(str_detect(scheme, "^GW")) %>%
  inner_join(
    domainpart_grts_n2khab,
    join_by(grts_address),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  # total obtained sample size per domain partition, across modules
  count(
    scheme,
    module_combo_code,
    panel_set,
    domain_part,
    stratum,
    name = "total_sample_size"
  ) %>%
  filter(str_detect(domain_part, "^BE")) %>%
  # since we only look at the inner domains, we can drop the '_part' notion
  rename(domain = domain_part) %>%
  # Joining sample sizes of domains per scheme x moco x panel set. Considering
  # this as the densification sample size relies on the fact that these included
  # domains are only explicit in modules where these domains are densified.
  left_join(
    scheme_moco_ps_dom_stratum_sample_size %>%
      select(
        scheme,
        module_combo_code,
        panel_set,
        domain,
        stratum,
        densification_sample_size = sp_sample_size_all_panels_stratum
      ),
    join_by(
      scheme,
      module_combo_code,
      panel_set,
      domain,
      stratum
    ),
    relationship = "one-to-one",
    unmatched = "drop"
  ) %>%
  replace_na(list(densification_sample_size = 0)) %>%
  add_col_in_aquatic_subset(n2khab_strata, n2khab_types_expanded_properties) %>%
  rename(is_aquatic = in_aquatic_subset) %>%
  mutate(
    is_aquatic = is_aquatic | stratum == "7140_mrd",
    sampledensification_proportion =
      densification_sample_size / total_sample_size
  ) %>%
  arrange(
    scheme,
    module_combo_code,
    panel_set,
    domain,
    stratum,
    is_aquatic
  )

# boxplot of the sample densification proportion per stratum
relative_sample_sizes_per_domain %>%
  ggplot(aes(
    x = domain,
    y = sampledensification_proportion,
    colour = is_aquatic
  )) +
  geom_boxplot(aes()) +
  facet_wrap(~ scheme + panel_set, labeller = label_both) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1)) +
  ggtitle("Distribution of the sample densification proportion, over strata")

# number of strata involved in the boxplot, for each box
relative_sample_sizes_per_domain %>%
  count(scheme, module_combo_code, panel_set, domain, is_aquatic) %>%
  pivot_wider(
    names_from = is_aquatic,
    values_from = n,
    names_prefix = "is_aquatic:"
  )

# density plot of the stratum's total sample size
relative_sample_sizes_per_domain %>%
  ggplot(aes(x = total_sample_size, colour = domain)) +
  geom_density() +
  facet_wrap(~ scheme + panel_set, labeller = label_both) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1)) +
  ggtitle("Distribution of total sample size, over strata")

# summing sample sizes over strata and panel sets
relative_sample_sizes_per_domain %>%
  mutate(sample_size_from_panfl = total_sample_size - densification_sample_size) %>%
  summarize(
    across(contains("sample_size"), sum),
    .by = c(scheme, domain, is_aquatic)
  ) %>%
  arrange(is_aquatic, domain)

# exploring outliers
relative_sample_sizes_per_domain %>%
  select(-module_combo_code) %>%
  filter(
    domain == "BE2200039",
    !is_aquatic,
    panel_set == 1,
    sampledensification_proportion < 1
  )

# code to share the R-object as files
relative_sample_sizes_per_domain %>%
  write_vc(
    "relative_sample_sizes_per_domain",
    root = projroot,
    sorting = c(
      "scheme",
      "module_combo_code",
      "panel_set",
      "domain",
      "stratum",
      "is_aquatic"
    ),
    strict = FALSE,
    optimize = FALSE,
    digits = 7
  )

