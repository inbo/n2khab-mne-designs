schemes <-
  read_schemes(lang = lang) %>%
  filter(attribute_1 %in% c("GW", "SURF", "SOIL")) %>%
  select(scheme, programme, attribute_1, attribute_2, attribute_3, tag_1, spatial_restriction)

core_schemes_1st_phase <- c(
  "GW_03.3",
  "SOIL_03.2",
  "SURF_03.4_lentic",
  "SURF_03.4_lotic"
)

schemes_1st_phase <- c(
  core_schemes_1st_phase,
  "GW_04.2",
  "GW_05.1_aq",
  "GW_05.1_terr", # don't take the whole target population here!! Only groups 2, 3, 5
  "GW_05.2",
  "GW_07.1",
  "SURF_064_lentic", # this one's existence is still to be confirmed
  "SURF_07.2"
)

targetpop <-
  read_scheme_types(lang = lang) %>%
  select(scheme, type) %>%
  semi_join(schemes, by = "scheme")

targetpop_exclude_from_1st_phase <-
  read_scheme_types(lang = lang) %>%
  filter(
    scheme == "GW_05.1_terr",
    str_detect(typegroup, "GW_05.1_terr_group(1|4)")
  ) %>%
  select(scheme, type)

targetpop <-
  targetpop %>%
  anti_join(targetpop_exclude_from_1st_phase, by = c("scheme", "type"))

# MHQ has no unique types relative to the MNE target population for GW+SURF+SOIL
read_scheme_types() %>%
  select(scheme, type) %>%
  semi_join(
    read_schemes() %>%
      filter(programme == "MHQ"),
    by = "scheme"
  ) %>%
  anti_join(targetpop, by = "type")

# Which types belong to targetpop of schemes_1st_phase and NOT to targetpop of core_schemes_1st_phase?
# This also needs the stratum_props object from 'type_exploration_to_select_in_poc.R'
non_core_types <-
  read_scheme_types(lang = lang) %>%
  select(scheme, type, typegroup, typegroup_shortname) %>%
  filter(scheme %in% schemes_1st_phase) %>%
  left_join(
    read_scheme_types() %>%
      count(scheme, typegroup, name = "group_size"),
    by = c("scheme", "typegroup")
  ) %>%
  # distinct(type) %>%
  anti_join(
    read_scheme_types(lang = lang) %>%
      select(scheme, type) %>%
      filter(scheme %in% core_schemes_1st_phase) %>%
      distinct(type),
    by = "type"
  ) %>%
  left_join(
    stratum_props %>%
      select(type, stratum, population_size, sample_support) %>%
      inner_join(
        read_types(lang = lang) %>%
          select(type, type_shortname),
        by = "type"
      ),
    by = "type",
    relationship = "many-to-many"
  ) %>%
  relocate(type_shortname, .after = type) %>%
  arrange(scheme, typegroup, type, stratum)

# Maintaining typegroep precision for a typegroup of k types, when p types are removed,
# necessitates sample size increase of each remaining type by a factor k / (k - p)

library(ggplot2)
crossing(p = 0:5, k = 10:30) %>%
  mutate(relative_sample_size_increase = k / (k - p)) %>%
  ggplot(aes(x = k, y = p, fill = relative_sample_size_increase)) +
  geom_tile() +
  geom_text(aes(label = round(relative_sample_size_increase, 2)), color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    x = "Number of types in a typegroup",
    y = "Number of types removed from a typegroup",
    fill = "Needed relative\nsample size increase"
  )

# The MNE + MHQ '1st phase' targetpop comprises all types targeted by scheme
# ATM_03.1

schemes <- read_schemes(lang = lang) %>%
  filter(programme == "MHQ") %>%
  select(scheme, programme, attribute_1, attribute_2, attribute_3, tag_1, spatial_restriction) %>%
  semi_join( ### REVISIT THIS: do we want to restrict MHQ to the types in MNE? (trivial at the time of writing)
    read_scheme_types() %>%
      select(scheme, type) %>%
      semi_join(targetpop, by = "type"),
    by = "scheme"
  ) %>%
  bind_rows(schemes, .)
targetpop <-
  read_scheme_types(lang = lang) %>%
  select(scheme, type) %>%
  semi_join(schemes, by = "scheme")
read_scheme_types(lang = lang) %>%
  filter(scheme == "ATM_03.1") %>%
  select(type) %>%
  anti_join(targetpop, by = "type")

