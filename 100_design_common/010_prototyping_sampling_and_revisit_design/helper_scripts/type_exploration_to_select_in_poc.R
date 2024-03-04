# This code requires the 'prepare-strata' chunk to have run.
# Below code narrows down to targetpops, for exploratory purposes.

# building blocks for the exploration which types to select from targetpops for
# the POC

pop_size_terr <-
  read_vc(
    "030_preparations/010_explore_targetpop/pop_size_terr",
    root = find_root(is_git_root)
  ) %>%
  as_tibble() %>%
  inner_join(
    read_vc(
      "030_preparations/010_explore_targetpop/type_approaches",
      root = find_root(is_git_root)
    ),
    by = "type") %>%
  filter(count_method == "cellbased" & approach_cellbased |
           count_method == "centroidbased" & !approach_cellbased) %>%
  select(-c(count_method, approach_cellbased)) %>%
  mutate(stratum = NA_character_,
         population_size = round(population_size) %>% as.integer) %>%
  select(type, stratum, population_size) %>%
  arrange(type, stratum)
pop_size_7220 <-
  habspring_units %>%
  st_drop_geometry %>%
  count(system_type) %>%
  pivot_wider(names_from = system_type,
              values_from = n) %>%
  transmute(all = mire + rivulet + unknown,
            terrestrial = mire + unknown,
            aquatic = rivulet + unknown) %>%
  mutate(type = "7220" %>% factor(levels = levels(pop_size_terr$type))) %>%
  pivot_longer(cols = c("all", "terrestrial", "aquatic"),
               names_to = "stratum",
               values_to = "population_size")
pop_size_3260 <-
  habstream %>%
  mutate(length = st_length(.) %>% drop_units(),
         population_size = ceiling(length / 100) %>% as.integer) %>%
  st_drop_geometry() %>%
  summarise(type = first(type),
            stratum = first(NA_character_),
            population_size = sum(population_size))
pop_size_lentic <-
  wsh_pol_area_class %>%
  semi_join(targetpops, by = "type") %>%
  count(type, stratum, name = "population_size")
pop_size_8310 <-
  habquarries_area_class %>%
  count(type, stratum, name = "population_size")
join_targetpops <- function(df) {
  df  %>%
    inner_join(targetpops %>%
                 select(scheme,
                        type,
                        contains("typegroup")),
               .,
               by = "type",
               relationship = "many-to-many") %>%
    filter(type != "7220" |
             (type == "7220" & stratum == "all" & !str_detect(scheme, "aq|terr|SURF")) |
             (type == "7220" & stratum == "aquatic" & str_detect(scheme, "aq|SURF")) |
             (type == "7220" & stratum == "terrestrial" & str_detect(scheme, "terr"))
    )
}
targetpops_stratumsize <-
  bind_rows(pop_size_terr,
            pop_size_7220,
            pop_size_3260,
            pop_size_lentic,
            pop_size_8310) %>%
  join_targetpops


# targetpops_stratumsize comprises the whole targetpops!
targetpops %>% anti_join(targetpops_stratumsize, by = c("scheme", "type"))
# targetpops_stratumsize has no empty population sizes!
targetpops_stratumsize %>% filter(is.na(population_size))
# targetpops_stratumsize has no type, stratum, population_size combinations that
# are different between schemes (it's obvious from the code above)
targetpops_stratumsize %>%
  distinct(type, stratum, population_size) %>%
  count(type, stratum) %>%
  filter(n > 1)
# the type_stratum_props object created below is the object we use to explore
# and decide which types to select for prototyping
stratum_props <-
  targetpops_stratumsize %>%
  distinct(type, stratum, population_size) %>%
  arrange(type, stratum) %>%
  inner_join(
    n2khab_types_expanded_properties,
    by = "type"
  )
# add relation to MNE schemes:
stratum_props %>%
  inner_join(targetpops, by = "type", relationship = "many-to-many") %>%
  filter(!str_detect(scheme, "^HQ")) %>%
  pivot_wider(
    names_from = scheme,
    values_from = type
  ) %>%
  relocate(matches("^(GW|SOIL|SURF)")) %>%
  View()
