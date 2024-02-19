library(ggplot2)
library(cols4all)
library(tidyterra)
library(ggnewscale)

# Comparison of ncells and nunits -----------------------------------------

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


# Input MBAG presentation -------------------------------------------------

schemes_plot <-
  domain_stratum_nunits %>%
  filter(domain != "SAC_network") %>%
  inner_join(n2khab_strata, by = "stratum") %>%
  select(domain, type) %>%
  inner_join(targetpop, by = "type", relationship = "many-to-many") %>%
  select(domain, scheme, type) %>%
  distinct() %>%
  filter(str_detect(scheme, "_03")) %>%
  inner_join(read_types(lang = "nl"), by = "type") %>%
  ggplot(aes(x = scheme, fill = typeclass_name)) +
  geom_bar() +
  scale_fill_discrete_c4a_cat("carto.safe") +
  facet_wrap(~ domain) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1)) +
  labs(
    x = "Meetnet",
    y = "Aantal types",
    title = "Aantal types per meetnet",
    fill = "Typeklasse"
  )
ggsave(
  file.path(plotpath, "schemes_plot.png"),
  schemes_plot,
  width = 9,
  height = 5
)

polygons <-
  stratum_polygons_cell_all_n2khab %>%
  semi_join(targetpop_strata, by = "stratum") %>%
  semi_join(hmt_pol, ., by = "polygon_id")

domains_plot <-
  domains %>%
  filter(domain != "SAC_network") %>%
  ggplot() +
  geom_sf(aes(fill = domain), colour = NA) +
  scale_fill_manual(values = c("grey95", "yellow", "lightsalmon")) +
  geom_sf(data = domains[1, ], colour = "grey80", fill = NA) +
  labs(title = "Verspreiding doelpopulatie in 3 domeinen", fill = "Domein") +
  geom_sf(data = polygons, fill = "darkgreen", colour = NA) +
  new_scale_fill() +
  geom_spatraster(
    data = !is.na(grts_mh_non_cell),
    aes(fill = GRTSmaster_habitats),
    # maxcell = 20e5
  ) +
  scale_fill_manual(values = c("transparent", "darkgreen")) +
  coord_sf(datum = "EPSG:31370") +
  guides(fill = "none") +
  theme_bw()
ggsave(
  file.path(plotpath, "domains_plot.png"),
  domains_plot,
  width = 9,
  height = 6,
  dpi = 600
)

# not used:

targetpop_grts <-
  stratum_grts_n2khab_phabcorrected %>%
  semi_join(targetpop_strata, by = "stratum") %>%
  distinct(grts_address) %>%
  pull(grts_address)

# subsetting by values is very inefficient (slow):
grts_mh_targetpop <- mask(grts_mh_n2khab, grts_mh_n2khab, targetpop_grts, inverse = TRUE)
# from https://gis.stackexchange.com/questions/421821/how-to-subset-a-spatraster-by-value-in-r
