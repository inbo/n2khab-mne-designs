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


# Input MBAA presentation 2024-02-23 --------------------------------------

schemes_plot <-
  domain_stratum_nunits %>%
  filter(domain != "SAC_network") %>%
  inner_join(n2khab_strata, by = "stratum") %>%
  select(domain, type) %>%
  inner_join(targetpops, by = "type", relationship = "many-to-many") %>%
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
  semi_join(targetpops_strata, by = "stratum") %>%
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

targetpops_grts <-
  stratum_grts_n2khab_phabcorrected %>%
  semi_join(targetpops_strata, by = "stratum") %>%
  distinct(grts_address) %>%
  pull(grts_address)

# subsetting by values is very inefficient (slow):
grts_mh_targetpops <- mask(grts_mh_n2khab, grts_mh_n2khab, targetpops_grts, inverse = TRUE)
# from https://gis.stackexchange.com/questions/421821/how-to-subset-a-spatraster-by-value-in-r


# Input MBAA presentation 2024-04-30 --------------------------------------

schemes_plot_mbaa_mne_phase_1 <-
  module_targetpops %>%
  filter(module == "mbaa_mne_phase_1") %>%
  # don't highlight 8310; artificially move it to terr:
  mutate(scheme = fct_recode(scheme, "GW_05.1_terr" = "GW_05.1_quarries")) %>%
  inner_join(read_types(lang = "nl"), by = "type") %>%
  ggplot(aes(x = scheme, fill = typeclass_name)) +
  geom_bar() +
  scale_fill_discrete_c4a_cat("carto.safe") +
  # facet_wrap(~ domain) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1)) +
  labs(
    x = "Meetnet",
    y = "Aantal types",
    title = "Aantal types per meetnet",
    fill = "Typeklasse"
  )
ggsave(
  file.path(plotpath, "schemes_plot_mbaa_mne_phase_1_8310_moved.png"),
  schemes_plot_mbaa_mne_phase_1,
  width = 9,
  height = 5
)

# Exploring approaches to get XY-coordinates from GRTSmaster_habitats

grts_addresses <- scheme_domain_stratum_spsamples %>%
  filter(domain == "Flanders") %>%
  distinct(grts_address) %>%
  pull(grts_address)

tictoc::tic()
grts_cells <- cells(grts_mh_n2khab, grts_addresses)[[1]]
tictoc::toc()

tictoc::tic()
grts_cells2 <-
  grts_mh_n2khab_index %>%
  filter(grts_address %in% grts_addresses) %>%
  arrange(grts_address) %>%
  pull(id)
tictoc::toc()

all.equal(grts_cells, grts_cells2)
# [1] "Mean relative difference: 0.50066098969208"
# see https://github.com/rspatial/terra/issues/1487

tictoc::tic()
res <- xyFromCell(grts_mh_n2khab, grts_cells2)
tictoc::toc()

# Going for the indexed approach; slightly faster than cells() (and for cells()
# see https://github.com/rspatial/terra/issues/1487)

scheme_types <- read_scheme_types(lang = lang)

spsamples <-
  scheme_domain_stratum_spsamples %>%
  filter(domain == "Flanders") %>%
  select(-domain) %>%
  inner_join(n2khab_strata, join_by(stratum)) %>%
  inner_join(scheme_types, join_by(scheme, type))

spsamples %>%
  count(scheme, type)

spsamples %>%
  count(scheme)

spsamples_points <- add_point_coords_grts(spsamples)

old <- theme_set(theme_bw())
theme_update(
  panel.grid = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank()
)
flanders <- read_admin_areas()
scheme_names <-
  read_schemes(lang = lang) %>%
  select(scheme, scheme_name)

for (scheme_i in sort(unique(spsamples$scheme))) {
  plot_i <-
    ggplot() +
    geom_sf(data = flanders, fill = "white") +
    geom_sf(
      data = spsamples_points %>%
        filter(scheme == scheme_i),
      size = 0.5,
      colour = "#843860"
    ) +
    coord_sf(datum = 31370) +
    facet_wrap(~ typegroup_shortname) +
    ggtitle(scheme_names %>% filter(scheme == scheme_i) %>% pull(scheme_name))
  ggsave(
    file.path(plotpath, str_c("sample_map_", scheme_i, ".png")),
    plot_i,
    width = 9,
    height = 6,
    dpi = 600
  )
}

