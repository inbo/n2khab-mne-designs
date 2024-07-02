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
grts_cells <-
  cells(grts_mh_n2khab, grts_addresses, pairs = TRUE)[[1]] %>%
    as_tibble() %>%
    arrange(value) %>%
    pull(cell)
tictoc::toc()

tictoc::tic()
grts_cells2 <-
  grts_mh_n2khab_index %>%
  filter(grts_address %in% grts_addresses) %>%
  arrange(grts_address) %>%
  pull(id)
tictoc::toc()

all.equal(grts_cells, grts_cells2)
# TRUE

tictoc::tic()
res <- xyFromCell(grts_mh_n2khab, grts_cells2)
tictoc::toc()

# Going for the indexed approach; slightly faster than cells()

type_spsamplesizes <-
  module_domain_scheme_stratum_sample_size_2 %>%
  filter(module == "mbaa_mne_phase_1", domain == "Flanders") %>%
  distinct(scheme, type, sp_sample_size_all_panels_type)

scheme_types_attr <-
  read_scheme_types(lang = lang) %>%
  inner_join(type_spsamplesizes, join_by(scheme, type)) %>%
  mutate(
    typegroup_name2 = str_c(
      coalesce(typegroup_shortname, ""),
      " (",
      n(),
      " type",
      ifelse(n() == 1, "", "s"),
      ", ",
      sum(sp_sample_size_all_panels_type) %>% as.integer(),
      " locs)"
    ) %>%
      str_trim(),
    .by = c(scheme, typegroup)
  ) %>%
  mutate(
    typegroup_name2 = typegroup_name2 %>%
      fct_reorder(coalesce(as.numeric(typegroup), 0))
  ) %>%
  select(scheme, type, typegroup_name2)

spsamples <-
  scheme_domain_stratum_spsamples %>%
  filter(domain == "Flanders") %>%
  select(-domain) %>%
  inner_join(n2khab_strata, join_by(stratum)) %>%
  inner_join(scheme_types_attr, join_by(scheme, type))

spsamples %>%
  count(scheme, type)

spsamples %>%
  count(scheme)

spsamples_points <- add_point_coords_grts(spsamples)

old <- theme_set(theme_bw())
theme_update(
  panel.grid = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  plot.title = element_text(size = 12, hjust = 0.5)
)
provinces <- read_admin_areas(dsn = "provinces")

for (scheme_i in sort(unique(spsamples$scheme))) {
  plot_i <-
    ggplot() +
    geom_sf(data = provinces, fill = "white", colour = "grey80") +
    geom_sf(
      data = spsamples_points %>%
        filter(scheme == scheme_i),
      size = 0.3,
      colour = "#843860"
    ) +
    coord_sf(datum = 31370) +
    facet_wrap(~ typegroup_name2) +
    ggtitle(scheme_i)
  ggsave(
    file.path(plotpath, str_c("sample_map_provinces_", scheme_i, ".png")),
    plot_i,
    width = 11,
    height = 7,
    dpi = 600
  )
}


# Plots for MNE video ------------------------------------------------------

provinces <- read_admin_areas(dsn = "provinces")

# below code is inspired by DCP MBAA (tag DCP_MBAA_20240621_poc_0.1.0)

spsamples <-
  scheme_domain_stratum_spsamples %>%
  select(-domain) %>%
  inner_join(n2khab_strata, join_by(stratum)) %>%
  inner_join(n2khab_schemes %>% select(scheme, scheme_name), join_by(scheme)) %>%
  mutate(
    scheme_name = str_replace(scheme_name, ": deelmeetnet", ":\ndeelmeetnet"),
    scheme_name = str_replace(scheme_name, " \\(incl", "\n(incl")
  )

spsamples_points <- add_point_coords_grts(spsamples)

spsamples_points %>%
  nest(data = -c(scheme, scheme_name)) %>%
  {
    pwalk(list(.$scheme, .$scheme_name, .$data), function(sch, schn, d) {
      p <- ggplot() +
        geom_sf(data = provinces, fill = "white", colour = "grey70") +
        geom_sf(data = d, size = 0.25, colour = "#843860") +
        ggtitle(schn) +
        theme(
          panel.grid = element_blank(),
          panel.background = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()
        ) +
        theme(plot.title = element_text(size = 12, hjust = 0.5))
      ggsave(
        file.path(plotpath, str_c("sample_map_provinces_no_facet_", sch, ".png")),
        p,
        width = 6,
        height = 3,
        dpi = 300
      )
    })
  }

