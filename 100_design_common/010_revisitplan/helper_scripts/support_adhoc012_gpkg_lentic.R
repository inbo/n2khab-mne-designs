# 2026-07-16 Generating the locations of specific lentic types as a spatial
# object with selected sampling unit attributes & temporal attributes and using
# the actual sampling unit geometries. Optionally writing this to a Geopackage.
#
# The results have been made with the RData file at commit d9383e08, by running:
#
# Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::html_document2",
# params = list(save_rdata = TRUE))'

# First run setup chunk
#
# Then run:

load(file.path(datapath, "binary/results/objects_panflpan5.RData"))


# Spatial object of lentic locations with unique GRTS address -------------

grts_lentic_sf <-
  stratum_grts_spsamples_lentic_sf %>%
  distinct(grts_address_final, polygon_id, geom)


# Filtering and adding needed attributes ----------------------------------

scheme_grts_sptempattribs_lentic_sf <-
  scheme_moco_ps_spsubset_targetfag_stratum_sppost_spsamples_calendar %>%
  # using type instead of stratum
  inner_join(
    n2khab_strata,
    join_by(stratum),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  filter(
    str_detect(type, lentic_types_regex),
    # for now, exclude 2190_a since its sampling frame needs changes
    type != "2190_a",
    str_detect(scheme, "^GW|^SURF")
  ) %>%
  mutate(year_sampling = year(date_start)) %>%
  # generating sampling unit x sampling year combinations per panel set, in
  # order to (approximately) select the years from the first cycle (see filter
  # step)
  distinct(
    scheme,
    panel_set,
    type,
    grts_address,
    grts_address_final,
    year_sampling
  ) %>%
  arrange(pick(everything())) %>%
  filter(
    (year_sampling %in% year_sampling[1:2] & panel_set == 1) |
      (year_sampling == year_sampling[1] & panel_set == 2),
    # prevent artifact of 2035 sneeking in
    year_sampling <= 2031,
    .by = c(scheme, panel_set, type, grts_address, grts_address_final)
  ) %>%
  # adding domain partition
  inner_join(
    domainpart_grts_n2khab,
    join_by(grts_address),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  # eliminating duplications of sampling unit x year between panel sets
  distinct(
    scheme,
    grts_address,
    grts_address_final,
    domain_part,
    type,
    year_sampling
  ) %>%
  arrange(scheme, grts_address, type, year_sampling) %>%
  # flattening type and year_sampling to obtain unique locations
  # (grts_address_final & geometry)
  summarise(
    types_in_sample =
      str_flatten(sort(unique(type)), collapse = "|") %>% factor(),
    sampling_years =
      str_flatten(sort(unique(year_sampling)), collapse = "|") %>% factor(),
    .by = c(scheme, grts_address, grts_address_final, domain_part)
  ) %>%
  # adding the geometry (and polygon_id: as a referral to watersurfaces_hab &
  # watersurfaces data source) of each grts_address_final
  inner_join(
    grts_lentic_sf,
    join_by(grts_address_final),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  st_as_sf(sf_column_name = "geom") %>%
  relocate(polygon_id, .after = grts_address_final) %>%
  relocate(domain_part, .before = geom)


# Optionally write to Geopackage ------------------------------------------

if (FALSE) {
  gpkg_path_gw_surf <- file.path(
    datapath,
    "binary/results/infodelivery/mnm_steekproef_lentisch_gw&surf_excl2190a.gpkg"
  )
  gpkg_path_surf <- file.path(
    datapath,
    "binary/results/infodelivery",
    "mnm_steekproef_oppwatermeetnet_lentisch_excl2190a.gpkg"
  )
  scheme_grts_sptempattribs_lentic_sf %>%
    base::split(.$scheme, drop = TRUE) %>%
    walk(\(df) {
      schemename <- df$scheme[1]
      write_sf(
        df,
        gpkg_path_gw_surf,
        layer = schemename,
        delete_dsn = str_detect(schemename, "^GW"),
        delete_layer = TRUE
      )
      if (str_detect(schemename, "^SURF")) {
        write_sf(
          df %>% select(-scheme, -grts_address),
          gpkg_path_surf,
          delete_dsn = TRUE
        )
      }
    })
}
