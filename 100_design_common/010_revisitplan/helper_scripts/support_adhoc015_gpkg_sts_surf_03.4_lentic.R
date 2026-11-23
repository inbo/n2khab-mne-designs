# 2026-11-20 Generating the spatial sampling units (per panel_set) of specific
# types in SURF_03.4_lentic as a spatial object with selected sampling unit
# attributes & temporal attributes (first cycle only) and using the actual
# sampling unit geometries. Optionally writing this to a Geopackage.
#
# The aim is to be able to visit spatial sampling units from a later panel in an
# earlier year, to anticipate sampling unit replacements that happen later on.
#
# This shifting is to be carried out at the panel_set x stratum x spatial
# poststratum (sp_poststratum) level.
#
# The results have been made with the RData file at commit ec1693f0, by running:
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

surf_03.4_lentic_sts_cycle1 <-
  scheme_moco_ps_spsubset_targetfag_stratum_sppost_spsamples_calendar %>%
  # adding type
  inner_join(
    n2khab_strata,
    join_by(stratum),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  filter(
    # for now, exclude 2190_a since its sampling frame needs changes
    type != "2190_a",
    scheme == "SURF_03.4_lentic"
  ) %>%
  mutate(year_sampling = year(date_start)) %>%
  # generating sampling unit x sampling year combinations per panel set, in
  # order to (approximately) select the years from the first cycle (see filter
  # step)
  distinct(
    scheme,
    panel_set,
    targetpanel,
    year_sampling,
    type,
    stratum,
    sp_poststratum,
    grts_address,
    grts_address_final,
    last_type_assessment = assessment_date,
    last_type_assessment_in_field = assessed_in_field,
    last_inaccessible = inaccessible
  ) %>%
  arrange(pick(everything())) %>%
  filter(
    (year_sampling %in% year_sampling[1:2] & panel_set == 1) |
      (year_sampling == year_sampling[1] & panel_set == 2),
    # prevent artifact of 2035 sneeking in
    year_sampling <= 2031,
    .by = c(scheme, panel_set, type, stratum, grts_address, grts_address_final)
  ) %>%
  # adding domain partition (spatial attribute)
  inner_join(
    domainpart_grts_n2khab,
    join_by(grts_address_final == grts_address),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  # flattening year_sampling to obtain the spatial sampling units (stratum x
  # grts_address) per panel set
  summarise(
    sampling_years =
      str_flatten(sort(unique(year_sampling)), collapse = "|") %>% factor(),
    .by = c(
      scheme,
      panel_set,
      targetpanel,
      type,
      stratum,
      sp_poststratum,
      domain_part,
      grts_address,
      grts_address_final,
      last_type_assessment,
      last_type_assessment_in_field,
      last_inaccessible
    )
  ) %>%
  arrange(
    scheme,
    panel_set,
    targetpanel,
    type,
    stratum,
    sp_poststratum,
    grts_address
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
  relocate(polygon_id, sampling_years, .after = grts_address_final)


# Optionally write to Geopackage ------------------------------------------

if (FALSE) {
  gpkg_path <- file.path(
    datapath,
    "binary/results/surf_03.4_lentic_sts_cycle1_excl2190a.gpkg"
  )
  surf_03.4_lentic_sts_cycle1 %>%
    st_write(file.path(
      datapath,
      "binary/results/surf_03.4_lentic_sts_cycle1_excl2190a.gpkg"
    ))
}
