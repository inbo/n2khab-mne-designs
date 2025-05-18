# This code is to support ad-hoc steps and building blocks in making a data
# collection plan or in preparing fieldwork and not to be considered part of the
# POC workflow. Just like the other helper scripts.



# Object wrt spatial coupling of piezometers with MNE sample --------



# This code requires availability of the following objects:
# - scheme_moco_ps_stratum_sppost_spsamples_sf
# - n2khab_strata

# First run setup chunk
#
# Then run:

load(file.path(datapath, "binary/results/objects_panflpan5.RData"))

spsamples_gw_sf_panflpan5 <-
  scheme_moco_ps_stratum_sppost_spsamples_sf %>%
  filter(str_detect(scheme, "^(GW)")) %>%
  distinct(stratum, grts_address, geometry) %>%
  # adding type metadata
  inner_join(
    n2khab_strata,
    join_by(stratum),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  inner_join(
    read_types() %>%
      select(
        type,
        hydr_class,
        hydr_class_shortname
      ),
    join_by(type),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  select(-type) %>%
  relocate(geometry, .after = last_col())

saveRDS(
  spsamples_gw_sf_panflpan5,
  file.path(datapath, "binary/intermediate/spsamples_gw_sf_panflpan5.rds")
)

write_sf(
  spsamples_gw_sf_panflpan5,
  file.path(datapath, "binary/intermediate/spsamples_gw_sf_panflpan5.gpkg")
)










# Tryout code to create data for piezometer positioning in aq types -------

# This code requires availability of the following objects:
# - scheme_moco_ps_stratum_sppost_spsamples_spares_sf
# - stratum_units_non_cell_n2khab
# - units_non_cell_n2khab_grts

# This code serves as a warmup for similar code in the n2khab-mne-monitoring
# repo, which then only needs the small RData file saved at the end (those
# objects are just from the POC, they're used as input below)

# First run setup chunk
#
# Then run:

load(file.path(datapath, "binary/results/objects_panflpan5.RData"))

n2khab_targetpops <-
  read_scheme_types() %>%
  select(scheme, type)
n2khab_types <-
  n2khab_targetpops %>%
  distinct(type) %>%
  arrange(type)

wsh <- read_watersurfaces_hab(interpreted = TRUE)
wsh_occ <-
  wsh$watersurfaces_types %>%
  # in general we restrict types using an expanded type list tailored to the
  # type levels present in data sources, but for the aquatic types expansion and
  # subsequent collapse of types are redundant steps
  semi_join(n2khab_types, join_by(type))
wsh_pol <-
  wsh$watersurfaces_polygons %>%
  semi_join(wsh_occ, join_by(polygon_id)) %>%
  select(polygon_id)


# Temporary approach to define the 3260 segments (i.e. it will miss a
# part and some may be false positives)
habstream <- read_habitatstreams()
segm_3260 <- read_watercourse_100mseg(element = "lines")[habstream, ] %>%
  unite(unit_id, vhag_code, rank)

flanders_buffer <-
  read_admin_areas(dsn = "flanders") %>%
  st_buffer(40)
habspring_units_aquatic <-
  read_habitatsprings(units_7220 = TRUE) %>%
  .[flanders_buffer, ] %>%
  filter(system_type != "mire")

stratum_units_non_cell_n2khab %>%
  inner_join(
    units_non_cell_n2khab_grts,
    join_by(sample_support_code, unit_id)
  ) %>%
  filter(
    sample_support_code %in% c(
      "watersurface",
      "watercourse_segment",
      "spring"
    ),
    unit_id %in% habspring_units_aquatic$unit_id |
      sample_support_code != "spring"
  ) %>%
  semi_join(
    scheme_moco_ps_stratum_sppost_spsamples_spares_sf %>%
      st_drop_geometry() %>%
      filter(str_detect(scheme, "^GW")),
    join_by(stratum, grts_address == grts_address_final)
  )

# we save these for usage in n2khab-mne-monitoring repo, since they take much
# code to recreate:
save(
  list = c(
    "stratum_units_non_cell_n2khab",
    "units_non_cell_n2khab_grts",
    "scheme_moco_ps_stratum_sppost_spsamples_spares_sf"
  ),
  file = file.path(
    datapath,
    "binary/intermediate/objects_for_aq_piezometers_panfl_pan5.RData"
  )
)











# Preparing code for mne-monitoring repo: samples, variables, FAG occasions ----


# First run setup chunk
#
# Then run:

load(file.path(datapath, "binary/results/objects_panflpan5.RData"))


## Sampling unit attributes -----------------------------


# attributes of spatial sampling units (~grts_address_final), useful in maps,
# selections and decisions. Note that we *identify* sampling units as stratum x
# grts_address; a unit_id is not needed provided that units don't share the same
# GRTS address (if some still do, it means that the GRTS raster is too coarse
# for those types, and will eventually need extra levels inside those specific
# cells)
scheme_moco_ps_stratum_targetpanel_spsamples <-
  scheme_moco_ps_spsubset_targetfag_stratum_sppost_spsamples_calendar %>%
  inner_join(
    n2khab_strata,
    join_by(stratum),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  inner_join(
    n2khab_types_expanded_properties %>%
      select(type, grts_join_method, sample_support_code),
    join_by(type),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  mutate(
    is_forest = str_detect(type, "^9|^2180|^rbbppm")
  ) %>%
  distinct(
    scheme,
    module_combo_code,
    panel_split,
    stratum,
    # 'aquatic' column will be improved for 7220 later on (now it simply has a
    # duplication (TRUE + FALSE) of all locations)
    is_aquatic = in_aquatic_subset,
    is_forest,
    grts_join_method,
    sample_support_code,
    grts_address,
    grts_address_final,
    targetpanel,
    last_type_assessment = assessment_date,
    last_type_assessment_in_field = assessed_in_field,
    last_inaccessible = inaccessible
  ) %>%
  arrange(pick(scheme:grts_address))

# existing sample support codes and spatial GRTS join methods:
n2khab_types_expanded_properties %>%
  distinct(grts_join_method, sample_support_code, sample_support) %>%
  arrange(grts_join_method, sample_support_code)

# with the currently active modules, module_combo_code and panel_split have a
# single unique value for each scheme. This is expected to change though in
# future (module_combo_code and panel_split do 'split' a scheme's spatial
# sample, applying different revisit designs). However we will currently take
# advantage of their uniqueness to keep things as simple as possible. Checking
# that foregoing statement is TRUE:
scheme_moco_ps_stratum_targetpanel_spsamples %>%
  distinct(scheme, module_combo_code, panel_split) %>%
  {nrow(.) == nrow(distinct(., scheme))}

# merging scheme:module_combo_code:panel_split:targetpanel, still distinguishing
# strata separately (even though they may share their location: this is unreal
# in the case of multiple cell-centered strata). For now, not distinguishing
# module_combo and panel_split as explained above.
stratum_schemetargetpanel_spsamples <-
  scheme_moco_ps_stratum_targetpanel_spsamples %>%
  select(-module_combo_code, -panel_split) %>%
  unite(scheme_targetpanel, scheme, targetpanel, sep = ":") %>%
  nest(scheme_targetpanels = scheme_targetpanel) %>%
  mutate(
    scheme_targetpanels = map_chr(scheme_targetpanels, \(df) {
      str_flatten(df$scheme_targetpanel, collapse = " | ")
    }) %>%
      factor()
  ) %>%
  relocate(scheme_targetpanels) %>%
  arrange(pick(stratum:grts_address))

# Note: if grts_address_final differs from grts_address, then this means a local
# replacement took place already in the past. If now it appears that the stratum
# is no longer present in the field, then a new replacement procedure must take
# place using grts_address as the anchor, provided that the type still occurs in
# the polygon. If not, the absence must be noted and sampling frame + sample are
# to be updated.
scheme_moco_ps_stratum_targetpanel_spsamples %>%
  filter(grts_address != grts_address_final) %>%
  glimpse









## Sampling unit geometries --------------------------------------

# obtaining geometries of the sampling units themselves:
# - for aquatic types, see code from
#   https://github.com/inbo/n2khab-mne-monitoring/pull/2, but then do use the
#   POC RData file used here
# - for type 7220 (springs) as a whole, see code provided below
# - for terrestrial types, these are cells; see code provided below


# geometries of 7220 units are represented by points, labelled with their GRTS
# address
# ////////////////////////////////////////////////////////////////////////////

flanders_buffer <-
  read_admin_areas(dsn = "flanders") %>%
  st_buffer(40)
# following function will be adapted to support the latest version of the data
# source; for now use version habitatsprings_2020v2
units_7220 <-
  read_habitatsprings(units_7220 = TRUE) %>%
  .[flanders_buffer, ] %>%
  mutate(unit_id = as.character(unit_id)) %>%
  # replacing unit_id by the grts_address
  inner_join(
    units_non_cell_n2khab_grts %>%
      filter(sample_support_code == "spring") %>%
      select(-sample_support_code),
    join_by(unit_id),
    relationship = "one-to-one",
    unmatched = c("error", "drop")
  ) %>%
  # to be solved later; a hack which looses one unit for now:
  filter(!is.na(grts_address)) %>%
  select(
    -unit_id,
    grts_address_final = grts_address
  ) %>%
  relocate(grts_address_final)


# geometries of terrestrial types, excluding 7220: these are cells
# ////////////////////////////////////////////////////////////////////////////

grts_mh <- read_GRTSmh()
# create a spatial index of the GRTS addresses
grts_mh_index <- tibble(
  id = seq_len(ncell(grts_mh)),
  grts_address = values(grts_mh)[, 1]
) %>%
  filter(!is.na(grts_address))


# cell centers of the terrestrial sampling units (excluding 7220):
units_cell_cellcenter <-
  stratum_schemetargetpanel_spsamples %>%
  filter(str_detect(sample_support_code, "cell")) %>%
  add_point_coords_grts(spatrast = grts_mh, spatrast_index = grts_mh_index)

# sampling units as raster cells:
units_cell_rast <-
  stratum_schemetargetpanel_spsamples %>%
  filter(str_detect(sample_support_code, "cell")) %>%
  pull(grts_address_final) %>%
  filter_grts_mh_by_address(spatrast = grts_mh, spatrast_index = grts_mh_index)
set.names(units_cell_rast, "grts_address_final")

# the number of non-NA cells matches the number of unique GRTS addresses
stratum_schemetargetpanel_spsamples %>%
  filter(str_detect(sample_support_code, "cell")) %>%
  distinct(grts_address_final) %>%
  nrow() %>%
  all.equal(global(units_cell_rast, "notNA") %>% as.integer())

# representing this limited number of cells as polygons: useful for plotting etc
units_cell_polygon <-
  units_cell_rast %>%
  as.polygons(aggregate = FALSE) %>%
  st_as_sf() %>%
  # to prefer the tibble approach in sf, we need to convert forth and back
  as_tibble() %>%
  # it appears that the CRS is actually retrieved from the tibble, but I don't
  # understand how (so the crs argument below isn't needed)
  st_as_sf(crs = "EPSG:31370")

# adding the sampling unit attributes to these polygons, arranged as in
# stratum_targetpanel_spsamples. Note that this duplicates cells with multiple
# strata!
units_cell_polygon_attribs <-
  units_cell_polygon %>%
  inner_join(
    stratum_schemetargetpanel_spsamples %>%
      filter(str_detect(sample_support_code, "cell")),
    join_by(grts_address_final),
    relationship = "one-to-many",
    unmatched = "error"
  ) %>%
  relocate(grts_address_final, .after = grts_address) %>%
  relocate(geometry, .after = last_col()) %>%
  arrange(pick(stratum:grts_address))

# storing some interactive maps of the extra types
generate_mapview_gw <- function(obj) {
  vals <- unique(obj$has_gw)
  cols <- if (length(vals) == 2) {
    c("pink", "blue")
  } else if (isFALSE(vals)) {
    "pink"
  } else {
    "blue"
  }
  mapview::mapview(
    obj,
    zcol = "has_gw",
    col.regions = cols,
    lwd = 2,
    map.types = c(
      "CartoDB.Positron",
      "OpenStreetMap",
      "Esri.WorldImagery",
      "OpenTopoMap"
    )
  )
}
store_stratum_map <- function(type) {
  obj <-
    units_cell_polygon_attribs %>%
    mutate(has_gw = str_detect(scheme_targetpanels, "GW")) %>%
    filter(stratum == type)
  map <- generate_mapview_gw(obj)
  htmlwidgets::saveWidget(
    map@map,
    str_c("maps/map_", type, ".html"),
    selfcontained = TRUE
  )
}
store_stratum_map("4010")
store_stratum_map("4030")
store_stratum_map("7140_oli")
store_stratum_map("6230_hmo")
store_stratum_map("9190")

# merging strata as well for visualization (where we want each row to represent
# another location):
schemetargetpanel_spsamples_terr <-
  stratum_schemetargetpanel_spsamples %>%
  filter(str_detect(sample_support_code, "cell")) %>%
  mutate(stratum_scheme_targetpanels = str_c(
    stratum,
    " (",
    grts_join_method,
    ") ",
    " [",
    scheme_targetpanels,
    "]"
  )) %>%
  mutate(
    stratum_scheme_targetpanels =
      str_flatten(stratum_scheme_targetpanels, collapse = " \u2588 ") %>%
      factor(),
    # n_strata = n(),
    .by = grts_address_final
  ) %>%
  # filter(n_strata > 1) %>%
  distinct(stratum_scheme_targetpanels, grts_address, grts_address_final) %>%
  inner_join(
    units_cell_polygon,
    .,
    join_by(grts_address_final),
    relationship = "one-to-many",
    unmatched = "error"
  ) %>%
  relocate(grts_address_final, .after = grts_address) %>%
  relocate(geometry, .after = last_col()) %>%
  arrange(stratum_scheme_targetpanels, grts_address)

# storing interactive map
schemetargetpanel_spsamples_terr_hasgw <-
  schemetargetpanel_spsamples_terr %>%
  mutate(has_gw = str_detect(stratum_scheme_targetpanels, "GW"))
map_all <- generate_mapview_gw(schemetargetpanel_spsamples_terr_hasgw)
htmlwidgets::saveWidget(map_all@map, "maps/map_all.html", selfcontained = TRUE)







## Cells for local unit replacement in terrestrial types except 7220 -------

# The units that are eligible for local replacement of a specific unit are those
# cells that have the same 'level 3' GRTS address as the considered unit, and
# belong to the same habitatmap polygon. The level 3 address is the GRTS address
# of the enclosing large cell (256 * 256 quare meters; i.e. 64 level 0 units) of
# the coarser level3 GRTS raster.

# reading the level0-resolution SpatRaster layer that holds the level 3
# addresses
grts_mh_brick_lev3 <- read_GRTSmh(brick = TRUE)[["level3"]]
# create a spatial index of the level 3 GRTS values
grts_mh_brick_lev3_index <- tibble(
  id = seq_len(ncell(grts_mh_brick_lev3)),
  grts_address = values(grts_mh_brick_lev3)[, 1]
) %>%
  filter(!is.na(grts_address))

# Generate replacement cell numbers as a list column, in order to keep the link
# between the GRTS address and the set of (usually 64) addresses in the
# enclosing level 3 cell. Beware that we must rely on grts_address if
# grts_address_final is different, so we can just use grts_address. We will
# limit these replacement cells in a later step, based on polygon IDs.
stratum_schemetargetpanel_spsamples_terr_replacementcells_unrestricted <-
  stratum_schemetargetpanel_spsamples %>%
  filter(str_detect(sample_support_code, "cell")) %>%
  mutate(
    replacement_cells = get_replacement_cellnrs(
      grts_address,
      spatrast = grts_mh,
      spatrast_lev3 = grts_mh_brick_lev3,
      spatrast_lev3_index = grts_mh_brick_lev3_index
    )
  ) %>%
  relocate(replacement_cells, .after = grts_address_final)

# If we don't need the rowwise link between grts_address and the respective sets
# of replacement addresses, we can just fetch the cell numbers for the whole
# data frame at once, which runs faster. We don't use it further, but actually
# this is what get_replacement_cellnrs() first calculates if as_list = TRUE.
cellnrs_replacement_integrated <-
  stratum_schemetargetpanel_spsamples %>%
  filter(str_detect(sample_support_code, "cell")) %>%
  pull(grts_address) %>%
  get_replacement_cellnrs(
    spatrast = grts_mh,
    spatrast_lev3 = grts_mh_brick_lev3,
    spatrast_lev3_index = grts_mh_brick_lev3_index,
    as_list = FALSE
  )

# In joining with the terrestrial sampling units, we must also take into account
# the polygon ID of the habitatmap and the stratum (because the GRTS join method
# depends on stratum). In the case of the 'cell' join method, it is possible to
# get multiple polygons attached to the same GRTS cell, provided that this
# polygon has been labelled to contain the specific type. Further, some GRTS
# addresses are from previously assessed sites with the type, while this
# information is not present in habitatmap_terr, hence also not in below used
# hmt_pol_stratum_grts_cell_all_n2khab. In these cases, we just keep all local
# replacement cells for now. Either the clip is done in the field, or the
# appropriate polygon from the habitatmap should be retrieved as well in order
# to make the clip, regardless of its label in the habitatmap.
stratum_schemetargetpanel_spsamples_terr_replacementcells <-
  stratum_schemetargetpanel_spsamples_terr_replacementcells_unrestricted %>%
  # adding polygon_id attribute (sometimes missing, sometimes more than one, as
  # explained above)
  left_join(
    hmt_pol_stratum_grts_cell_all_n2khab,
    join_by(stratum, grts_address),
    relationship = "many-to-many",
    unmatched = "drop"
  ) %>%
  # adding all GRTS addresses of the polygon, taking into account the stratum's
  # GRTS join method
  left_join(
    hmt_pol_stratum_grts_cell_all_n2khab %>%
      rename(grts_address_replac = grts_address),
    join_by(stratum, polygon_id),
    relationship = "many-to-many",
    unmatched = "drop"
  ) %>%
  # nesting polygons & addresses; the number of rows is the same as before the
  # first join above
  nest(polygon_addresses = c(polygon_id, grts_address_replac)) %>%
  # filtering replacement cells by polygon ID, unless polygon ID is missing
  mutate(
    replacement_cells = map2(
      polygon_addresses,
      replacement_cells,
      function(poladr, repladr) {
        poladr_unique <- unique(poladr$grts_address_replac)
        if (length(poladr_unique) == 1 && is.na(poladr_unique)) {
          repladr
        } else {
        repladr %>%
          filter(grts_address_replac %in% poladr_unique)
        }
      }
    )
  ) %>%
  select(-polygon_addresses)

# distribution of the number of replacement cells per sampling unit:
stratum_schemetargetpanel_spsamples_terr_replacementcells %>%
  mutate(nrcells = map_int(replacement_cells, nrow)) %>%
  pull(nrcells) %>%
  summary()

# plotting some examples using terra's plot method
plot_replacement_example <- function(nr_replacement_cells) {
  stratum_schemetargetpanel_spsamples_terr_replacementcells %>%
    mutate(nrcells = map_int(replacement_cells, nrow)) %>%
    filter(nrcells == nr_replacement_cells) %>%
    slice_sample(n = 1) %>%
    pluck("replacement_cells", 1) %>%
    pull(cellnr_replac) %>%
    {grts_mh[., drop = FALSE]} %>%
    plot()
}
plot_replacement_example(64)
plot_replacement_example(40)
plot_replacement_example(30)
plot_replacement_example(12)
plot_replacement_example(5)

# we may like to have a single vector of all replacement cell numbers
cellnrs_replacement <-
  stratum_schemetargetpanel_spsamples_terr_replacementcells %>%
  select(replacement_cells) %>%
  unnest(replacement_cells) %>%
  distinct(cellnr_replac) %>%
  pull(cellnr_replac)

# generate sf points object of all replacement cell centers
coords <- xyFromCell(grts_mh, cellnrs_replacement)
tibble(
  cellnr = cellnrs_replacement,
  grts_address = grts_mh[cellnrs_replacement][, 1],
  x = coords[, "x"],
  y = coords[, "y"]
) %>%
  st_as_sf(coords = c("x", "y"), crs = crs(grts_mh))

# SpatRaster of all replacement cells; note the use of the cells argument:
units_cell_replacement_rast <-
  filter_grts_mh_by_address(
    spatrast = grts_mh,
    spatrast_index = grts_mh_index,
    cells = cellnrs_replacement
  )
global(units_cell_replacement_rast, "notNA")[1, 1] == length(cellnrs_replacement)







## FAG occasions, field activities and variables ------------------------

# field activities (FAs) per field activity group (FAG) in the active modules
# and schemes (considered without the spatial overlap between core and non-core
# schemes). A FAG represents the field activities that must happen during the
# same location visit.
fag_fa <-
  mod_scheme_field_activity %>%
  semi_join(mod_scheme_yrs_moco_ps, join_by(module, scheme)) %>%
  distinct(field_activity_group, field_activity) %>%
  arrange(field_activity_group, field_activity)

fag_stratum_grts_calendar

# fag_stratum_grts_calendar defines the needed visits of the spatial sampling
# units and is organized at the FAG level. The rank is an indication of the
# needed order of different FAGs at one location, in the same cycle. In some
# cases repetitions do happen for certain FAGs in a scheme, not all FAGs, as
# prescribed by the date interval.

# Below code brings the FAG calendar at the resolution of each field activity.
fag_fa_stratum_grts_calendar <-
  fag_stratum_grts_calendar %>%
  inner_join(
    fag_fa,
    join_by(field_activity_group),
    relationship = "many-to-many",
    unmatched = c("error", "drop")
  ) %>%
  select(-c(typelevel_certain:inaccessible))

# Note that both calendar objects have a scheme_moco_ps column that makes clear
# which scheme x module combo x panel split the FAG is serving. This may be a
# SUBSET of the same information at the level of the spatial sampling unit
# without considering FAG occasions, since not all field activities necessarily
# serve all schemes.

# Link between field activities and their protocol
fa_protocol <-
  field_activities %>%
  inner_join(
    activities %>%
      select(activity, protocol),
    join_by(field_activity == activity),
    relationship = "one-to-one",
    unmatched = c("error", "drop")
  )

# List of variables / variable sets to be collected in the field (will expand
# when mod_scheme_vars expands)
scheme_moco_fa_fieldvar <-
  mod_scheme_vars %>%
  # bring to module combo level
  inner_join(
    mod_scheme_yrs_moco_ps %>%
      distinct(module, scheme, module_combo_code),
    join_by(module, scheme),
    relationship = "many-to-one",
    unmatched = "drop"
  ) %>%
  relocate(module_combo_code, .after = scheme) %>%
  # field activities only
  semi_join(
    field_activities,
    join_by(main_datacollection_method == field_activity)
  ) %>%
  select(
    -module,
    field_activity = main_datacollection_method
  ) %>%
  # make unique after dropping module:
  distinct(
    scheme,
    module_combo_code,
    field_activity,
    variable_set,
    # # not including variable: the (target) variable is either the same as the
    # # measurement variable, or it is an aggregated variable which we don't
    # # measure as such in the field
    # variable,
    measurement_var
  ) %>%
  # variables with the SAMP field activity are variables to be determined in the
  # lab, so not relevant for the fieldwork (but the sampling protocol is)
  filter(!str_detect(field_activity, "SAMP"))








