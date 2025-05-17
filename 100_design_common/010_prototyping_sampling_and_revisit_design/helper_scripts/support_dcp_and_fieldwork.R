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











# Preparing code for mne-monitoring repo: variable sets and FAG occasions -----


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
# - for aquatic types, see code from https://github.com/inbo/n2khab-mne-monitoring/pull/2
# - for 7220 as a whole, see code provided below
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

# cell centers of the terrestrial sampling units (excluding 7220):
units_cell_cellcenter <-
  stratum_schemetargetpanel_spsamples %>%
  filter(str_detect(sample_support_code, "cell")) %>%
  add_point_coords_grts(spatrast = grts_mh)

# sampling units as raster cells:
units_cell_rast <-
  stratum_schemetargetpanel_spsamples %>%
  filter(str_detect(sample_support_code, "cell")) %>%
  pull(grts_address_final) %>%
  # filter_grts_mh_by_address() uses the loaded grts_mh_n2khab_index object.
  # Note that the spatrast argument works equally well with grts_mh (as with the
  # defaultgrts_mh_n2khab), which we will use, since the rasters' extent and
  # resolution match.
  filter_grts_mh_by_address(grts_mh)
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

# merging strata as well for visualization:
schemetargetpanel_spsamples <-
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
schemetargetpanel_spsamples_hasgw <-
  schemetargetpanel_spsamples %>%
  mutate(has_gw = str_detect(stratum_scheme_targetpanels, "GW"))
map_all <- generate_mapview_gw(schemetargetpanel_spsamples_hasgw)
htmlwidgets::saveWidget(map_all@map, "maps/map_all.html", selfcontained = TRUE)







## Cells for local unit replacement in terrestrial types except 7220 -------

# The units that are eligible for local replacement of a specific unit are those
# cells that have the same 'level 3' GRTS address as the considered unit. The
# level 3 address is the GRTS address of the enclosing large cell (256 * 256
# quare meters; i.e. 64 level 0 units) of the coarser level3 GRTS raster.

# reading the level0-resolution SpatRaster layer that holds the level 3
# addresses
grts_mh_brick_lev3 <- read_GRTSmh(brick = TRUE)[["level3"]]
# create a spatial index of the level 3 GRTS values
grts_mh_brick_lev3_index <- tibble(
  id = seq_len(ncell(grts_mh_brick_lev3)),
  grts_address = values(grts_mh_brick_lev3)[, 1]
) %>%
  filter(!is.na(grts_address))

# generate replacement cell numbers as a list column, in order to keep the link
# between the GRTS address and the set of (usually 64) addresses in the
# enclosing level 3 cell. Beware that we must rely on grts_address if
# grts_address_final is different, so we can just use grts_address. Doing this
# for many rows takes a lot of time and might profit from 'parallel' execution.
# The result is probably best stored for efficiency.
stratum_schemetargetpanel_spsamples_replacement <-
  stratum_schemetargetpanel_spsamples %>%
  filter(str_detect(sample_support_code, "cell")) %>%
  # as an example, just do this for a few rows
  slice(2000:2009) %>%
  mutate(
    replacement_cellnrs = get_replacement_cellnrs(
      grts_address,
      spatrast = grts_mh,
      spatrast_lev3 = grts_mh_brick_lev3,
      spatrast_lev3_index = grts_mh_brick_lev3_index
    )
  )

# much, much quicker if we don't want the rowwise link between grts_address and
# the respective sets of replacement addresses, and just fetch the cell numbers
# for the whole data frame at once:
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

# an alternative to generate the link between the GRTS addresses and the
# addresses of the replacement cells, is to generate both addresses from the
# same cell numbers. The object samplingunits_replacementunits can be joined to
# the terrestrial sampling units via grts_address.
replacement_cells_grts03 <- tibble(
  grts_address_replac = grts_mh[cellnrs_replacement_integrated][, 1],
  grts_address_replac_lev3 = grts_mh_brick_lev3[cellnrs_replacement_integrated][, 1]
)
samplingunits_grts03 <- replacement_cells_grts03 %>%
  filter(
    grts_address_replac %in% (stratum_schemetargetpanel_spsamples %>%
      filter(str_detect(sample_support_code, "cell")) %>%
      pull(grts_address))
  ) %>%
  rename(grts_address = grts_address_replac)
samplingunits_replacementunits <-
  samplingunits_grts03 %>%
  inner_join(
    replacement_cells_grts03,
    join_by(grts_address_replac_lev3),
    relationship = "many-to-many",
    unmatched = "error"
  ) %>%
  select(-grts_address_replac_lev3) %>%
  nest(grts_addresses_replacement = grts_address_replac)


# SpatRaster of all above replacement cells; note the use of the cells argument:
units_cell_replacement_rast <-
  filter_grts_mh_by_address(
    spatrast = grts_mh,
    cells = cellnrs_replacement_integrated
  )







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








