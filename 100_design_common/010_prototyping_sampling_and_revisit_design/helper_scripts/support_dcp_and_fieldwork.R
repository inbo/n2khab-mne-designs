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

# Note: if grts_address_final differs from grts_address, then this means a local
# replacement took place already in the past. If now it appears that the stratum
# is no longer present in the field, then a new replacement procedure must take
# place using grts_address as the anchor, provided that the type still occurs in
# the polygon. If not, the absence must be noted and sampling frame + sample are
# to be updated.

# existing sample support codes and spatial GRTS join methods:
n2khab_types_expanded_properties %>%
  distinct(grts_join_method, sample_support_code, sample_support) %>%
  arrange(grts_join_method, sample_support_code)

# obtaining geometries of the sampling units themselves:
# - for aquatic types, see code from https://github.com/inbo/n2khab-mne-monitoring/pull/2
# - for 7220 as a whole, see code provided below
# - for terrestrial types, these are cells; see code provided below


# geometries of 7220 units are represented by points, labelled with their GRTS
# address
# =========================================================================
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
# =================================================================
grts_mh <- read_GRTSmh()
scheme_moco_ps_stratum_targetpanel_spsamples %>%
  pull(grts_address_final) %>%
  # filter_grts_mh_by_address() uses the loaded grts_mh_n2khab_index object.
  # Note that the spatrast argument works equally well with grts_mh (as with the
  # defaultgrts_mh_n2khab), which we will use, since the rasters' extent and
  # resolution match.
  filter_grts_mh_by_address(grts_mh)





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








