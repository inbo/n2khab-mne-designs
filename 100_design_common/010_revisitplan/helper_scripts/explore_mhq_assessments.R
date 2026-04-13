# First run setup chunk
#
# Then run:

load(file.path(datapath, "binary/results/objects_panflpan5.RData"))
hmt <- read_habitatmap_terr(keep_aq_types = FALSE, drop_7220 = TRUE)
wsh <- read_watersurfaces_hab(interpreted = TRUE)
grts_mh <- read_GRTSmh()



# TERRESTRIAL -------------------------------------------------------------

mhq_terr_datapath <- file.path(dirname(gitroot), "n2khab-sample-admin/data/mhq_terr/rapportage2025")

mhq_terr_assessments <-
  read_vc("mhq_terr_assessments", root = mhq_terr_datapath) %>%
  as_tibble()
mhq_terr_refpoints <-
  read_vc("mhq_terr_refpoints", root = mhq_terr_datapath) %>%
  as_tibble()
mhq_terr_measurements <-
  read_vc("mhq_terr_measurements", root = mhq_terr_datapath) %>%
  as_tibble()
mhq_terr_popunits <-
  read_vc("mhq_terr_popunits", root = mhq_terr_datapath) %>%
  as_tibble()

glimpse(mhq_terr_popunits)
glimpse(mhq_terr_refpoints)
glimpse(mhq_terr_assessments)
glimpse(mhq_terr_measurements)


# mhq_terr_popunits -------------------------------------------------------
# BASED ON HABITATMAP + ASSESSMENTS
# This dataset has all information needed to amend the sampling frame

# point_code not unique --> multiple types on the same cell center?
mhq_terr_popunits$point_code %>% {n_distinct(.) == length(.)}
mhq_terr_popunits$point_code %>% n_distinct()

mhq_terr_popunits %>%
  count(point_code) %>%
  filter(n > 1) %>%
  semi_join(mhq_terr_popunits, ., join_by(point_code))

# point_code is unique when only considering assessment as a source.
mhq_terr_popunits %>%
  filter(str_detect(source, "assessment")) %>%
  count(point_code) %>%
  filter(n > 1) %>%
  nrow() == 0

# point_code consistent with combination of grts_ranking x grts_ranking_draw
mhq_terr_popunits %>%
  distinct(point_code, grts_ranking, grts_ranking_draw) %>%
  nrow() == n_distinct(mhq_terr_popunits$point_code)

# number & nature of legacy sites (~ 20%) XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
mhq_terr_popunits %>% filter(legacy_site) %>% nrow()
mhq_terr_popunits %>%
  filter(legacy_site) %>%
  count(type, sort = TRUE) %>%
  print(n = Inf)

# grts_ranking empty
mhq_terr_popunits %>% filter(is.na(grts_ranking_draw)) %>% nrow() # bosinventarisatie
mhq_terr_popunits %>% filter(is.na(grts_ranking)) %>% nrow()

mhq_terr_popunits %>%
  filter(is.na(grts_ranking_draw)) %>%
  count(legacy_site)

# all points from 'bosinventarisatie' are not labelled as cell centroid
mhq_terr_popunits %>%
  filter(is.na(grts_ranking_draw)) %>%
  inner_join(
    mhq_terr_refpoints,
    join_by(point_code),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  count(is_centroid)

# all legacy sites are not labelled as cell centroid
mhq_terr_popunits %>%
  filter(legacy_site) %>%
  inner_join(
    mhq_terr_refpoints,
    join_by(point_code),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  count(is_centroid)

# grts_ranking kept
mhq_terr_popunits %>%
  filter(!is.na(grts_ranking_draw), grts_ranking_draw == grts_ranking) %>%
  nrow()

# grts_ranking_kept x legacy_site
mhq_terr_popunits %>%
  mutate(grts_ranking_kept = !is.na(grts_ranking_draw) & grts_ranking_draw == grts_ranking) %>%
  count(grts_ranking_kept, legacy_site)

# check types
all(mhq_terr_popunits$type %in% read_types()$type)

# hmt misses some polygon_ids that appear member of the target population
not_in_hmt <-
  mhq_terr_popunits %>%
  mutate(
    in_hmt = polygon_id %in% hmt$habitatmap_terr_polygons$polygon_id
  ) %>%
  filter(!in_hmt)
not_in_hmt %>% count(type)
not_in_hmt %>% count(source) # most are from 'assessment only' XXXXXXXXXXXXXXXXXXXXXXXXXX


# explore
mhq_terr_popunits %>% count(source)
mhq_terr_popunits$phab %>% summary()




# mhq_terr_refpoints ------------------------------------------------------

mhq_terr_refpoints %>% count(is_centroid)
all(mhq_terr_popunits$point_code %in% mhq_terr_refpoints$point_code)
allgrts <- values(grts_mh)
allgrts <- allgrts[!is.na(allgrts)]
all(mhq_terr_refpoints$grts_ranking %in% allgrts)

mhq_terr_popunits %>%
  inner_join(
    mhq_terr_refpoints,
    join_by(point_code, grts_ranking),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  invisible()

# different point codes can be used at the same GRTS address
mhq_terr_refpoints %>%
  filter(grts_ranking == 1466998)

# but this is also the case for centroid points
mhq_terr_refpoints %>%
  filter(is_centroid) %>%
  count(grts_ranking) %>%
  filter(n > 1)

# there are row duplicates
mhq_terr_refpoints %>%
  distinct() %>%
  nrow() == nrow(mhq_terr_refpoints)

# so different point codes can be in use with the same coordinates
mhq_terr_refpoints %>%
  distinct() %>%
  count(x, y) %>%
  filter(n > 1)


# mhq_terr_assessments ----------------------------------------------------

# some unexpected combinations? XXXXXXXXXXXXXXXX
# - some have assessment_source NA
# - no_habitat = !is_present & field_assessment  ? NA = we don't know. Only TRUE (-> to exclude, except for RIB) or NA are relevant
mhq_terr_assessments %>%
  count(
    is_present,
    no_habitat,
    assessment_source,
    inaccessible,
    not_measurable,
    sort = TRUE
  ) %>%
  mutate(pct = n / sum(n) * 100)

mhq_terr_assessments$assessment_date %>% n_distinct()

# number of repetitions XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# old latest assessment years: how to deal with this?
mhq_terr_assessments %>%
  mutate(assessment_year = year(assessment_date)) %>%
  summarize(
    n_repeated = n(),
    last_assessment_year = max(assessment_year),
    .by = c(point_code)
  ) %>%
  count(n_repeated, last_assessment_year) %>%
  pivot_wider(names_from = n_repeated, values_from = n, values_fill = 0)

# contradictions between changed GRTS address and change_location?
# no, see https://github.com/inbo/n2khab-sample-admin/issues/46
assessment_replacement <-
  mhq_terr_assessments %>%
  select(assessment_date, point_code, type, is_present, change_location) %>%
  inner_join(
    mhq_terr_popunits %>%
      filter(str_detect(source, "assessment")) %>%
      select(point_code, grts_ranking, grts_ranking_draw, type),
    join_by(point_code, type),
    relationship = "many-to-one",
    unmatched = c("drop", "error")
  ) %>%
  mutate(local_replacement = grts_ranking != grts_ranking_draw) %>%
  relocate(local_replacement, .before = grts_ranking)

assessment_replacement %>%
  count(is_present, change_location, local_replacement) %>%
  filter(change_location != local_replacement)

assessment_replacement %>%
  filter(change_location != local_replacement) %>%
  semi_join(assessment_replacement, ., join_by(point_code)) %>%
  arrange(grts_ranking, grts_ranking_draw, type, point_code) %>%
  print(n = 25)


# change_location should not have 'is_centroid'. Checking it.
mhq_terr_assessments %>%
  inner_join(
    mhq_terr_refpoints,
    join_by(point_code),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  count(is_centroid, change_location)
# however this isn't TRUE, see https://github.com/inbo/n2khab-sample-admin/issues/46

# contrasting change_location, local_replacement and is_centroid
assessment_popunits_points <-
  assessment_replacement %>%
  rename(grts_address_shift = local_replacement) %>%
  filter(is_present) %>%
  inner_join(
    mhq_terr_refpoints %>%
      select(point_code, is_centroid),
    join_by(point_code),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  )

assessment_popunits_points %>%
  filter(change_location) %>%
  count(change_location, is_centroid, grts_address_shift)

assessment_popunits_points %>%
  filter(change_location, is_centroid, !grts_address_shift)

mhq_terr_popunits %>%
  filter(point_code == "101169_1")

# contrasting change_location and grts_address_shift
assessment_popunits_points %>%
  filter(grts_address_shift) %>%
  count(grts_address_shift, change_location)

assessment_popunits_points %>%
  filter(grts_address_shift, !change_location) %>%
  count(grts_address_shift, change_location, is_centroid)





# recreate object to also contain the legacy_site attribute
assessment_popunits_points_legacystatus <-
  mhq_terr_assessments %>%
  select(assessment_date, point_code, type, is_present, change_location) %>%
  inner_join(
    mhq_terr_popunits %>%
      filter(str_detect(source, "assessment")) %>%
      select(point_code, grts_ranking, grts_ranking_draw, type, legacy_site),
    join_by(point_code, type),
    relationship = "many-to-one",
    unmatched = c("drop", "error")
  ) %>%
  filter(is_present) %>%
  mutate(grts_address_shift = grts_ranking != grts_ranking_draw) %>%
  relocate(grts_address_shift, .before = grts_ranking) %>%
  inner_join(
    mhq_terr_refpoints %>%
      select(point_code, is_centroid),
    join_by(point_code),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  )

assessment_popunits_points_legacystatus %>%
  filter(grts_address_shift, !change_location) %>%
  count(grts_address_shift, change_location, legacy_site, is_centroid)

assessment_popunits_points_legacystatus %>%
  filter(grts_address_shift, !change_location, !legacy_site) %>%
  count(type, sort = TRUE)


# some assessed locations in mhq_terr_popunits seem to be absent from mhq_terr_assessments
#
mhq_terr_popunits %>%
  filter(str_detect(source, "assessment")) %>%
  select(point_code, grts_ranking, grts_ranking_draw, type, source) %>%
  anti_join(
    mhq_terr_assessments %>%
      select(assessment_date, point_code, type, is_present) %>%
      filter(is_present),
    join_by(point_code, type)
  )


# negative observations in mhq_terr_assessments that appear in mhq_terr_popunits

mhq_terr_assessments %>%
  filter(assessment_date == max(assessment_date), .by = point_code) %>%
  filter(!is_present) %>%
  select(point_code, assessment_date, is_present, type) %>%
  inner_join(
    mhq_terr_popunits %>%
      select(point_code, grts_ranking_draw, grts_ranking, type, source),
    join_by(point_code, type),
    relationship = "many-to-many",
    unmatched = "drop"
  ) %>%
  arrange(type, grts_ranking_draw)




# mhq_terr_measurements ---------------------------------------------------

mhq_terr_measurements %>%
  mutate(measurement_year = year(measurement_date)) %>%
  count(measurement_year)














# WATERSURFACES -----------------------------------------------------------

mhq_watersurfaces_datapath <- file.path(dirname(gitroot), "n2khab-sample-admin/data/mhq_watersurfaces/rapportage2025")

mhq_watersurfaces_populationunits <-
  read_vc("mhq_watersurfaces_populationunits", root = mhq_watersurfaces_datapath) %>%
  as_tibble()
mhq_watersurfaces_assessments <-
  read_vc("mhq_watersurfaces_assessments", root = mhq_watersurfaces_datapath) %>%
  as_tibble()

glimpse(mhq_watersurfaces_populationunits)
glimpse(mhq_watersurfaces_assessments)

# mhq_watersurfaces_populationunits ---------------------------------------

# is type up to date ? XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXx

# sampling_unit_code not unique
mhq_watersurfaces_populationunits$sampling_unit_code %>% {n_distinct(.) == length(.)}

mhq_watersurfaces_populationunits %>%
  count(source)

# sampling_unit_code x type is unique
mhq_watersurfaces_populationunits %>%
  count(sampling_unit_code, type) %>%
  filter(n > 1) %>%
  nrow() == 0

# sampling_unit_code = polygon_id
mhq_watersurfaces_populationunits %>%
  filter(sampling_unit_code != polygon_id)

# 232 of 3091 (7.5%) have been shifted ? XXXXXXXXXXXXXXXX
mhq_watersurfaces_populationunits %>%
  filter(grts_ranking != grts_ranking_draw) %>%
  count(type, source)




# mhq_watersurfaces_assessments -------------------------------------------

# by polygon_id, not sampling_unit_code (but they're the same)

# combinations
mhq_watersurfaces_assessments %>%
  count(
    is_present,
    any_habitat,
    any_type,
    assessment_source,
    inaccessible,
    sort = TRUE
  ) %>%
  mutate(pct = n / sum(n) * 100)


# number of repetitions XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# old latest assessment years: how to deal with this?
mhq_watersurfaces_assessments %>%
  mutate(assessment_year = year(assessment_date)) %>%
  summarize(
    n_repeated = n(),
    last_assessment_year = max(assessment_year),
    .by = c(polygon_id)
  ) %>%
  count(n_repeated, last_assessment_year) %>%
  pivot_wider(names_from = n_repeated, values_from = n, values_fill = 0)


