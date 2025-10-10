# Counting the number of shallow groundwater sampling occasions

# First run setup chunk
#
# Then run:

load(file.path(datapath, "binary/results/objects_panflpan5.RData"))

single_year <- 2026

shallsamp_singleyear <-
  fag_stratum_grts_calendar %>%
  filter(
    year(date_start) == single_year,
    str_detect(field_activity_group, "SHALLSAMP")
  )

# de-duplicating 7220 since these are still present as terrestrial & aquatic
shallsamp_singleyear %>%
  distinct(stratum, grts_address, date_start)
  # (alternatively, to keep all but one columns:)
  # distinct(pick(-field_activity_group))

shallsamp_singleyear

shallsamp_singleyear %>%
  distinct(stratum, grts_address, date_start) %>%
  count(date_start)
