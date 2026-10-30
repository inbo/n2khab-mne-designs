# 2026-10-30 Inspection of sampling units for 2027 in SURF schemes, where
# wait_any is FALSE: looking at relation between last assessment year and
# whether a LOCEVAL has been planned in 2026
#
# The results have been made with the RData file at tag rep_0.18.0 (or commit
# 55e77c97), by running:
#
# Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::html_document2",
# params = list(save_rdata = TRUE))'

# First run the setup chunk.
#
# Then run the necessary chunks from support_fieldwork_organization.R at commit
# 55e77c97; the single needed object is
# fieldwork_shortterm_prioritization_by_stratum.

# all planned sampling units for DATACOLL FAGS (SURF schemes) in 2027
surf_units_2027 <-
  fieldwork_shortterm_prioritization_by_stratum %>%
  filter(
    !wait_any,
    str_detect(field_activity_group, "DATACOLL"),
    year(date_start) == 2027
  ) %>%
  distinct(grts_address, stratum, last_type_assessment)

# sampling units from surf_units_2027 that had a planned LOCEVAL occasion (all
# are in 2026), denoted with TRUE value in column loceval_kept
surf_units_2027_locevals <-
  fieldwork_shortterm_prioritization_by_stratum %>%
  filter(
    str_detect(schemes_served_all, "SURF"),
    str_detect(field_activity_group, "LOCEVAL"),
    !wait_any
  ) %>%
  semi_join(
    surf_units_2027,
    join_by(grts_address, stratum, last_type_assessment)
  ) %>%
  distinct(grts_address, stratum) %>%
  mutate(loceval_kept = TRUE)

# surf_units_2027 with two extra columns: yr_last_type_assessment, loceval_kept
surf_units_2027_extended <-
  surf_units_2027 %>%
  left_join(
    surf_units_2027_locevals,
    join_by(grts_address, stratum)
  ) %>%
  mutate(
    yr_last_type_assessment = year(last_type_assessment),
    loceval_kept = coalesce(loceval_kept, FALSE)
  )

# crosstable of n(sampling units) to show relation between
# yr_last_type_assessment & loceval_kept
surf_units_2027_extended %>%
  count(yr_last_type_assessment, loceval_kept) %>%
  pivot_wider(
    names_from = loceval_kept,
    names_prefix = "loceval_kept=",
    values_from = n,
    values_fill = 0
  )

# crosstable of number of unique GRTS addresses (i.e. different watersurfaces)
# to show relation between yr_last_type_assessment & loceval_kept
surf_units_2027_extended %>%
  summarize(
    strata = str_flatten(sort(unique(stratum)), collapse = "|"),
    yr_last_type_assessment = ifelse(
      all(is.na(yr_last_type_assessment)),
      NA,
      max(yr_last_type_assessment, na.rm = TRUE)
    ),
    loceval_kept = any(loceval_kept),
    .by = grts_address
  ) %>%
  count(yr_last_type_assessment, loceval_kept) %>%
  pivot_wider(
    names_from = loceval_kept,
    names_prefix = "loceval_kept=",
    values_from = n,
    values_fill = 0
  )
