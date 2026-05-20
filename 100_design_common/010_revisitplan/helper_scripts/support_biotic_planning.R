# This code is to support biotic fieldworkplanning. It provides overviews of the
# amount of biotic fieldwork.

library(ggplot2)

# First run setup chunk
#
# Then run:

load(file.path(datapath, "binary/results/objects_panflpan5.RData"))

gs_id <- "1vqxRmWVuc39HCF15K6xZhJEa5ls7Z7uF_xsEr_swQdk"
ws_name_prefix <- str_c(str_flatten(modules$code_short), " | ")

biotic_fag_scheme_aggr <-
  fag_stratum_grts_calendar %>%
  filter(
    str_detect(field_activity_group, "LOCEVAL|LSVI"),
    year(date_start) < 2036
  ) %>%
  unnest(scheme_moco_ps) %>%
  simplify_mhq_schemes() %>%
  nest(scheme_moco_ps = c(
    scheme,
    module_combo_code,
    panel_set,
    date_start_upcoming,
    date_end_upcoming,
    is_current_occasion
  )) %>%
  mutate(
    year = year(date_start) %>% as.integer(),
    schemes = map_chr(scheme_moco_ps, function(df) {
      # nr of schemes scheduled LATER which this FAG also serves:
      n_extra <- df$scheme[!df$is_current_occasion] %>%
        unique() %>%
        length()
      df$scheme[df$is_current_occasion] %>%
        unique() %>%
        sort() %>%
        str_flatten(collapse = " | ") %>%
        {
          if (n_extra == 0) . else {
            str_c(., str_glue(" \u275a and {n_extra} later scheme(s)"))
          }
        }
    })
  )

# WRITE PIVOT TABLE TO GSHEET:
biotic_fag_scheme_aggr %>%
  count(schemes, year, field_activity_group) %>%
  arrange(year, schemes) %>%
  pivot_wider(
    names_from = c(year, field_activity_group),
    values_from = n,
    names_sort = TRUE
  ) %>%
  arrange(schemes) %>%
  write_sheet(
    ss = gs_id,
    sheet = str_c(ws_name_prefix, "biotic_FAG_planning")
  )

biotic_fag_scheme_aggr %>%
  count(field_activity_group, year) %>%
  arrange(year, field_activity_group) %>%
  pivot_wider(
    names_from = year,
    values_from = n,
    names_sort = TRUE
  )

biotic_fag_scheme_collapsed <-
  biotic_fag_scheme_aggr %>%
  mutate(
    scheme_allocation = case_when(
      str_detect(schemes, "SURF") ~ "SURF",
      str_detect(schemes, "GW") ~ "GW",
      str_detect(schemes, "SOIL") ~ "SOIL",
      str_detect(schemes, "MHQ") ~ "MHQ"
    ) %>%
      factor(levels = c("GW", "SURF", "SOIL", "MHQ"))
  )

# WRITE PIVOT TABLE TO GSHEET:
biotic_fag_scheme_collapsed %>%
  count(year, scheme_allocation, field_activity_group) %>%
  pivot_wider(
    names_from = year,
    values_from = n,
    names_sort = TRUE
  ) %>%
  arrange(scheme_allocation, field_activity_group) %>%
  write_sheet(
    ss = gs_id,
    sheet = str_c(ws_name_prefix, "biotic_FAG_planning_collapsed")
  )

# WRITE PIVOT TABLE TO GSHEET:
biotic_fag_scheme_collapsed %>%
  # when making total year counts for MHQ, we don't count the LOCEVAL FAGs as they
  # will be combined with the LSVI FAGs.
  filter(
    !(str_detect(field_activity_group, "LOCEVAL") & scheme_allocation == "MHQ")
  ) %>%
  count(scheme_allocation, year) %>%
  arrange(year, scheme_allocation) %>%
  pivot_wider(
    names_from = year,
    values_from = n,
    names_sort = TRUE
  ) %>%
  arrange(scheme_allocation) %>%
  write_sheet(
    ss = gs_id,
    sheet = str_c(ws_name_prefix, "biotic_FAG_planning_collapsed_2")
  )

biotic_fag_scheme_collapsed %>%
  count(year, scheme_allocation) %>%
  ggplot(aes(x = year, y = n, colour = scheme_allocation, group = scheme_allocation)) +
  geom_line() +
  scale_x_continuous(breaks = 2024:2035)

biotic_fag_scheme_collapsed %>%
  filter(scheme_allocation == "MHQ", year(date_start) == 2027) %>%
  distinct(grts_address, field_activity_group) %>%
  mutate(dummy = "X") %>%
  pivot_wider(names_from = field_activity_group, values_from = dummy, values_fill = "") %>%
  View()


