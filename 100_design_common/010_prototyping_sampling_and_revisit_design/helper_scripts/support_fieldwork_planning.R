# This code is to support fieldworkplanning.

library(ggplot2)



# Counting FAG occasions with LOCEVAL & LSVI activities in 1st cycle --------


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
  left_join(mhq_scheme_category, by = "scheme") %>%
  mutate(
    is_mhq = str_detect(scheme, "^HQ"),
    scheme = ifelse(is_mhq, str_c("MHQ_", category), as.character(scheme)) %>%
      factor(levels = c(
        levels(schemes$scheme),
        "MHQ_terrestrial_open",
        "MHQ_terrestrial_forest",
        "MHQ_lentic",
        "MHQ_lotic"
      ))
  ) %>%
  select(-is_mhq, -category) %>%
  nest(scheme_moco_ps = c(scheme, module_combo_code, panel_split)) %>%
  mutate(
    year = year(date_start) %>% as.integer(),
    schemes = map_chr(scheme_moco_ps, function(df) {
      df$scheme %>%
        unique() %>%
        sort() %>%
        str_flatten(collapse = " | ")
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
      str_detect(schemes, "GW") ~ "GW",
      str_detect(schemes, "SURF") ~ "SURF",
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








# Inspection of specific sample sizes -------------------------------------

generate_sample_size_table <- function(df) {
  df %>%
    summarize(
      nunits = sum(nunits),
      .by = c(
        module,
        domain,
        scheme,
        cycle_duration_y,
        type,
        sp_sample_size_all_panels_type
      )
    ) %>%
    mutate(
      yearly_sample_size = round(sp_sample_size_all_panels_type / cycle_duration_y, 1),
      fraction_sampled = sp_sample_size_all_panels_type / nunits,
      rel_abs_size = str_c(round(fraction_sampled, 3), " (", sp_sample_size_all_panels_type, ")")
    ) %>%
    select(
      module,
      domain,
      scheme,
      type,
      rel_abs_size
    ) %>%
    arrange(module, scheme, domain, type) %>%
    pivot_wider(
      names_from = domain,
      values_from = rel_abs_size,
      names_sort = TRUE
    )
}

module_domain_scheme_stratum_sample_size %>%
  filter(
    type %in% c("4010", "4030", "6230_hmo", "7140_oli", "9190"),
    scheme %in% c("GW_03.3", "SOIL_03.2")
  ) %>%
  generate_sample_size_table()

module_domain_scheme_stratum_sample_size %>%
  filter(
    !(type %in% c("4010", "4030", "6230_hmo", "7140_oli", "9190")),
    scheme %in% c("GW_03.3", "SOIL_03.2")
  ) %>%
  nest(.by = type) %>%
  slice_sample(n = 10) %>%
  unnest(data) %>%
  generate_sample_size_table() %>%
  print(n = Inf)


