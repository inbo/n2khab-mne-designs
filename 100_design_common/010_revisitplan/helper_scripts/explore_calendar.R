
# Exploring the nature of yearly changes of number of FAGs ---------------------


# fag <- "GWSHALLSAMPREADMAN"
# notat <- "12panelsof3mfor6y(SER1y-SERby2repeat2)[shift(6m)]"
fag <- "LOCEVALTERR"
notat <- "6panelsof1y(SER)[shift(-1y)]"
sch <- "GW_03.3"
cycle <- 1
yr_min <- function(cy = cycle) 2024 + 6 * (cy - 1)
yr_max <- function(cy = cycle) 2029 + 6 * (cy - 1)

# total number of occasions of the chosen FAG in the final calendar
fag_stratum_grts_calendar %>%
  filter(
    field_activity_group == fag,
    between(year(date_start), yr_min(), yr_max())
  ) %>%
  # unnest(scheme_moco_ps) %>%
  # filter(scheme == sch) %>%
  mutate(year = year(date_start) %>% as.integer()) %>%
  count(year)

# year-to-year change in number of occasions, per stratum
fag_stratum_grts_calendar %>%
  filter(
    field_activity_group == fag,
    between(year(date_start), yr_min(), yr_max())
  ) %>%
  # unnest(scheme_moco_ps) %>%
  # filter(scheme == sch) %>%
  mutate(year = year(date_start) %>% as.integer()) %>%
  count(year, stratum) %>%
  ggplot(aes(x = year, y = n, group = stratum)) +
  geom_line() +
  facet_wrap(~stratum, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4))



# total number of locations per panel, within sch x TERR
scheme_moco_ps_spsubset_fas_stratum_sppost_panelmemship %>%
  filter(scheme == sch, notation_paneldesign == notat) %>%
  unnest(panel_membership) %>%
  filter(!str_detect(panel, "AQ")) %>%
  count(panel)

# total number of locations per panel and per stratum, within sch x TERR
scheme_moco_ps_spsubset_fas_stratum_sppost_panelmemship %>%
  filter(scheme == sch, notation_paneldesign == notat) %>%
  unnest(panel_membership) %>%
  filter(!str_detect(panel, "AQ")) %>%
  count(panel, stratum) %>%
  ggplot(aes(x = panel, y = n, group = stratum)) +
  geom_line() +
  facet_wrap(~stratum, scales = "free_y") +
  theme(axis.text.x = element_blank())

# total number of occasions in calendar before filtering superfluous occasions,
# within sch x TERR: identical results to panel size (or x2 in case of 2
# location visits per cycle):
scheme_moco_ps_spsubset_fag_stratum_sppost_spsamples_calendar %>%
  filter(
    scheme == sch,
    notation_paneldesign == notat,
    !str_detect(panel, "AQ"),
    between(year(date_start), yr_min(), yr_max())
  ) %>%
  count(panel)

# same conclusion per stratum(within sch x TERR)
scheme_moco_ps_spsubset_fag_stratum_sppost_spsamples_calendar %>%
  filter(
    scheme == sch,
    notation_paneldesign == notat,
    !str_detect(panel, "AQ"),
    between(year(date_start), yr_min(), yr_max())
  ) %>%
  count(panel, stratum) %>%
  ggplot(aes(x = panel, y = n, group = stratum)) +
  geom_line() +
  facet_wrap(~stratum, scales = "free_y") +
  theme(axis.text.x = element_blank())


# evolution before filtering out superfluous visits

fag_stratum_grts_calendar_cycleid %>%
  filter(
    stratum == "7140_oli",
    field_activity_group == fag,
    between(year(date_start), yr_min(), yr_max())
  ) %>%
  # unnest(scheme_moco_ps) %>%
  # filter(scheme == sch) %>%
  mutate(year = year(date_start) %>% as.integer()) %>%
  count(year)

fag_stratum_grts_calendar_cycleid %>%
  filter(
    field_activity_group == fag,
    between(year(date_start), yr_min(), yr_max())
  ) %>%
  # unnest(scheme_moco_ps) %>%
  # filter(scheme == sch) %>%
  mutate(year = year(date_start) %>% as.integer()) %>%
  count(year, stratum) %>%
  ggplot(aes(x = year, y = n, group = stratum)) +
  geom_line() +
  facet_wrap(~stratum, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4))



# evolution after filtering out superfluous visits

test <-
  fag_stratum_grts_calendar_cycleid %>%
  mutate(
    n_visits = n(),
    repeated = fag_is_auxiliary &
      n_visits > n_locvisits_per_fagcycle,
    keep = !fag_is_auxiliary |
      n_visits <= n_locvisits_per_fagcycle |
      # we retain the first unique date_start values, notably the number of
      # expected location visits in the FAG cycle, when more visits remain than
      # expected. Exploration showed that this can apply to a wide range of
      # FAGs.
      date_start %in% head(sort(unique(date_start)), first(n_locvisits_per_fagcycle)),
    .by = c(
      stratum,
      grts_address,
      field_activity_group,
      fag_cycle,
      n_locvisits_per_fagcycle,
      cycle_id
    )
  )

test %>%
  filter(
    keep,
    stratum == "7140_oli",
    field_activity_group == fag,
    between(year(date_start), yr_min(), yr_max())
  ) %>%
  # unnest(scheme_moco_ps) %>%
  # filter(scheme == sch) %>%
  mutate(year = year(date_start) %>% as.integer()) %>%
  count(year)

test %>%
  filter(
    keep,
    field_activity_group == fag,
    between(year(date_start), yr_min(), yr_max())
  ) %>%
  # unnest(scheme_moco_ps) %>%
  # filter(scheme == sch) %>%
  mutate(year = year(date_start) %>% as.integer()) %>%
  count(year, stratum) %>%
  ggplot(aes(x = year, y = n, group = stratum)) +
  geom_line() +
  facet_wrap(~stratum, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4))



# cross-table of kept occasions, in the case of 2 integrated schemes

test %>%
  filter(
    keep,
    stratum == "7140_oli",
    field_activity_group == fag,
    between(year(date_start), yr_min(), yr_max())
  ) %>%
  count(year(date_start))
  unnest(scheme_moco_ps) %>%
  mutate(year = year(date_start)) %>%
  select(grts_address, scheme, year) %>%
  pivot_wider(names_from = scheme, values_from = year) %>%
  count(GW_03.3, SOIL_03.2) %>%
  pivot_wider(names_from = SOIL_03.2, values_from = n, names_sort = TRUE) %>%
  arrange(1)






# Exploring panel membership of autocontinuous measurements -----------

scheme_moco_ps_spsubset_fag_stratum_sppost_spsamples_calendar %>%
  filter(
    panel == "GW_03.3_TERR_panflpan5_PS1_2panelsof3m(fastalign|SER)[shift(6m)]_PANEL01",
    date_start == make_date(2025, 07, 01)
  ) %>%
  select(stratum, grts_address) %>%
  semi_join(
    scheme_moco_ps_spsubset_fag_stratum_sppost_spsamples_calendar %>%
      filter(
        notation_paneldesign == "24panelsof3m(SER)",
        year(date_start) == 2025
      ),
    .,
    join_by(stratum, grts_address)
  ) %>%
  count(panel)
# so, everything from readpanel 1 that is installed in 2025, is in installpanels
# 5 or 7

# Why are these not covered in panel 01? Are they in panel 02? Or panel set 2?
scheme_moco_ps_spsubset_fag_stratum_sppost_spsamples_calendar %>%
  filter(
    panel == "GW_03.3_panflpan5_PS1_24panelsof3m(SER)_PANEL05",
    date_start == make_date(2025, 07, 01)
  ) %>%
  select(stratum, grts_address) %>%
  anti_join(
    scheme_moco_ps_spsubset_fag_stratum_sppost_spsamples_calendar %>%
      filter(str_detect(
        panel, "GW_03.3_(TERR|AQ)_panflpan5_PS1_2panelsof3m\\(fastalign.+_PANEL01"
      )),
    join_by(stratum, grts_address)
  )
# so, everything from installpanel 5 in 2025 is in readpanel 1

