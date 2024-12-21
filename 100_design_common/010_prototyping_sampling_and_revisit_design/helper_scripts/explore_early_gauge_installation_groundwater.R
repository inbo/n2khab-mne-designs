library(ggplot2)

# this code requires the INTERMEDIATE calendar object (1st cleaning done)

test <- scheme_moco_ps_spsubset_fag_stratum_sppost_spsamples_calendar %>%
  mutate(compartment = str_match(scheme, "([A-Z]+)_?\\d.*")[, 2] %>% factor) %>%
  mutate(
    first_date_gwinst = min(
      date_start[str_detect(field_activity_group, "GWINST")],
      ymd("21000101"),
      na.rm = TRUE
    ),
    .by = c(compartment, grts_address)
  ) %>%
  filter(
    field_activity_group == "SURFINSTGAUGE",
    compartment == "GW"
  ) %>%
  mutate(early_gauge = date_end <= first_date_gwinst)

test %>%
  filter(early_gauge) %>%
  mutate(days_too_early_gauge = day(as.period(first_date_gwinst - date_end))) %>%
  count(scheme, in_aquatic_subset, date_interval, first_date_gwinst, days_too_early_gauge) %>%
  View("delays")

test %>%
  summarize(
    number = n(),
    min_date = min(date_start),
    max_date = max(date_end),
    .by = c(scheme, module_combo_code, early_gauge)
  )

test %>%
  filter(early_gauge) %>%
  mutate(days_too_early_gauge = day(as.period(first_date_gwinst - date_end))) %>%
  count(days_too_early_gauge) %>%
  print(n = Inf)

test %>%
  filter(early_gauge) %>%
  mutate(days_too_early_gauge = day(as.period(first_date_gwinst - date_end))) %>%
  count(days_too_early_gauge) %>%
  ggplot(aes(x = days_too_early_gauge, y = n)) +
  geom_col(width = 5, alpha = 0.5) +
  ggtitle("Days not binned")

test %>%
  filter(early_gauge) %>%
  mutate(days_too_early_gauge = day(as.period(first_date_gwinst - date_end))) %>%
  ggplot(aes(x = days_too_early_gauge)) +
  geom_histogram(fill = "white", colour = "grey70", binwidth = 365/4) +
  ggtitle("Days binned")
