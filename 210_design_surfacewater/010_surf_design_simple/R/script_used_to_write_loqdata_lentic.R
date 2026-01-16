# First run the setup chunk!

# Below code is an altered form of code under
# 210_design_surfacewater/design_elaborate (by Tom De Dobbelaer) at commit
# 4b934d71 and needs the aquachem package at commit 07c8c84a

library(aquachem)

aquachem <- connect_aquachem()

# tbl(aquachem, "FactResultAqua") %>% glimpse
#
# tbl(aquachem, "FactResultAqua") %>%
#   filter(IsBelowLOQ) |>
#   select(contains("Result"))

# locs_lentic <- get_locs_aquachem(aquachem)

locs_lentic <-
  read_vc("lentic_chem", root = datapath) %>%
  as_tibble() %>%
  distinct(loc_code)

lentic_chem_improved_values <-
  get_chem_aquachem(
    con = aquachem,
    locs = locs_lentic,
    variable = c("NH4", "NO3", "T.P"),
    collect = FALSE
  ) %>%
  filter(FieldSamplingDate > "1989-01-01", FieldSamplingDate < "2018-12-31") %>%
  select(
    loc_code,
    date_sampling = FieldSamplingDate,
    variable = Component,
    value = ResultFormattedNumeric,
    below_loq = IsBelowLOQ
  ) %>%
  mutate(variable = case_match(
    variable,
    "T.P" ~ "ptot",
    "NO3" ~ "no3",
    "NH4" ~ "nh4"
  )) %>%
  collect() %>%
  # only keep 'below LOQ' observations if no other are available for that
  # variable at that occasion
  filter(
    n() == 1 | (n() > 1 & all(below_loq)) | (n() > 1 & !below_loq),
    .by = c(loc_code, date_sampling, variable)
  ) %>%
  # if several values are available, those are valid observations (due to
  # previous filter) and they were aggregated in the original dataset; we follow
  # this for convenience. This only applies to a few cases.
  summarize(
    value = mean(value),
    below_loq = first(below_loq),
    .by = c(loc_code, date_sampling, variable)
  ) %>%
  mutate(
    loc_code = factor(loc_code),
    date_sampling = as_date(date_sampling),
    variable = factor(variable),
    loq = ifelse(below_loq, value, NA_real_),
    value = ifelse(below_loq, NA_real_, value)
  ) %>%
  arrange(loc_code, date_sampling, variable)

write_vc(
  lentic_chem_improved_values,
  "lentic_chem_improved_values",
  root = file.path(datapath, "intermediate"),
  sorting = c("loc_code", "date_sampling", "variable"),
  strict = FALSE,
  optimize = TRUE,
  digits = 12
)

DBI::dbDisconnect(aquachem)






















