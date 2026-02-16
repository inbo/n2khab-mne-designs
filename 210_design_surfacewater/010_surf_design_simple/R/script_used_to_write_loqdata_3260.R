# First run the setup chunk!

# Below code is an altered form of code under
# 210_design_surfacewater/design_elaborate (by Tom De Dobbelaer) at commit
# 4b934d71 and needs the aquachem package at commit 07c8c84a

library(aquachem)
library(googledrive)

HT3260_100mseg <- read_vc(
  "HT3260_100m_segments",
  root = file.path(datapath, "raw")
) %>%
  as_tibble()

drive_download(
  as_id("1g6pRydEPskDk-ZpSVmz1Xf1osYnekEAn"),
  path = file.path(tempdir(), "watercourse_100mseg_names.gpkg")
)

geodatabase_HT3260 <-
  sf::read_sf(file.path(tempdir(), "watercourse_100mseg_names.gpkg")) %>%
  inner_join(
    HT3260_100mseg,
    join_by(rank),
    relationship = "one-to-many",
    unmatched = "drop"
  )
con <- connect_vmm()
loqdata_3260 <-
  get_chem_vmm(
    con = con,
    stream_geodatabase = TRUE,
    variable = c("NO3-", "oPO4", "oPO4 f", "NH4+"),
    buffer_stream = 30,
    guess = TRUE,
    geodatabase = geodatabase_HT3260,
    full_output = TRUE
  ) %>%
  sf::st_drop_geometry() %>%
  select(-loc_code) %>%
  rename(
    variable = variable_code,
    loc_code = rank,
    date_sampling = date
  ) %>%
  filter(
    check_name,
    date_sampling >= "2000-01-01",
    date_sampling <= "2018-12-31",
    date_sampling >= period_min,
    date_sampling <= period_max,
    !is.na(loq)
  ) %>%
  mutate(variable = case_match(
    variable,
    "oPO4" ~ "po4",
    "NO3-" ~ "no3",
    "NH4+" ~ "nh4",
    "oPO4 f" ~ "po4f"
  )) %>%
  distinct(loc_code, date_sampling, variable, unit, loq) %>%
  mutate(
    variable = factor(variable),
    unit = factor(unit)
  ) %>%
  arrange(loc_code, date_sampling, variable)

loqdata_3260 %>%
  write_vc(
    "loqdata_3260",
    root = file.path(datapath, "intermediate"),
    strict = FALSE,
    optimize = TRUE,
    sorting = c("loc_code", "date_sampling", "variable"),
    digits = 12
  )
