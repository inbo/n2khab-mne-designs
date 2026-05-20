# 2025-06-17 This code was originally written to support INBO's answer
# contribution of sv896.
#
# 2025-12-02 This code has now been redirected to a separate googlesheet with up
# to date results. It is meant to use the latest version of the RData file and
# of packages.
#

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(forcats)
library(sf)
library(n2khab)
library(n2khabmon)
library(rprojroot)
library(googlesheets4)
library(janitor)
# Setup for googledrive authentication. Set the appropriate env vars in
# .Renviron and make sure you ran drive_auth() interactively with these settings
# for the first run (or to renew an expired Oauth token).
# See ?gargle::gargle_options for more information.
if (Sys.getenv("GARGLE_OAUTH_EMAIL") != "") {
  options(gargle_oauth_email = Sys.getenv("GARGLE_OAUTH_EMAIL"))
}
if (Sys.getenv("GARGLE_OAUTH_CACHE") != "") {
  options(gargle_oauth_cache = Sys.getenv("GARGLE_OAUTH_CACHE"))
}

projroot <- find_root(is_rstudio_project)
datapath <- file.path(projroot, "data")

load(file.path(datapath, "binary/results/objects_panflpan5.RData"))

gs_id_local <- "1ABLDonmZj8KfqrMBLYZrwP5Remy2mnlDMCiBXS5Jmdg"

sac <-
  read_admin_areas(dsn = "sac") %>%
  # dissolve by sac
  group_by(sac_code, sac_name) %>%
  summarize(.groups = "drop") %>%
  st_cast()
provinces <-
  read_admin_areas(dsn = "provinces") %>%
  select(province = name) %>%
  mutate(province = factor(province, levels = province[c(1, 3, 4, 2, 5)]))

simplescheme_type_samplecoords_prepare <-
  scheme_moco_ps_stratum_sppost_spsamples_sf %>%
  filter(!str_detect(scheme, "^HQ")) %>%
  st_join(provinces) %>%
  st_join(sac) %>%
  mutate(
    x = round(st_coordinates(.)[, "X"]),
    y = round(st_coordinates(.)[, "Y"]),
    compartment = str_extract(scheme, "^[A-Z]+") %>%
      factor(levels = c("GW", "SURF", "SOIL")) %>%
      fct_recode(
        grondwater = "GW",
        oppervlaktewater = "SURF",
        bodem = "SOIL"
      ),
    scheme_simplified = fct_recode(
      scheme,
      GW_03.3 = "GW_05.1_terr",
      GW_03.3 = "GW_05.2",
      SURF_03.4 = "SURF_03.4_lentic",
      SURF_03.4 = "SURF_03.4_lotic"
    ) %>%
      fct_relevel("GW_03.3", "SURF_03.4")
  ) %>%
  st_drop_geometry() %>%
  inner_join(
    n2khab_strata,
    join_by(stratum),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  inner_join(
    read_types(lang = "nl") %>%
      select(type, typeclass_name),
    join_by(type),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  )

scheme_names <-
  read_schemes(lang = "nl") %>%
  mutate(
    scheme_name = str_match(
      scheme_name,
      ".+\\d\\.?\\d+\\s+(\\w.+)$"
    )[, 2]
  ) %>%
  select(scheme, scheme_name) %>%
  add_row(
    scheme = "SURF_03.4",
    scheme_name = "Eutrofiëring via het oppervlaktewater"
  ) %>%
  mutate(scheme = factor(
    scheme,
    levels = levels(simplescheme_type_samplecoords_prepare$scheme_simplified)
  ))

simplescheme_type_samplecoords <-
  simplescheme_type_samplecoords_prepare %>%
  inner_join(
    scheme_names,
    join_by(scheme_simplified == scheme),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  arrange(
    compartment,
    scheme_simplified,
    typeclass_name,
    type,
    province,
    sac_code
  ) %>%
  # distinct in order to collapse schemes within same compartment
  distinct(
    compartiment = compartment,
    meetnet = scheme_name,
    habitattypeklasse = typeclass_name,
    "habitat(sub)type" = type,
    x,
    y,
    provincie = province,
    sbzh_code = sac_code,
    sbzh_naam = sac_name
  )




###########################################################################
#######       UPDATING GOOGLESHEET          ###############################
###########################################################################

# totals by typeclass & province
simplescheme_type_samplecoords %>%
  count(compartiment, habitattypeklasse, provincie) %>%
  split(.$compartiment) %>%
  walk(\(df) {
    tab <- str_c(df$compartiment[1], " provincies")
    df %>%
      select(-compartiment) %>%
      pivot_wider(
        names_from = habitattypeklasse,
        values_from = n
      ) %>%
      arrange(provincie) %>%
      adorn_totals(where = c("row", "col"), name = "Totaal") %>%
      as_tibble() %>%
      write_sheet(
        ss = gs_id_local,
        sheet = tab
      )
  })

# totals by typeclass & sac
simplescheme_type_samplecoords %>%
  count(compartiment, habitattypeklasse, sbzh_code, sbzh_naam) %>%
  split(.$compartiment) %>%
  walk(\(df) {
    tab <- str_c(df$compartiment[1], " sbzh")
    crosstable <-
      df %>%
      select(-compartiment) %>%
      pivot_wider(
        names_from = habitattypeklasse,
        values_from = n
      ) %>%
      arrange(sbzh_code)
    # create subtotals within vs outside sac as separate table
    summarytable <-
      crosstable %>%
      mutate(in_sac = ifelse(
        is.na(sbzh_code),
        "Totaal buiten SBZ-H",
        "Totaal binnen SBZ-H"
      )) %>%
      summarize(
        across(!matches("sbzh"), \(x) sum(x, na.rm = TRUE)),
        .by = in_sac
      ) %>%
      rename(sbzh_code = in_sac) %>%
      mutate(sbzh_naam = "") %>%
      relocate(sbzh_naam, .after = sbzh_code) %>%
      adorn_totals(where = "row", name = "Totaal", fill = "")
    # combine both tables
    crosstable %>%
      filter(!is.na(sbzh_code)) %>%
      bind_rows(summarytable) %>%
      adorn_totals(where = "col", name = "Totaal") %>%
      as_tibble() %>%
      write_sheet(
        ss = gs_id_local,
        sheet = tab
      )
  })

