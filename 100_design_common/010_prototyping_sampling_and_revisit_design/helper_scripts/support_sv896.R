# 2025-06-16 This code was written to support INBO's answer contribution of
# sv896.
#
# The results have been made with the RData file obtained at commit 8d7b6942, by
# running:
#
# Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::html_document2",
# params = list(save_rdata = TRUE))'

# Optional: reproduce package versions with renv (change FALSE to TRUE)
reproduce_r_package_versions <- FALSE
if (reproduce_r_package_versions) {
  source("renv/activate.R") # activate renv on demand
  renv::restore() # restore package versions from renv.lock
  # we only proceed if project library is in sync with renv.lock:
  status <- renv::status()
  if (!status$synchronized) {
    stop(
      "Restoring package versions went wrong. ",
      "Please run renv::status() manually, solve problems and try again."
    )
  }
}

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(forcats)
library(sf)
library(n2khab)
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

gs_id_public <- "1vPrKmiS9OUCvuHXOS54g_kRtd2_uL6mKsM7061B7nQw"
gs_id_local <- "1iwoB38pZYYx6VcTwO4qp4rKCGObDZMIQ_6LXHFSqMbo"

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
    typeklasse = typeclass_name,
    "habitat(sub)type" = type,
    x,
    y,
    provincie = province,
    sbzh_code = sac_code,
    sbzh_naam = sac_name
  )




###########################################################################
#######       UPDATING GOOGLESHEETS         ###############################
###########################################################################

# all individual locations
simplescheme_type_samplecoords %>%
  select(-compartiment) %>%
  write_sheet(
    ss = gs_id_public,
    sheet = "meetnet_type_samplecoords"
  )

# totals by typeclass & province
simplescheme_type_samplecoords %>%
  count(compartiment, typeklasse, provincie) %>%
  split(.$compartiment) %>%
  walk(\(df) {
    tab <- str_c(df$compartiment[1], " provincies")
    df %>%
      select(-compartiment) %>%
      pivot_wider(
        names_from = typeklasse,
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
  count(compartiment, typeklasse, sbzh_code, sbzh_naam) %>%
  split(.$compartiment) %>%
  walk(\(df) {
    tab <- str_c(df$compartiment[1], " sbzh")
    crosstable <-
      df %>%
      select(-compartiment) %>%
      pivot_wider(
        names_from = typeklasse,
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

