# 2025-12-05 This code was written to support reporting on pan progress. It
# is based on support_sv896.R.
#
# The results have been made with the RData file at tag poc_0.14.0, by
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

gs_id <- "1tL8OwBqE1jAyMXmN4_11aLmycRC7q4jZB-msooYp2o4"

# if needed, instead reproduce the old version using poc_0.9.0:
# load(file.path(datapath, "binary/results/objects_panflpan5_poc_0.9.0.RData"))

load(file.path(datapath, "binary/results/objects_panflpan5.RData"))

compartment_domain_count <-
  scheme_moco_ps_stratum_sppost_spsamples %>%
  filter(!str_detect(scheme, "^HQ")) %>%
  unnest(sp_poststr_samples) %>%
  inner_join(
    n2khab_strata,
    join_by(stratum),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  inner_join(
    domain_grts_n2khab %>%
      filter(domain != "SAC_network"),
    join_by(grts_address),
    relationship = "many-to-many",
    unmatched = c("error", "drop")
  ) %>%
  select(scheme, type, grts_address, domain) %>%
  mutate(
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
      fct_relevel("GW_03.3", "SURF_03.4"),
    domain = fct_recode(domain, "Totaal Vlaanderen" = "Flanders")
  ) %>%
  arrange(
    domain,
    compartment,
    scheme_simplified,
    type
  ) %>%
  # distinct in order to collapse schemes within same compartment
  distinct(
    compartment,
    scheme_simplified,
    type,
    domain,
    grts_address
  ) %>%
  count(compartment, domain) %>%
  pivot_wider(
    names_from = compartment,
    values_from = n
  )

compartment_domain_count

compartment_domain_count %>%
  write_sheet(
    ss = gs_id,
    sheet = "2026 VMM Lucht voortgangsoverleg PAS (rep_0.17.0)"
  )

