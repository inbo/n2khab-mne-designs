# This code is to support fieldwork optimization and not to be considered part
# of the POC workflow. Just like the other helper scripts.

# This code requires availability of the following objects:
# - scheme_fag_fa

if (Sys.getenv("GARGLE_OAUTH_EMAIL") != "") {
  options(gargle_oauth_email = Sys.getenv("GARGLE_OAUTH_EMAIL"))
}
if (Sys.getenv("GARGLE_OAUTH_CACHE") != "") {
  options(gargle_oauth_cache = Sys.getenv("GARGLE_OAUTH_CACHE"))
}

library(googlesheets4)

gs_fwopt_id <- "1wFbevEcaNbQ1RBIwEdkoloRYmarQ59E2gctbcxVyH_M"
scheme_fag_fa %>%
  distinct(field_activity_group, field_activity) %>%
  arrange(pick(everything())) %>%
  inner_join(
    field_activities,
    join_by(field_activity),
    unmatched = "drop", # change to 'error' when 'SURFLEV' activities are represented
    relationship = "many-to-one"
  ) %>%
  write_sheet(
    ss = gs_fwopt_id,
    sheet = "field_activities_2"
  )
