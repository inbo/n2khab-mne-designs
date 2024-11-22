# This code is to support ad-hoc steps and building blocks in making a data
# collection plan and not to be considered part of the workflow. Just like the
# other helper scripts.

# This code requires availability of the following objects:
# - scheme_domain_stratum_spsamples_sf

scheme_stratum_spsamples_sf %>%
  write_sf(
    file.path(
      datapath,
      "binary/intermediate",
      "scheme_stratum_spsamples.gpkg"
    )
  )
