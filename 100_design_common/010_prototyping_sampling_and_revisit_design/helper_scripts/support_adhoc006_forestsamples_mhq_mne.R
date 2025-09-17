# Generating a file with forest locations both in MHQ and MNE samples

# First run setup chunk
#
# Then run:

load(file.path(datapath, "binary/results/objects_panflpan5.RData"))

stratum_schemepstargetpanel_spsamples %>%
  filter(is_forest & (last_type_assessment_in_field | in_mhq_samples)) %>%
  select(
    scheme_ps_targetpanels,
    stratum,
    grts_address,
    grts_address_final,
    in_mhq_samples,
    last_type_assessment_in_field
  ) %>%
  arrange(pick(everything())) %>%
  write_vc(
    "forests_grts_overlap",
    root = file.path(datapath, "text/temp"),
    sorting = c(
      "scheme_ps_targetpanels",
      "stratum",
      "grts_address",
      "grts_address_final",
      "in_mhq_samples",
      "last_type_assessment_in_field"
    ),
    optimize = FALSE,
    digits = 6
  )

# vbi_overlaps already had its vc-file written by update_vbi_overlaps.R

