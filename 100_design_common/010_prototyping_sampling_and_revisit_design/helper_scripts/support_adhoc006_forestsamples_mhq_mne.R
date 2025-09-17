# Generating a file with forest locations both in MHQ and MNE samples

# First run setup chunk
#
# Then run:

load(file.path(datapath, "binary/results/objects_panflpan5.RData"))

grts_mh <- read_GRTSmh()
# create a spatial index of the GRTS addresses
grts_mh_index <- tibble(
  id = seq_len(ncell(grts_mh)),
  grts_address = values(grts_mh)[, 1]
) %>%
  filter(!is.na(grts_address))

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
  add_point_coords_grts(
    grts_var = "grts_address_final",
    spatrast = grts_mh,
    spatrast_index = grts_mh_index
  ) %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  relocate(x, y, .after = grts_address_final) %>%
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
      "last_type_assessment_in_field",
      "x",
      "y"
    ),
    optimize = FALSE,
    digits = 6
  )

# vbi_overlaps already had its vc-file written by update_vbi_overlaps.R

