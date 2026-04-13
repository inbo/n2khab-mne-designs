# number of locations per scheme
scheme_moco_ps_stratum_targetpanel_spsamples %>%
  distinct(pick(-panel_set, -targetpanel)) %>%
  count(scheme)

# number of locations per compartment scheme (no double counting within
# compartment)
stratum_schemepstargetpanel_spsamples %>%
  mutate(
    GW = str_detect(scheme_ps_targetpanels, "GW"),
    SURF = str_detect(scheme_ps_targetpanels, "SURF"),
    SOIL = str_detect(scheme_ps_targetpanels, "SOIL")
  ) %>%
  summarize(across(GW:SOIL, sum))

# number of all MNE locations (no double counting between compartments)
stratum_schemepstargetpanel_spsamples %>%
  filter(str_detect(scheme_ps_targetpanels, "GW|SURF|SOIL")) %>%
  nrow()

