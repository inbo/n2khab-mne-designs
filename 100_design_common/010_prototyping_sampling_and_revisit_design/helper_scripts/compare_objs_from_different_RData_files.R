# First, run index.Rmd, then below code.

# RData file created by following shell command at tag poc_0.3.1 and then renamed.
# Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::html_document2", params = list(regenerate_binary_files = FALSE, active_modules = c("pan_effectmon_flanders", "pan_effectmon_sac5_custommeas"), update_spatiotemp_gsheet = FALSE, save_rdata = TRUE))'
prepare_lazy_get(
  file.path(datapath, "binary/results/objects_panflpan5_previous.RData"),
  new_envir_name = "panflpan5_previous"
)
# RData file created by following shell command at commit ee267a53.
# Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::html_document2", params = list(regenerate_binary_files = FALSE, active_modules = c("pan_effectmon_flanders", "pan_effectmon_sac5_custommeas"), update_spatiotemp_gsheet = FALSE, save_rdata = TRUE))'
prepare_lazy_get(
  file.path(datapath, "binary/results/objects_panflpan5.RData"),
  new_envir_name = "panflpan5"
)
# RData file created by following shell command at commit ee267a53.
# Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::html_document2", params = list(regenerate_binary_files = FALSE, active_modules = c("pan_effectmon_flanders", "pan_effectmon_sac5_custommeas"), update_spatiotemp_gsheet = FALSE, save_rdata = TRUE, phab_correct = FALSE))'
prepare_lazy_get(
  file.path(datapath, "binary/results/objects_panflpan5_nophabcorrection.RData"),
  new_envir_name = "panflpan5_nophabcorrection"
)

ls(envir = panflpan5_nophabcorrection)


# investigating effect of adding MHQ assessments (terrestrial) in the bsf ------

get("scheme_moco_ps_dom_stratum_sample_size", envir = panflpan5_nophabcorrection) %>%
  anti_join(
    get("scheme_moco_ps_dom_stratum_sample_size", envir = panflpan5_previous),
    join_by(scheme, module_combo_code, panel_set, sampling_frame_id, cycle_duration_y, domain, stratum)
  )
get("mod_dom_scheme_ssf_stratum_nunits", envir = panflpan5_nophabcorrection) %>%
  anti_join(
    get("mod_dom_scheme_ssf_stratum_nunits", envir = panflpan5_previous),
    join_by(module, domain, scheme, is_core_scheme, sampling_frame_id, stratum)
  )
get("domain_type_nunits", envir = panflpan5_nophabcorrection) %>%
  filter(type == "6510_hus")
get("domain_type_nunits", envir = panflpan5_previous) %>%
  filter(type == "6510_hus")

  # differences due to changing name of the sp_poststrata in 6510_hus:
get("scheme_moco_ps_spsubset_fas_stratum_sppost_panelmemship", envir = panflpan5_nophabcorrection) %>%
  anti_join(
    get("scheme_moco_ps_spsubset_fas_stratum_sppost_panelmemship", envir = panflpan5_previous),
    # .,
    join_by(scheme, module_combo_code, panel_set, notation_paneldesign, stratum, sp_poststratum)
  )

stratum_grts_n2khab_phabcorrected_no_replacements <-
  get("stratum_grts_n2khab_phabcorrected_no_replacements", envir = panflpan5_nophabcorrection)

get("scheme_moco_ps_stratum_dom_spsamples", envir = panflpan5_nophabcorrection) %>%
  anti_join(
    get("scheme_moco_ps_stratum_dom_spsamples", envir = panflpan5_previous),
    join_by(scheme, module_combo_code, panel_set, stratum, domain, grts_address)
  ) %>%
  add_assessment_data() %>%
  count(assessed_in_field)




# investigating the effect of phab-correction -----------------------------

# types with < 1 units in a domain can get lost:

get("domain_stratum_nunits", envir = panflpan5_nophabcorrection) %>%
  anti_join(
    get("domain_stratum_nunits", envir = panflpan5),
    join_by(domain, stratum)
  )

get("module_domain_scheme_stratum_sample_size", envir = panflpan5_nophabcorrection) %>%
  anti_join(
    get("module_domain_scheme_stratum_sample_size", envir = panflpan5),
    join_by(module, domain, scheme, stratum)
  ) %>%
  select(domain, scheme, stratum)

get("scheme_moco_ps_dom_stratum_sample_size", envir = panflpan5_nophabcorrection) %>%
  anti_join(
    get("scheme_moco_ps_dom_stratum_sample_size", envir = panflpan5),
    join_by(scheme, module_combo_code, panel_set, sampling_frame_id, cycle_duration_y, domain, stratum)
  )

# large shifts in both directions wrt locations (> 1/3), and leading to a net
# increase in number of locations after phab-correction

get("scheme_moco_ps_stratum_sppost_spsamples_sf", envir = panflpan5_nophabcorrection) %>%
  st_drop_geometry() %>%
  anti_join(
    get("scheme_moco_ps_stratum_sppost_spsamples_sf", envir = panflpan5) %>%
      st_drop_geometry(),
    .,
    join_by(scheme, module_combo_code, panel_set, stratum, sp_poststratum, grts_address)
  )






# investigating changes in MHQ schemes in pan2124 module ------------------


# RData file created by following shell command on 2025-02-12 and then renamed.
# Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::html_document2", params = list(regenerate_binary_files = FALSE, update_spatiotemp_gsheet = FALSE, save_rdata = TRUE))''
prepare_lazy_get(
  file.path(datapath, "binary/results/objects_panflpan5pan2124mbaa_previous.RData"),
  new_envir_name = "all_previous"
)
# RData file created by following shell command at tag poc_0.4.0.
# Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::html_document2", params = list(regenerate_binary_files = FALSE, update_spatiotemp_gsheet = FALSE, save_rdata = TRUE))'
prepare_lazy_get(
  file.path(datapath, "binary/results/objects_panflpan5pan2124mbaa.RData"),
  new_envir_name = "all"
)

ls(envir = all)

get("mhq_sampled_types_per_domain", envir = all_previous) %>%
  anti_join(
    get("mhq_sampled_types_per_domain", envir = all)
  )

get("module_domain_scheme_designattr", envir = all_previous) %>%
  anti_join(
    get("module_domain_scheme_designattr", envir = all),
    join_by(module, domain, scheme)
  )

get("module_domain_schemes", envir = all_previous) %>%
  anti_join(
    get("module_domain_schemes", envir = all),
    join_by(module, domain, scheme)
  )

get("mod_dom_scheme_ssf_stratum_nunits", envir = all_previous) %>%
  anti_join(
    get("mod_dom_scheme_ssf_stratum_nunits", envir = all),
    join_by(scheme)
  )

get("sp_samplingframe_domain", envir = all_previous) %>%
  pluck("sampling_frame_domain", 1) %>%
  count(domain, stratum) %>%
  filter(str_detect(stratum, "7140")) %>%
  inner_join(
    get("sp_samplingframe_domain", envir = all) %>%
      pluck("sampling_frame_domain", 1) %>%
      count(domain, stratum, name = "n_all"),
    join_by(domain, stratum)
  )

get("domain_stratum_nunits", envir = all_previous) %>%
  filter(str_detect(stratum, "7140"))

