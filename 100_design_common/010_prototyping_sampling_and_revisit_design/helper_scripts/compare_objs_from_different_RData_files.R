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

# investigating the effect of adding MHQ assessments (terrestrial) in the sampling frame

get("scheme_moco_ps_dom_stratum_sample_size", envir = panflpan5_nophabcorrection) %>%
  anti_join(
    get("scheme_moco_ps_dom_stratum_sample_size", envir = panflpan5_previous),
    join_by(scheme, module_combo_code, panel_split, sampling_frame_id, cycle_duration_y, domain, stratum)
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
    join_by(scheme, module_combo_code, panel_split, notation_paneldesign, stratum, sp_poststratum)
  )

stratum_grts_n2khab_phabcorrected_no_replacements <-
  get("stratum_grts_n2khab_phabcorrected_no_replacements", envir = panflpan5_nophabcorrection)

get("scheme_moco_ps_stratum_dom_spsamples", envir = panflpan5_nophabcorrection) %>%
  anti_join(
    get("scheme_moco_ps_stratum_dom_spsamples", envir = panflpan5_previous),
    join_by(scheme, module_combo_code, panel_split, stratum, domain, grts_address)
  ) %>%
  add_assessment_data() %>%
  count(assessed_in_field)
