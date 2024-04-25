collapse_strata <- function(df) {
  df %>%
    mutate(
      stratum = case_match(
        stratum,
        "5130_hei" ~ "5130",
        "5130_kalk" ~ "5130",
        "rbbkam+" ~ "rbbkam",
        "rbbvos+" ~ "rbbvos",
        "rbbzil+" ~ "rbbzil",
        "9120_qb" ~ "9120",
        .default = stratum
      )
    ) %>%
    left_join(
      tribble(
        ~main_type, ~subtype,
        "2330", "2330_bu",
        "2330", "2330_dw",
        "6230", "6230_ha",
        "6230", "6230_hmo",
        "6230", "6230_hn",
        "91E0", "91E0_va",
        "91E0", "91E0_vm",
        "91E0", "91E0_vn"
      ),
      by = c("stratum" = "main_type"),
      relationship = "many-to-many"
    ) %>%
    mutate(
      stratum = ifelse(is.na(subtype), stratum, subtype) %>%
        as.character %>%
        factor(levels = levels(n2khab_strata_expanded$stratum))
    ) %>%
    select(-subtype)
}


aggregate_sample_size <- function(df, sample_size_all_panels_var, mhq_scheme_category) {
    df %>%
    mutate(yearly_sample_size = .data[[sample_size_all_panels_var]] / cycle_duration_y) %>%
    summarize(
      yearly_sample_size = sum(yearly_sample_size, na.rm = TRUE),
      .by = c(module, scheme)
    ) %>%
    filter(!is.na(yearly_sample_size), yearly_sample_size > 0) %>%
    arrange(module, scheme) %>%
    left_join(mhq_scheme_category, by = "scheme") %>%
    mutate(
      is_mhq = str_detect(scheme, "^HQ"),
      scheme_aggr = ifelse(is_mhq, str_c("MHQ_", category), as.character(scheme))
    ) %>%
    summarize(
      yearly_sample_size = sum(yearly_sample_size) %>% round() %>% as.integer(),
      .by = c(module, scheme_aggr)
    )
}


#' Add point coordinate columns to a data frame with a GRTS address column
add_point_coords_grts <- function(
    df,
    grts_var = "grts_address",
    spatrast = grts_mh_n2khab,
    spatial = TRUE) {

  addresses <- df %>%
    distinct(.data[[grts_var]]) %>%
    pull(.data[[grts_var]]) %>%
    sort()

  grts_cells <- grts_mh_n2khab_index %>%
    filter(grts_address %in% addresses) %>%
    arrange(grts_address) %>%
    pull(id)

  coords <- xyFromCell(spatrast, grts_cells)

  df %>%
    left_join(
      tibble(grts_address = addresses, x = coords[,"x"], y = coords[,"y"]),
      join_by(grts_address)
    ) %>%
    {if (isFALSE(spatial)) . else {
      st_as_sf(., coords = c("x", "y"), crs = crs(spatrast))
    }}
}


