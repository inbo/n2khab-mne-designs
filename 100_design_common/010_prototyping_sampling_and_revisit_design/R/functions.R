apply_activity_sequence_filters <- function(df) {
  df %>%
    filter(
      # for GW_03.3 x GWLEVREADDIVER as core scheme, use the piezwell sequence:
      !(
        scheme == "GW_03.3" & is_core_scheme &
          main_field_method == "GWLEVREADDIVER" &
          ((in_aquatic_subset & activity_sequence != "gwsurf_lev_piezwell") |
             (!in_aquatic_subset & activity_sequence != "gw_lev_piezwell"))
      ),
      # for GW_03.3 x GWSHALLSAMP as core scheme, use the readman sequence:
      !(
        scheme == "GW_03.3" & is_core_scheme &
          main_field_method == "GWSHALLSAMP" &
          ((in_aquatic_subset & activity_sequence != "gw_samp_gwsurf_readman") |
             (!in_aquatic_subset & activity_sequence != "gw_samp_gw_readman"))
      ),
      # for other GWSHALLSAMP, use the gw_samp sequence:
      !(
        (scheme != "GW_03.3" | !is_core_scheme) &
          main_field_method == "GWSHALLSAMP" &
          activity_sequence != "gw_samp"
      ),
      # for GW_05.1_aq x (GWLEVREADDIVER or SURFLEVREADGAUGE) as non-core
      # scheme, only include sequence gwsurf_lev_piezwell
      !(
        scheme == "GW_05.1_aq" & !is_core_scheme &
          main_field_method %in% c("GWLEVREADDIVER", "SURFLEVREADGAUGE") &
          activity_sequence != "gwsurf_lev_piezwell"
      ),
      # for GW_05.1_terr, GW_05.2 x GWLEVREADDIVER as non-core scheme, only
      # include sequence gw_lev_piezwell
      !(
        scheme %in% c("GW_05.1_terr", "GW_05.2") & !is_core_scheme &
          main_field_method == "GWLEVREADDIVER" &
          activity_sequence != "gw_lev_piezwell"
      ),
      # for GW_05.1_aq x GWLEVREADDIVER as core scheme, only include sequence
      # gwsurf_lev_well
      !(
        scheme == "GW_05.1_aq" & is_core_scheme &
          main_field_method == "GWLEVREADDIVER" &
          activity_sequence != "gwsurf_lev_well"
      ),
      # for GW_05.1_terr, GW_05.2 x GWLEVREADDIVER as core scheme, only include
      # sequence gw_lev_well
      !(
        scheme %in% c("GW_05.1_terr", "GW_05.2") & is_core_scheme &
          main_field_method == "GWLEVREADDIVER" &
          activity_sequence != "gw_lev_well"
      ),
      # for SURF_03.4_lentic x SURFLEVREADGAUGE, use the surf_lent_samp
      # sequence:
      !(
        scheme == "SURF_03.4_lentic"  &
          main_field_method == "SURFLEVREADGAUGE" &
          activity_sequence != "surf_lent_samp"
      ),
      # for SURF_03.4_lotic x SURFLEVREADGAUGE, use the surf_lent_samp sequence:
      !(
        scheme == "SURF_03.4_lotic"  &
          main_field_method == "SURFLEVREADGAUGE" &
          activity_sequence != "surf_lot_samp"
      )
    )
}


collapse_strata <- function(df) {
  df %>%
    mutate(
      stratum = case_match(
        stratum,
        "5130_hei" ~ "5130",
        "5130_kalk" ~ "5130",
        "rbbkam+" ~ "rbbkam",
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
        as.character() %>%
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


#' Apply finite population correction to a sample size for infinite populations
#'
#' @param n_inf Sample size for infinite populations. Can be a vector.
#' @param pop Population size. Can be a vector.
apply_fpc <- function(n_inf, pop) {
  ifelse(
    n_inf != 0 & !is.na(n_inf),
    n_inf * pop / (n_inf + pop),
    n_inf
  )
}


#' Calculate deviation from a total budgeted finite sample size over strata
#'
#' First calculate infinite sample sizes from a vector of budgeted finite sample
#' sizes (for strata), using a fixed coefficient for the whole vector.
#' Then apply finite population correction for each stratum, making use of its
#' guessed infinite sample size and its population size.
#' Finally, return the difference between the sum of resulting sample sizes and
#' the sum of the initial sample sizes (which together reflect the overall
#' budgeted sample size).
#'
#' The idea is to take control of the precision of estimators, which is tied to
#' the sample size.
#' Typically one wants to get at similar (equal) precision between strata,
#' as represented in the vector of initially set sample sizes for strata,
#' ignoring finite populations, and which together reflect the budgeted number
#' of sampling units.
#'
#' @param x Coefficient (scalar numeric).
#' @param n Vector of initially set sample sizes for strata (see Details).
#' @param pop Vector of stratum sizes.
get_fpc_sample_size_difference <- function(x, n, pop) {
  n <- ifelse(is.na(n), 0, n)
  n_inf <- n * x
  n_fin <- apply_fpc(n_inf, pop)
  abs(sum(n_fin) - sum(n))
}


#' Add point coordinate columns to a data frame with a GRTS address column
add_point_coords_grts <- function(
    df,
    grts_var = "grts_address",
    spatrast = grts_mh_n2khab,
    spatrast_index = grts_mh_n2khab_index,
    spatial = TRUE) {
  addresses <- df %>%
    distinct(.data[[grts_var]]) %>%
    pull(.data[[grts_var]]) %>%
    sort()

  grts_cells <- spatrast_index %>%
    filter(grts_address %in% addresses) %>%
    arrange(grts_address) %>%
    pull(id)

  coords <- xyFromCell(spatrast, grts_cells)

  df %>%
    left_join(
      tibble(grts_address = addresses, x = coords[, "x"], y = coords[, "y"]),
      join_by(grts_address)
    ) %>%
    {
      if (isFALSE(spatial)) {
        .
      } else {
        st_as_sf(., coords = c("x", "y"), crs = crs(spatrast))
      }
    }
}


distribute_sample_over_panels <- function(sps, pan) {
  remainder <- nrow(sps) %% nrow(pan)
  if (remainder > 0) {
    indexes_remainder <- lpm1(remainder, matrix(rep(1, nrow(pan)), ncol = 1))
    panels_remainder <- pan$generic_panel[indexes_remainder]
  } else {
    panels_remainder <- pan$generic_panel[0]
  }
  min_group_size <- nrow(sps) %/% nrow(pan)
  if (min_group_size > 0) {
    panels_complete <- rep(pan$generic_panel, each = min_group_size)
  } else {
    panels_complete <- pan$generic_panel[0]
  }
  sps %>%
    arrange(grts_address) %>%
    mutate(panel = sort(c(panels_complete, panels_remainder)))
}
