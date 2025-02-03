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





#' Add column 'is_strictly_aquatic' based on stratum column in a data frame
#'
#' @param df Data frame that has a column `stratum` and to which a column
#'   `is_strictly_aquatic` has to be added.
#' @param strata Data frame with columns `type` and `stratum` (`stratum` is
#'   primary key) to declare the relation between type and stratum.
#' @param type_properties Data frame with (at least) columns `type` and
#'   `hydr_class`, where `type` is a primary key.
add_col_is_strictly_aquatic <- function(df, strata, type_properties) {
  df %>%
    inner_join(
      strata,
      join_by(stratum),
      relationship = "many-to-one",
      unmatched = c("error", "drop")
    ) %>%
    inner_join(
      type_properties %>%
        mutate(is_strictly_aquatic = hydr_class == "HC3") %>%
        select(type, is_strictly_aquatic),
      join_by(type),
      relationship = "many-to-one",
      unmatched = c("error", "drop")
    ) %>%
    select(-type)
}



#' Add column 'in_aquatic_subset' based on stratum column in a data frame
#'
#' @param df Data frame that has a column `stratum` and to which a column
#'   `in_aquatic_subset` has to be added, which potentially invokes duplicating
#'   rows where a type has `hydr_class == "HC23"`.
#' @inheritParams add_col_is_strictly_aquatic
add_col_in_aquatic_subset <- function(df, strata, type_properties) {
  df %>%
    inner_join(
      strata,
      join_by(stratum),
      relationship = "many-to-one",
      unmatched = c("error", "drop")
    ) %>%
    inner_join(
      type_properties %>%
        select(type, hydr_class),
      join_by(type),
      relationship = "many-to-one",
      unmatched = c("error", "drop")
    ) %>%
    select(-type) %>%
    mutate(in_aquatic_subset = case_when(
      hydr_class == "HC3" ~ list(TRUE),
      hydr_class == "HC23" ~ list(c(TRUE, FALSE)),
      .default = list(FALSE)
    )) %>%
    select(-hydr_class) %>%
    unnest(in_aquatic_subset)
}



#' Choose optimal threshold to distribute sample sizes in a GRTS address series
#'
#' Given the sampling frame as a series of GRTS addresses and sizes of several
#' samples + spare units that need to be distributed in the series, choose the
#' thresholds in the GRTS address series where each consecutive sample should
#' begin.
#'
#' Note that the 'break points' in the function's code, when samples need to be
#' overlapped, refer to the indices of the second up to the last but one sample.
#'
#' @param sample_sizes Integer vector of sample sizes. The order must reflect
#'   the desired order of samples in the GRTS series.
#' @param spare_sample_sizes Integer vector of spare sample sizes, that
#'   accompany the `sample_sizes`.
#' @param grts_addresses Integer vector of GRTS addresses of the sampling frame.
pick_grts_thresholds <- function(sample_sizes,
                                 spare_sample_sizes,
                                 grts_addresses) {
  if (max(sample_sizes) > length(grts_addresses)) {
    stop("A sample size has been provided that is larger than the sampling frame size.")
  }
  if (sum(sample_sizes, spare_sample_sizes) <= length(grts_addresses)) {
    indices <- dplyr::lag(sample_sizes + spare_sample_sizes, default = 0) + 1
  } else if (sum(sample_sizes) <= length(grts_addresses)) {
    spare_total <- length(grts_addresses) - sum(sample_sizes)
    warning(
      "Reducing (total) spare sample size from ",
      sum(spare_sample_sizes),
      " to ",
      spare_total
    )
    new_spare_sample_sizes <- round(sample_sizes / sum(sample_sizes) * spare_total)
    indices <- dplyr::lag(sample_sizes + new_spare_sample_sizes, default = 0) + 1
  } else {
    warning("Will need to overlap samples between revisit designs; ignoring spare samples.")
    if (length(sample_sizes) == 2) {
      indices <- c(1, length(grts_addresses) - sample_sizes[2] + 1)
    } else if (length(sample_sizes) == 3) {
      # one-dimensional optimization (1 break point), using optimize()
      break_points <- optimize(
        calculate_weighted_overlap_uniformity,
        interval = c(1, sample_sizes[1] + 1),
        sample_sizes = sample_sizes,
        total_available = length(grts_addresses)
      )$minimum
      indices <- c(
        1,
        round(break_points),
        length(grts_addresses) - tail(sample_sizes, 1) + 1
      )
    } else {
      # multi-dimensional optimization, using optim()
      nbp <- length(sample_sizes) - 2
        # lbound & ubounds were originally intended for the lower & upper args
        # of the L-BFGS-B method, which I didn't get working however
      lbounds <- pmax(1, 989 - tail(rev(cumsum(rev(sample_sizes))[-1]), nbp) + 1)
      ubounds <- head(cumsum(sample_sizes) + 1, nbp)
      break_points <- optim(
        colMeans(rbind(lbounds, ubounds)),
        calculate_weighted_overlap_uniformity,
        sample_sizes = sample_sizes,
        total_available = length(grts_addresses)
      )$par
      indices <- c(
        1,
        round(break_points),
        length(grts_addresses) - tail(sample_sizes, 1) + 1
      )
    }
  }
  sort(grts_addresses)[indices]
}



#' Calculate uniformity statistic of sample-size weighted overlap between
#' multiple overlapping samples in a GRTS series
#'
#' A variant of the calculated chi-square statistic (meant to be minimized
#' through optimization) is penalized for the occurrence of gaps (i.e. spare
#' sampling units) in case this function is applied to sample sizes that
#' together are larger than the sampling frame size, so that gaps should be
#' avoided when distributing the samples.
#'
#' @param break_points Positions in GRTS series (as rank: 1, 2, 3, ...) where
#'   the next sample begins. Must be an integer vector of `length(sample_sizes)
#'   - 2`: the last break point must not be given since it is determined as
#'   `total_available - tail(sample_sizes, 1) + 1`.
#' @param sample_sizes Integer vector with sizes of the samples.
#' @param total_available Integer of length 1. Total length of available GRTS
#'   addresses.
#' @param penalize Logical. Should the chi-square statistic be penalized for the
#'   presence (and size) of gaps?
calculate_weighted_overlap_uniformity <- function(break_points,
                                                  sample_sizes,
                                                  total_available,
                                                  penalize = TRUE) {
  stopifnot(length(break_points) == length(sample_sizes) - 2)
  stopifnot(sum(sample_sizes) > total_available)
  stopifnot(all(break_points <= head(cumsum(sample_sizes) + 1, length(sample_sizes) - 2)))
  start <- c(1, break_points, total_available - tail(sample_sizes, 1) + 1)
  end <- c(sample_sizes + start - 1)
  # calculate gaps, which serve as penalty on top of the statistic
  gaps <- pmax(0, start - dplyr::lag(end, default = 0) - 1)
  gap_penalty <- sum(gaps) * 10
  # calculate overlaps; sum them and express them per sample unit for the uniformity test
  overlap_right <- max(0, end - dplyr::lead(start - 1, default = total_available))
  overlap_left <- max(0, dplyr::lag(end + 1, default = 1) - start)
  chisq_stat <- chisq.test(round(
    (overlap_left + overlap_right) / sample_sizes * 100
  ))$statistic
  if (penalize) {
    chisq_stat + gap_penalty
  } else {
    chisq_stat
  }
}




#' Select spare units from a sampling frame with sampling units marked
#'
#' @param ssf_sample Data frame that reflects a spatial sampling frame with
#'   columns.
#'
#'   - The first column is a grouping variable that will be used to apply the
#'   operation for each group.
#'   - The second column must be `grts_address` (integer) and should represent
#'   the reverse hierarchical GRTS address.
#'   - The third and last column must be `sample_status` (character), where
#'   population units that are in the sample must be labelled as `"in_sample"`;
#'   all other population units must be `NA`.
#' @param coef_spare Numeric.
#' The coefficient to apply to (each) sample size in order to determine the
#' number of corresponding spare units.
#'
#' @return A data frame of the same form as `ssf_sample`, only containing
#' the spare units.
generate_spare_units <- function(ssf_sample, coef_spare) {
  spare_unit_count <- ssf_sample %>%
    summarize(
      sample_size = sum(sample_status == "in_sample", na.rm = TRUE),
      .by = 1
    ) %>%
    mutate(
      n_spare_units = round(sample_size * coef_spare) %>%
        as.integer() %>%
        pmax(3L)
    ) %>%
    select(-sample_size)
  ssf_available <-
    ssf_sample %>%
    filter(is.na(sample_status))
  if (nrow(ssf_available) == 0) {
    return(ssf_available)
  } else {
    ssf_available %>%
      inner_join(
        spare_unit_count,
        join_by(!!(colnames(ssf_sample)[1])),
        relationship = "many-to-one",
        unmatched = c("error", "drop")
      ) %>%
      nest(grts_status = c(grts_address, sample_status)) %>%
      mutate(
        grts_status = map2(
          grts_status,
          n_spare_units,
          function(grts, nspare) {
            grts %>%
              slice_min(grts_address, n = nspare) %>%
              mutate(sample_status = "spare_unit")
          }
        )
      ) %>%
      select(-n_spare_units) %>%
      unnest(grts_status)
  }
}



#' Convert a Period vector to simplified character vector
#'
#' @param x A vector of class 'Period'.
simplify_period <- function(x) {
  as.character(x) %>%
    str_split(" ") %>%
    map_chr(\(vec) vec[!str_detect(vec, "^0")])
}




distribute_sample_over_panels <- function(sps, pan) {
  remainder <- nrow(sps) %% nrow(pan)
  pan_row_numbers <- seq_len(nrow(pan))
  if (remainder > 0) {
    indexes_remainder <- lpm1(remainder, matrix(rep(1, nrow(pan)), ncol = 1))
    panels_remainder <- pan_row_numbers[indexes_remainder]
  } else {
    panels_remainder <- pan_row_numbers[0]
  }
  min_group_size <- nrow(sps) %/% nrow(pan)
  if (min_group_size > 0) {
    panels_complete <- rep(pan_row_numbers, each = min_group_size)
  } else {
    panels_complete <- pan_row_numbers[0]
  }
  sps %>%
    arrange(grts_address) %>%
    mutate(genericpanels_row = sort(c(panels_complete, panels_remainder)))
}



#' Subsample an ADHOC FAG in a FAG calendar
#'
#' Subsample an ADHOC FAG in a FAG calendar, taking into account a custom
#' proportion.
#'
#' The custom proportion for subsampling is applied to  spatial-temporal
#' calendar of a single ADHOC FAG, after limiting the number of within-year
#' repetitions of the ADHOC FAG per location to local_max_per_year.
#'
#' @param fag_cal FAG calendar object.
#' @param adhoc_fag Name of the ADHOC field activity group (FAG).
#' @param local_max_per_year In case of repeated ADHOC FAG at a location, the
#'   number of occasions to be sampled _before_ subsampling with `proportion`.
#' @param proportion Proportion used in subsampling
subsample_adhocfag <- function(fag_cal,
                               adhoc_fag,
                               local_max_per_year = 1,
                               proportion) {
  fag_cal %>%
    filter(field_activity_group == adhoc_fag) %>%
    mutate(year_start = year(date_start)) %>%
    # limit to local_max_per_year
    slice_sample(
      n = local_max_per_year,
      by = c(stratum, grts_address, year_start)
    ) %>%
    select(-year_start) %>%
    # subsample
    slice_sample(prop = samplingprop_adhocpipereplace)
}



