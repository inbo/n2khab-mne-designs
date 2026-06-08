verify_n2khab_data <- function(reference, requirements) {
  checksums <-
    reference %>%
    filter(version_id %in% requirements) %>%
    mutate(
      filepath_expected = file.path(
        locate_n2khab_data(),
        ifelse(processed, "20_processed", "10_raw"),
        source_id,
        filename
      ),
      file_exists = file.exists(filepath_expected),
      xxh64sum_user = ifelse(
        file_exists,
        xxh64sum(filepath_expected),
        NA_character_
      )
    )
  checksum_missingfile <- checksums %>% filter(!file_exists)
  if (nrow(checksum_missingfile) > 0) {
    assign(
      "checksum_missingfile",
      checksum_missingfile,
      envir = parent.env(environment())
    )
    stop(
      "Missing or incomplete data sources:\n",
      str_flatten(unique(checksum_missingfile$source_id), collapse = "\n"),
      "\nPlease inspect the required versions in `checksum_missingfile`. ",
      "Use n2khab::download_zenodo(\"<doi>\", \"<path>\") as needed. ",
      "See https://inbo.github.io/n2khab/articles/v020_datastorage.html ."
    )
  }
  checksum_diff <- checksums %>% filter(file_exists, xxh64sum != xxh64sum_user)
  if (nrow(checksum_diff) > 0) {
    assign(
      "checksum_diff",
      checksum_diff,
      envir = parent.env(environment())
    )
    stop(
      "Wrong data source versions detected for:\n",
      str_flatten(unique(checksum_diff$source_id), collapse = "\n"),
      "\nPlease inspect the required versions in `checksum_diff`. ",
      "Use n2khab::download_zenodo(\"<doi>\", \"<path>\") as needed. ",
      "See https://inbo.github.io/n2khab/articles/v020_datastorage.html ."
    )
  }
  message("All n2khab_data requirements are fulfilled!")
}



apply_activity_sequence_filters <- function(df) {
  df %>%
    filter(
      # for GW_03.3 x GWLEVREADDIVER as core scheme, use the piezwell sequence:
      !(
        scheme == "GW_03.3" & is_core_scheme &
          main_field_activity == "GWLEVREADDIVER" &
          ((in_aquatic_subset & activity_sequence != "gwsurf_lev_piezwell") |
             (!in_aquatic_subset & activity_sequence != "gw_lev_piezwell"))
      ),
      # for GW_03.3 x GWSHALLSAMP as core scheme, use the readman sequence:
      !(
        scheme == "GW_03.3" & is_core_scheme &
          main_field_activity == "GWSHALLSAMP" &
          ((in_aquatic_subset & activity_sequence != "gw_samp_gwsurf_readman") |
             (!in_aquatic_subset & activity_sequence != "gw_samp_gw_readman"))
      ),
      # for other GWSHALLSAMP, use the gw_samp sequence:
      !(
        (scheme != "GW_03.3" | !is_core_scheme) &
          main_field_activity == "GWSHALLSAMP" &
          activity_sequence != "gw_samp"
      ),
      # for GW_05.1_aq x (GWLEVREADDIVER or SURFLEVREADGAUGE) as non-core
      # scheme, only include sequence gwsurf_lev_piezwell
      !(
        scheme == "GW_05.1_aq" & !is_core_scheme &
          main_field_activity %in% c("GWLEVREADDIVER", "SURFLEVREADGAUGE") &
          activity_sequence != "gwsurf_lev_piezwell"
      ),
      # for GW_05.1_terr, GW_05.2 x GWLEVREADDIVER as non-core scheme, only
      # include sequence gw_lev_piezwell
      !(
        scheme %in% c("GW_05.1_terr", "GW_05.2") & !is_core_scheme &
          main_field_activity == "GWLEVREADDIVER" &
          activity_sequence != "gw_lev_piezwell"
      ),
      # for GW_05.1_aq x GWLEVREADDIVER as core scheme, only include sequence
      # gwsurf_lev_well
      !(
        scheme == "GW_05.1_aq" & is_core_scheme &
          main_field_activity == "GWLEVREADDIVER" &
          activity_sequence != "gwsurf_lev_well"
      ),
      # for GW_05.1_terr, GW_05.2 x GWLEVREADDIVER as core scheme, only include
      # sequence gw_lev_well
      !(
        scheme %in% c("GW_05.1_terr", "GW_05.2") & is_core_scheme &
          main_field_activity == "GWLEVREADDIVER" &
          activity_sequence != "gw_lev_well"
      ),
      # for SURF_03.4_lentic x SURFLEVREADGNSS, use the surf_lent_samp
      # sequence:
      !(
        scheme == "SURF_03.4_lentic"  &
          main_field_activity == "SURFLEVREADGNSS" &
          activity_sequence != "surf_lent_samp"
      ),
      # for SURF_03.4_lotic x SURFLEVREADGNSS, use the surf_lent_samp sequence:
      !(
        scheme == "SURF_03.4_lotic"  &
          main_field_activity == "SURFLEVREADGNSS" &
          activity_sequence != "surf_lot_samp"
      )
    )
}


#' Collapse (unexpand) a data frame with a stratum or type column
#'
#' Collapses a data frame that has been the result of a n2khab::expand_types()
#' operation.
#'
#' The collapsing (unexpanding) comprises two aspects:
#'
#' - replacing subtype codes by their main type code (where the target
#' population is defined by the latter)
#' - adding units, associated with a main type, to each corresponding subtype
#' layer that triggered the expansion to the main type.
#'
#' In effect, 'collapsing' leads to less stratum or type levels, but _more_
#' rows.
#'
#' @param df A data frame with a column specified by `stratumvar`.
#' @param types Are we collapsing _type_ levels? If `FALSE` (the default), it is
#'   assumed we are dealing with _stratum_ levels instead.
#' @param stratumvar String. Name of the column in `df` that is to be collapsed.
#'   Default is `"stratum"` unless types is TRUE, in which case the default is
#'   `"type"`.
collapse_strata <- function(
  df,
  types = FALSE,
  stratumvar = ifelse(isTRUE(types), "type", "stratum")
) {
  maintype_collapse <-
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
    ) %>%
    bind_rows(
      if (types) {
        tribble(
          ~main_type, ~subtype,
          "3130", "3130_aom",
          "3130", "3130_na"
        )
      } else {
        tribble(
          ~main_type, ~subtype,
          "3130_0_1", "3130_aom_0_1",
          "3130_0_1", "3130_na_0_1",
          "3130_1_5", "3130_aom_1_5",
          "3130_1_5", "3130_na_1_5",
          "3130_5_50", "3130_aom_5_50",
          "3130_5_50", "3130_na_5_50",
          "3130_50_150", "3130_aom_50_150",
          "3130_50_150", "3130_na_50_150"
        )
      }
    )
  df %>%
    mutate(
      "{stratumvar}" := recode_values(
        .data[[stratumvar]],
        "5130_hei" ~ "5130",
        "5130_kalk" ~ "5130",
        "rbbkam+" ~ "rbbkam",
        "rbbzil+" ~ "rbbzil",
        "9120_qb" ~ "9120",
        default = .data[[stratumvar]]
      )
    ) %>%
    left_join(
      maintype_collapse,
      join_by({{stratumvar}} == main_type),
      relationship = "many-to-many",
      unmatched = "drop"
    ) %>%
    mutate(
      "{stratumvar}" := ifelse(is.na(subtype), .data[[stratumvar]], subtype) %>%
        as.character() %>%
        factor(levels = levels(n2khab_strata_expanded %>% pull({{stratumvar}})))
    ) %>%
    select(-subtype)
}


aggregate_sample_size <- function(df,
                                  sample_size_all_panels_var,
                                  mhq_scheme_category,
                                  by_module_combo = FALSE) {
  if (by_module_combo) {
    modvar <- "module_combo_code"
  } else {
    modvar <- "module"
  }
  df %>%
    mutate(yearly_sample_size = .data[[sample_size_all_panels_var]] / cycle_duration_y) %>%
    summarize(
      yearly_sample_size = sum(yearly_sample_size, na.rm = TRUE),
      .by = c(all_of(modvar), scheme)
    ) %>%
    filter(!is.na(yearly_sample_size), yearly_sample_size > 0) %>%
    arrange(.data[[modvar]], scheme) %>%
    left_join(
      mhq_scheme_category,
      join_by(scheme),
      relationship = "many-to-one",
      unmatched = "drop"
    ) %>%
    mutate(
      is_mhq = str_detect(scheme, "^HQ"),
      scheme_aggr = ifelse(is_mhq, str_c("MHQ_", category), as.character(scheme))
    ) %>%
    summarize(
      yearly_sample_size = sum(yearly_sample_size) %>% round() %>% as.integer(),
      .by = c(all_of(modvar), scheme_aggr)
    )
}


#' Simplify a scheme column by aggregating MHQ schemes
#'
#' This transforms the scheme column of a data frame in place, by collapsing its
#' levels according to MHQ scheme.
simplify_mhq_schemes <- function(df) {
  df %>%
    left_join(
      mhq_scheme_category,
      join_by(scheme),
      relationship = "many-to-one",
      unmatched = "drop"
    ) %>%
    mutate(
      scheme = ifelse(
        str_detect(scheme, "^HQ"),
        str_c("MHQ_", category),
        as.character(scheme)
      ) %>%
        factor(levels = c(
          levels(schemes$scheme),
          "MHQ_terrestrial_open",
          "MHQ_terrestrial_forest",
          "MHQ_lentic",
          "MHQ_lotic"
        ))
    ) %>%
    select(-category)
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



#' Add grts_address_final and other attributes to a stratum x grts_address
#' object
#'
#' grts_address always refers to the GRTS address used in ranking and sampling,
#' but some locations may not have the targeted stratum and are linked to a
#' replacement site. This function adds the replacement site
#' (grts_address_final) and some other attributes from
#' stratum_grts_n2khab_phabcorrected_no_replacements.
#'
#' @param df Data frame holding a stratum and grts_address column.
add_assessment_data <- function(df) {
  df %>%
  inner_join(
    stratum_grts_n2khab_phabcorrected_no_replacements,
    join_by(stratum, grts_address),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  ) %>%
  mutate(
    grts_address_final = ifelse(is.na(replaced_by), grts_address, replaced_by)
  ) %>%
  relocate(grts_address_final, .after = grts_address) %>%
  select(-replaced_by)
}


#' Drop attributes previously added by add_assessment_data()
#'
#' Dropping attributes previously added by `add_assessment_data()` can be
#' useful if the assessment attributes were needed only temporarily.
#'
#' @param df Data frame.
drop_assessment_data <- function(df) {
  assessmcol <- c(
    colnames(stratum_grts_n2khab_phabcorrected_no_replacements),
    "grts_address_final"
  )
  assessmcol <- assessmcol[!(assessmcol %in% c("stratum", "grts_address"))]
  df %>%
    select(!any_of(assessmcol))
}



#' Add point coordinate columns to a data frame with a GRTS address column
#'
#' @param df Data frame.
#' @param grts_var String. The column name in df that holds the GRTS addresses.
#' @param spatrast SpatRaster object with level 0 GRTS addresses.
#' @param spatrast_index Data frame with columns 'id' and 'grts_address',
#'   holding the cell numbers (cell IDs) for each GRTS address in `spatrast`.
#' @param spatial Logical. Should the returned object be a sf points object? If
#'   `FALSE`, a data frame is returned with x and y coordinates as columns.
#'
#' @returns An sf points object or a tibble with coordinates, depending on the
#'   `spatial` argument.
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
      join_by({{ grts_var }} == grts_address)
    ) %>%
    {
      if (isFALSE(spatial)) {
        .
      } else {
        st_as_sf(., coords = c("x", "y"), crs = crs(spatrast))
      }
    }
}

#' Generate raster cells based on a vector of GRTS addresses
#'
#' Subsets the SpatRaster provided in the `spatrast` argument, using a vector of
#' either GRTS addresses or cell numbers.
#'
#' @param addresses Vector of integer GRTS addresses (level 0).
#' @inheritParams add_point_coords_grts
#' @param cells Vector of cell numbers to use; overrides addresses.
#' @param drop_address Logical. Should the non-missing values of the returned
#'   SpatRaster contain the original values, or should they be set as 1?
#' @param output_cell_nrs Logical. Should the function just return the cell
#'   numbers as an integer vector?
#'
#' @returns SpatRaster, or an integer vector if `output_cell_nrs` is `TRUE`.
filter_grtsraster_by_address <- function(
    addresses = NULL,
    spatrast = grts_mh_n2khab,
    spatrast_index = grts_mh_n2khab_index,
    cells = NULL,
    drop_address = FALSE,
    output_cell_nrs = FALSE) {
  if (is.null(cells)) {
    cells <- subset(spatrast_index, grts_address %in% addresses)$id
  }
  if (output_cell_nrs) {
    return(cells)
  }
  r <- spatrast[cells, drop = FALSE]
  if (drop_address) {
    r[!is.na(r)] <- 1
  }
  r
}



#' Generate the potential 'level 3' replacement GRTS cell numbers for a given
#' vector of GRTS addresses
#'
#' Given a vector of GRTS addresses, provides the cell numbers that fall inside
#' the enclosing larger 256 * 256 GRTS cell ('level 3 GRTS cell'). Note that
#' this result must still be limited to the cells of a specific polygon if this
#' is used for the polygon-constrained local replacement method.
#'
#' @inheritParams filter_grtsraster_by_address
#' @param spatrast_lev3 SpatRaster object with level 3 GRTS addresses, at the
#'   resolution of `spatrast`.
#' @param spatrast_lev3_index Data frame with columns 'id' and 'grts_address',
#'   holding the cell numbers for each GRTS address in `spatrast_lev3`.
#' @param as_list Logical. Should the result be given as a list, ordered so that
#'   the first element contains the replacement cell numbers corresponding to
#'   the first element of `addresses`, and so on? In this case, each element is
#'   a tibble of both the cell numbers and the GRTS address (level 0). Note that
#'   different GRTS addresses at level 0 may still yield the same set of
#'   replacement cell numbers if they reside in the same level 3 cell. If
#'   `FALSE`, a single vector is returned of unique cell numbers.
#'
#' @returns Vector or list, depending on the value of `as_list`.
get_level3replacement_cellnrs <- function(
    addresses,
    spatrast = grts_mh_n2khab,
    spatrast_index = grts_mh_n2khab_index,
    spatrast_lev3,
    spatrast_lev3_index,
    as_list = TRUE
) {
    id0 <- subset(spatrast_index, grts_address %in% unique(addresses))$id
    addr3 <- spatrast_lev3[id0]$level3
    id3 <- spatrast_lev3_index %>%
      filter(grts_address %in% unique(addr3)) %>%
      pull(id)
  if (!as_list) {
    id3
  } else {
    replacement_cells_grts03 <- tibble(
      cellnr_replac = id3,
      grts_address_replac = spatrast[id3][, 1],
      grts_address_replac_lev3 = spatrast_lev3[id3][, 1]
    )
    given_cells_grts03 <- replacement_cells_grts03 %>%
      select(-cellnr_replac) %>%
      filter(grts_address_replac %in% addresses) %>%
      rename(grts_address = grts_address_replac)
    sampledcells_replacementcells <-
      given_cells_grts03 %>%
      inner_join(
        replacement_cells_grts03,
        join_by(grts_address_replac_lev3),
        relationship = "many-to-many",
        unmatched = "error"
      ) %>%
      select(-grts_address_replac_lev3) %>%
      nest(replacement_cells = c(cellnr_replac, grts_address_replac))
    # following statement takes care to align the row order with the GRTS
    # addresses vector
    sampledcells_replacementcells[match(
      addresses,
      sampledcells_replacementcells$grts_address
    ), ]$replacement_cells
  }
}



#' Convert a vector of GRTS addresses to the corresponding level 3 addresses
#'
#' @inheritParams filter_grtsraster_by_address
#' @inheritParams get_level3replacement_cellnrs
convert_level0_to_level3 <- function(
    addresses,
    spatrast = grts_mh_n2khab,
    spatrast_index = grts_mh_n2khab_index,
    spatrast_lev3
) {
  result <- tibble(
    addr = addresses,
    id0 = spatrast_index[match(addresses, spatrast_index$grts_address), ]$id
  )
  id0_nona <- result$id0[!is.na(result$id0)] %>% unique()
  result %>%
    left_join(
      tibble(
        id0 = id0_nona,
        lev3addr = spatrast_lev3[id0_nona]$level3
      ),
      join_by(id0),
      relationship = "many-to-one",
      unmatched = "error"
    ) %>%
    pull(lev3addr)
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




#' Add column 'in_aquatic_subset' based on type column in a data frame
#'
#' @param df Data frame that has a column `type` and to which a column
#'   `in_aquatic_subset` has to be added, which potentially invokes duplicating
#'   rows where a type has `hydr_class == "HC23"`.
add_typecol_in_aquatic_subset <- function(df) {
  df %>%
    inner_join(
      read_types() %>%
        # duplicating types that have aquatic forms & terrestrial forms:
        filter(hydr_class == "HC23") %>%
        uncount(2) %>%
        mutate(in_aquatic_subset = rep(c(TRUE, FALSE), 2)) %>%
        bind_rows(
          read_types() %>%
            filter(hydr_class != "HC23") %>%
            mutate(in_aquatic_subset = hydr_class == "HC3")
        ) %>%
        select(type, in_aquatic_subset),
      join_by(type),
      unmatched = c("error", "drop"),
      relationship = "many-to-many"
    )
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




#' Read and tidy csv file with MHQ samples
#'
#' @param path File path.
#' @param grts_var Column name to be used as GRTS address.
#' @param single_type Optional string to set a single type that represents all
#'   rows.
read_csv_mhq_samples <- function(path,
                                 grts_var = "grts_ranking_draw",
                                 single_type = NULL) {
  (
    if (is.null(single_type)) {
      read_delim(
        file = path,
        delim = ";",
        col_types = cols_only(
          {{ grts_var }} := col_integer(),
          habitattype = col_character()
        )
      ) %>%
        select(
          stratum = habitattype,
          grts_address = {{ grts_var }}
        )
    } else {
      read_delim(
        file = path,
        delim = ";",
        col_types = cols_only(
          {{ grts_var }} := col_integer()
        )
      ) %>%
        mutate(stratum = single_type) %>%
        select(
          stratum,
          grts_address = {{ grts_var }}
        )
    }
  ) %>%
    mutate(
      # shortcut a complication for forests, where > 1 type is sometimes noted
      stratum = str_extract(stratum, "^\\w+(\\+$)?"),
      stratum = parse_factor(stratum, levels = levels(n2khab_strata$stratum))
    ) %>%
    arrange(stratum, grts_address)
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
#' @details This procedure also copes with the case of a GRTS series in a
#'   particular spatial poststratum that has multiple subseries ('subsamples')
#'   marked as 'in_sample', with addresses outside of the sample in between
#'   these. This can be the consequence of combining different GRTS partitions
#'   with the same number in the same poststratum, originating from spatial
#'   sampling in different original domains (including vs included domain).
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
#' @param coef_spare Numeric. The coefficient to apply to (each) sample size in
#'   order to determine the number of corresponding spare units.
#'
#' @return A data frame of the same form as `ssf_sample`, only containing the
#'   spare units.
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
    arrange(sp_poststratum, grts_address) %>%
    # marking consecutive subsamples per spatial poststratum
    mutate(
      subsample_start = !is.na(sample_status) &
        sample_status == "in_sample" &
        is.na(lag(sample_status)),
      subsample = cumsum(subsample_start),
      .by = sp_poststratum
    ) %>%
    # adding the subsample size (not counting spare units)
    mutate(
      subsample_size = sum(!is.na(sample_status)),
      .by = c(sp_poststratum, subsample)
    ) %>%
    # keeping only the largest subsample per spatial poststratum
    filter(
      subsample_size == max(subsample_size),
      .by = sp_poststratum
    ) %>%
    # keeping only the last subsample if multiple subsamples still emerged
    filter(
      subsample == max(subsample),
      .by = sp_poststratum
    ) %>%
    select(-starts_with("subsample")) %>%
    # keeping only the units eligible as spare unit
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



#' Distribute a spatially balanced sample over panels in a spatially and
#' temporally balanced way
#'
#' @param sps Tibble with a column `grts_address`, representing a spatial
#'   sample.
#' @param pan Tibble of membership-aligned sets of generic panels, each column
#'   referring to a different number of generic panels and their repetition
#'   pattern over time. The first column of the tibble must be `id` (incremental
#'   integer), representing sequential date-intervals.
#' @param virtpan Tibble consisting of a column `panel_address` and `panel_id`,
#'   typically derived from `create_1d_grts_sample()`.
distribute_sample_over_panels <- function(sps, pan, virtpan) {
  remainder <- nrow(sps) %% nrow(pan)
  pan_row_numbers <- seq_len(nrow(pan))
  if (remainder > 0) {
    panels_remainder <-
      virtpan %>%
      arrange(panel_address) %>%
      slice_head(n = remainder) %>%
      arrange(panel_id) %>%
      mutate(
        panel_id_rescaled = ceiling(panel_id / (nrow(virtpan) / nrow(pan))) %>%
          as.integer()
      ) %>%
      pull(panel_id_rescaled)
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




#' Create a one-dimensional GRTS sample of a given size
#'
#' Creates a one-dimensional GRTS sample, where population units are numbered
#' consecutively and distance between units is defined by the difference in
#' number.
#'
#' @details The size determines the identifiers of the population units: they
#'   are numbered from 1 to `size`.
#'
#' @param size Requested population size
#' @inheritParams grtsdb::add_level
#'
#' @returns Tibble with columns `address` (the GRTS address) and `id` (the
#'   population unit ID).
create_1d_grts_sample <- function(size, verbose = FALSE) {
  n2khab:::require_pkgs("grtsdb")
  db_1d <- grtsdb::connect_db(":memory:")
  bbox_1d <- matrix(c(1, size), ncol = 2)
  cellsize_1d <- 1
  grtsdb::add_level(
    bbox = bbox_1d,
    cellsize = cellsize_1d,
    grtsdb = db_1d,
    verbose = verbose
  )
  result <-
    grtsdb::extract_sample(
    samplesize = size,
    bbox = bbox_1d,
    cellsize = cellsize_1d,
    grtsdb = db_1d,
    verbose = verbose
  ) %>%
    as_tibble() %>%
    select(address = ranking, id = x1c) %>%
    mutate(id = as.integer(id)) %>%
    arrange(address)
  DBI::dbDisconnect(db_1d)
  return(result)
}






#' Subsample and relax an ADHOC FAG in a FAG calendar
#'
#' Subsample an ADHOC FAG in a FAG calendar, taking into account a custom
#' proportion.
#'
#' The custom proportion for subsampling is applied to the spatial-temporal
#' calendar of a single ADHOC FAG, after limiting the number of within-year
#' repetitions of the ADHOC FAG per location to local_max_per_year.
#'
#' Furthermore, the date intervals of the remaining ADHOC FAGs are relaxed to
#' take (at random) one of the existing date intervals of non-ADHOC & non-biotic
#' FAGs that have the same start date (in the same stratum x location). This is
#' done to not limit ADHOC FAGs to their initial date interval of one month, but
#' align them with existing date intervals.
#'
#' @param fag_cal FAG calendar object.
#' @param adhoc_fag Name of the ADHOC field activity group (FAG).
#' @param local_max_per_year In case of repeated ADHOC FAG at a location, the
#'   number of occasions to be sampled _before_ subsampling with `proportion`.
#' @param proportion Proportion used in subsampling
subsample_and_relax_adhocfag <- function(fag_cal,
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
    slice_sample(prop = samplingprop_adhocpipereplace) %>%
    # relax the ADHOC date intervals
    inner_join(
      fag_cal %>%
        filter(!str_detect(field_activity_group, "ADHOC|LOCEVAL|LSVI")) %>%
        select(-field_activity_group, -rank, -scheme_moco_ps) %>%
        rename(
          date_end_new = date_end,
          date_interval_new = date_interval
        ),
      join_by(stratum, grts_address, date_start),
      relationship = "one-to-many",
      unmatched = c("error", "drop")
    ) %>%
    slice_sample(
      n = 1,
      by = !c(date_end_new, date_interval_new)
    ) %>%
    mutate(
      date_end = date_end_new,
      date_interval = date_interval_new
    ) %>%
    select(-contains("new")) %>%
    arrange(stratum, grts_address, date_start, rank, date_end, field_activity_group)
}



#' Set up an environment with lazy-loaded R-objects from RData file
#'
#' Based upon https://stackoverflow.com/a/8703024. Lazy-loading objects from a
#' lazy-load database prevents loading all objects into memory, while being able
#' to access them on-demand. Loading the DB only loads the index but not the
#' contents. The function sets this up as in a specific environment, so that one
#' can use `get("object_name", envir = an_environment)` and `ls(envir =
#' an_environment)`.
#'
#' @param rdata_filepath File path to the RData file.
#' @param new_envir_name String to be used as the name of the new environment.
#' @param database_name String to be used as the internally known name of the
#'   lazy-load database.
prepare_lazy_get <- function(rdata_filepath,
                             new_envir_name,
                             database_name = new_envir_name) {
  # populate a temporary environment with the objects
  temp_env <- env_panflpan5_previous <- local({
    load(rdata_filepath)
    environment()
  })
  # make lazy-load database from temp_env and give it a local name
  tools:::makeLazyLoadDB(temp_env, file.path(tempdir(), database_name))
  # remove the temp environment
  rm(list = ls(envir = temp_env), envir = temp_env)
  gc()
  rm(temp_env)
  gc()
  # create the requested environment to access the lazy-load database
  assign(new_envir_name, new.env(), envir = parent.env(environment()))
  # lazy-load the R object database in the new environment
  lazyLoad(file.path(tempdir(), database_name), eval(str2lang(new_envir_name)))
  invisible(NULL)
}


