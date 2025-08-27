## Total sample sizes per scheme -------------------------------------------

get_total_sample_sizes <- function(df, types = NULL) {
  df1 <- df %>%
    st_drop_geometry() %>%
    distinct(scheme, module_combo_code, stratum, grts_address)
  if (is.null(types)) {
    df1 %>%
      count(scheme, module_combo_code)
  } else {
    df1 %>%
      inner_join(
        n2khab_strata,
        join_by(stratum),
        relationship = "many-to-one",
        unmatched = c("error", "drop")
      ) %>%
      semi_join(types, join_by(type)) %>%
      count(scheme, module_combo_code)
  }
}



## Stratum sample sizes for cell types --------------------------------

compare_ssizes_per_stratum <- function(df, dfref = ssizes_ref) {
  df %>%
  semi_join(cell_types, join_by(stratum == type)) %>%
  # sum sample sizes over panel sets, limited by nunits
  summarize(
    ssize_stratum_altered = pmin(
      sum(sp_sample_size_all_panels_stratum),
      first(nunits)
    ),
    .by = c(module, domain, scheme, stratum, nunits, spss_stratum_truncated)
  ) %>%
  inner_join(
    dfref %>%
      semi_join(cell_types, join_by(stratum == type)) %>%
      # sum sample sizes over panel sets, limited by nunits
      summarize(
        ssize_stratum = pmin(
          sum(sp_sample_size_all_panels_stratum),
          first(nunits)
        ),
        .by = c(module, domain, scheme, stratum)
      ),
    join_by(module, domain, scheme, stratum),
    relationship = "one-to-one",
    unmatched = "error"
  ) %>%
  filter(!is.na(ssize_stratum_altered))
}



plot_abs_sample_sizes <- function(df, flanders = TRUE) {
  compare_ssizes_per_stratum(df) %>%
    mutate(
      compartment = str_match(scheme, "^(\\w+)_")[, 2] %>% factor()
    ) %>%
    select(-scheme) %>%
    filter(!is.na(compartment)) %>%
    distinct() %>%
    pivot_longer(
      starts_with("ssize"),
      names_to = "scenario",
      values_to = "sample_size"
    ) %>%
    mutate(scenario = fct_recode(
      scenario,
      new = "ssize_stratum_altered",
      ref = "ssize_stratum"
    )) %>%
    {
      if (flanders) {
        filter(., domain == "Flanders")
      } else {
        filter(., domain != "Flanders")
      }
    } %>%
    ggplot(aes(x = stratum, y = sample_size, fill = scenario, label = sample_size)) +
    geom_col(position = "identity", alpha = 0.4) +
    geom_text(size = 3.5) +
    facet_grid(compartment + domain ~ ., scales = "free_y") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1))
}

# COMPARE MHQ OVERLAP -----------------------------------------------------

show_mhq_overlap <- function(df) {
  df %>%
    st_drop_geometry() %>%
    semi_join(cell_types, join_by(stratum == type)) %>%
    mutate(compartment = str_match(scheme, "^(\\w+)_")[, 2]) %>%
    filter(!is.na(compartment)) %>%
    distinct(compartment, stratum, grts_address, assessed_in_field) %>%
    summarize(
      count_is_mhq = sum(assessed_in_field),
      proportion_is_mhq = round(sum(assessed_in_field) / n(), 2),
      .by = compartment
    )
}

# INVESTIGATE INTEGRATED SAMPLE PROPORTIONS PER DOMAIN -----------------------

get_integrated_ssizes_per_domain <- function(df,
                                             max_sample_prop,
                                             regex_compartment = "^GW") {
  df %>%
    st_drop_geometry() %>%
    distinct(scheme, sp_poststratum, stratum, grts_address) %>%
    count(scheme, sp_poststratum, stratum) %>%
    filter(str_detect(sp_poststratum, "^BE")) %>%
    rename(domain = sp_poststratum) %>%
    bind_rows(
      df %>%
        st_drop_geometry() %>%
        distinct(scheme, sp_poststratum, stratum, grts_address) %>%
        count(scheme, stratum) %>%
        mutate(domain = "Flanders")
    ) %>%
    semi_join(cell_types, join_by(stratum == type)) %>%
    mutate(
      domain = factor(domain, levels = levels(dom_scheme_stratum_nunits$domain))
    ) %>%
    filter(str_detect(scheme, regex_compartment)) %>%
    inner_join(
      dom_scheme_stratum_nunits,
      join_by(scheme, domain, stratum),
      relationship = "one-to-one",
      unmatched = c("error", "drop")
    ) %>%
    mutate(sample_prop = round(n / nunits, 2)) %>%
    filter(sample_prop > max_sample_prop)
}

get_integrated_ssizes_per_domain(sps_new, 0.2, "^GW")


# WRITING GPKG LAYERS -----------------------------------------------------

write_to_gpkg_layer <- function(df, layername, regex_compartment = "^GW", types = NULL) {
  df1 <- df %>% filter(str_detect(scheme, regex_compartment))
  if (is.null(types)) {
    df1 <-
      df1 %>%
      semi_join(cell_types, join_by(stratum == type))
  } else {
    df1 <-
      df1 %>%
      inner_join(
        n2khab_strata,
        join_by(stratum),
        relationship = "many-to-one",
        unmatched = c("error", "drop")
      ) %>%
      semi_join(types, join_by(type))
  }
  df1 %>%
    distinct(scheme, module_combo_code, stratum, grts_address, grts_address_final, geometry) %>%
    mutate(compartment = str_match(scheme, "^(\\w+)_")[, 2]) %>%
    select(compartment, stratum, grts_address, grts_address_final) %>%
    group_by(compartment, grts_address, grts_address_final, geometry) %>%
    summarize(
      strata = str_flatten(sort(unique(stratum)), collapse = " | "),
      n_strata = length(unique(stratum)),
      .groups = "drop"
    ) %>%
    write_sf(path_gpkg, layer = layername, delete_layer = TRUE)
}
