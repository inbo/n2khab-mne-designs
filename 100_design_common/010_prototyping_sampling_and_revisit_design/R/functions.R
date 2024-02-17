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
