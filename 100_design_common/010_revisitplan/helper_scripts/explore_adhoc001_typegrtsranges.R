# Determining positions of GRTS addresses of another type that is to be phased
# out, in the GRTS series of a possible target type

# First run setup chunk
#
# Then run:

load(file.path(datapath, "binary/results/objects_panflpan5.RData"))

module_domain_scheme_ps_stratum_sample_size %>%
  filter(
    str_detect(type, "6510_hu(a|s)?$|6410_(ve|mo)|6230_hmo"),
    # domain == "Flanders",
    scheme == "GW_03.3"
  ) %>%
  pivot_wider(
    id_cols = c(stratum, domain, nunits),
    names_from = panel_set,
    names_prefix = "PS",
    values_from = sp_sample_size_all_panels_stratum
  ) %>%
  mutate(total_ssize = PS1 + PS2)

estimate_new_sample_position <- function(unit_rank,
                                         type_source,
                                         typeset_target,
                                         scheme_limit) {
  unit_rank <- as.integer(unit_rank)
  df_source <-
    scheme_moco_ps_stratum_sppost_spsamples %>%
    filter(
      scheme == scheme_limit,
      stratum == type_source
    ) %>%
    unnest(sp_poststr_samples) %>%
    summarize(
      grts_address_source = grts_address[unit_rank],
      .by = c(sp_poststratum, panel_set)
    ) %>%
    mutate(
      sp_poststratum = fct_expand(
        sp_poststratum,
        "Flanders_remainder",
        "Flanders"
      ) %>%
        replace_values("Flanders" ~ "Flanders_remainder")
    ) %>%
    rename(panel_set_source = panel_set)

  scheme_moco_ps_stratum_sppost_spsamples_spares %>%
    filter(
      scheme == scheme_limit,
      stratum %in% typeset_target
    ) %>%
    select(-scheme, -module_combo_code) %>%
    mutate(
      grts_position_source = unit_rank,
      source = list(df_source),
      position_detection = map2(
        sp_poststr_samples_spareunits,
        source,
        function(samp, src) {
          samp %>%
            mutate(
              sp_poststratum = fct_expand(
                sp_poststratum,
                "Flanders_remainder",
                "Flanders"
              ) %>%
                replace_values("Flanders" ~ "Flanders_remainder")
            ) %>%
            inner_join(
              src,
              join_by(sp_poststratum),
              relationship = "many-to-many",
              unmatched = "drop"
            ) %>%
            summarize(
              # rank position of source type in GRTS series of target type (if
              # this is NA, it is because there is no grts_address_source
              # available because unit_rank is higher than available sample
              # size)
              grts_position =
                as.integer(sum(grts_address < grts_address_source) + 1),
              # sample size of target type within panel set x sp_poststratum
              sample_size = sum(sample_status == "in_sample"),
              # relative position wrt sample size (> 1 means it is in the spare
              # units, if at all)
              grts_rel_position = grts_position / sample_size,
              # are there actually spare units that supersede the source
              # position?
              any_spare_left = sum(
                grts_address[sample_status == "spare_unit"] >
                  first(grts_address_source)
              ) > 0,
              .by = c(sp_poststratum, panel_set_source)
            )
        }
      )
    ) %>%
    select(-sp_poststr_samples_spareunits, -source) %>%
    unnest(position_detection) %>%
    relocate(grts_position_source, .after = panel_set_source) %>%
    mutate(
      source_unit_remains_relevant = !(panel_set == 2 & grts_position == 1) &
        between(grts_rel_position, 0, 2) & any_spare_left
    )
}



# Applying the function ---------------------------------------------------

scheme_0 <- "GW_03.3"
# scheme_0 <- "SOIL_03.2"

## type_source is 6410_ve ------------------

estimate_new_sample_position(
  unit_rank = 1,
  type_source = "6410_ve",
  typeset_target = c("6410_mo", "6230_hmo"),
  scheme_limit = scheme_0
)

estimate_new_sample_position(
  unit_rank = 2,
  type_source = "6410_ve",
  typeset_target = c("6410_mo", "6230_hmo"),
  scheme_limit = scheme_0
)

estimate_new_sample_position(
  unit_rank = 3,
  type_source = "6410_ve",
  typeset_target = c("6410_mo", "6230_hmo"),
  scheme_limit = scheme_0
)

estimate_new_sample_position(
  unit_rank = 4,
  type_source = "6410_ve",
  typeset_target = c("6410_mo", "6230_hmo"),
  scheme_limit = scheme_0
)

estimate_new_sample_position(
  unit_rank = 6,
  type_source = "6410_ve",
  typeset_target = c("6410_mo", "6230_hmo"),
  scheme_limit = scheme_0
)

## type_source is 6510_hus ------------------

estimate_new_sample_position(
  unit_rank = 1,
  type_source = "6510_hus",
  typeset_target = c("6510_hu", "6510_hua"),
  scheme_limit = scheme_0
)

estimate_new_sample_position(
  unit_rank = 2,
  type_source = "6510_hus",
  typeset_target = c("6510_hu", "6510_hua"),
  scheme_limit = scheme_0
)

estimate_new_sample_position(
  unit_rank = 3,
  type_source = "6510_hus",
  typeset_target = c("6510_hu", "6510_hua"),
  scheme_limit = scheme_0
)

estimate_new_sample_position(
  unit_rank = 4,
  type_source = "6510_hus",
  typeset_target = c("6510_hu", "6510_hua"),
  scheme_limit = scheme_0
)

estimate_new_sample_position(
  unit_rank = 5,
  type_source = "6510_hus",
  typeset_target = c("6510_hu", "6510_hua"),
  scheme_limit = scheme_0
)

estimate_new_sample_position(
  unit_rank = 6,
  type_source = "6510_hus",
  typeset_target = c("6510_hu", "6510_hua"),
  scheme_limit = scheme_0
)

estimate_new_sample_position(
  unit_rank = 8,
  type_source = "6510_hus",
  typeset_target = c("6510_hu", "6510_hua"),
  scheme_limit = scheme_0
)

estimate_new_sample_position(
  unit_rank = 12,
  type_source = "6510_hus",
  typeset_target = c("6510_hu", "6510_hua"),
  scheme_limit = scheme_0
)

