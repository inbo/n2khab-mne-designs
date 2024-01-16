if (Sys.getenv("GARGLE_OAUTH_EMAIL") != "") {
  options(gargle_oauth_email = Sys.getenv("GARGLE_OAUTH_EMAIL"))
}
if (Sys.getenv("GARGLE_OAUTH_CACHE") != "") {
  options(gargle_oauth_cache = Sys.getenv("GARGLE_OAUTH_CACHE"))
}

library(googlesheets4)

gs_id <- "1vqxRmWVuc39HCF15K6xZhJEa5ls7Z7uF_xsEr_swQdk"
n2khab_types_expanded_properties %>%
  select(type, grts_join_method, sample_support) %>%
  inner_join(
    read_types(lang = lang) %>%
      select(type, type_shortname),
    by = "type"
  ) %>%
  relocate(type_shortname, .after = type) %>%
  write_sheet(
    ss = gs_id,
    sheet = "type_properties")

schemes %>%
  select(-spatial_restriction) %>%
  inner_join(
    read_schemes(lang = lang) %>%
      select(
        scheme,
        # attribute_1,
        attribute_1_name,
        # attribute_2,
        attribute_2_name
      ),
    by = "scheme"
  ) %>%
  relocate(attribute_3, tag_1, .after = last_col()) %>%
  write_sheet(gs_id, "scheme_properties")
