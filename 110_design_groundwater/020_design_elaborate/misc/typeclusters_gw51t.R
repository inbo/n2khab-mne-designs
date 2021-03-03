library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)
library(n2khab)
gsheet <- as_sheets_id("1XTWmGThdZQcdIXOeQNFy6yAsultkVXZNjh0SfwxVPUg")
# read_scheme_types(lang = "nl", extended = TRUE) %>%
#     select(scheme,
#            type, typegroup, typegroup_shortname,
#            type_shortname, hydr_class, hydr_class_shortname,
#            groundw_dep, groundw_dep_shortname) %>%
#     filter(scheme == "GW_05.1_terr") %>%
#     left_join(dataset %>%
#                   count(type, name = "nrobs_xg3"),
#               by = "type") %>%
#     mutate(nrobs_xg3 = ifelse(is.na(nrobs_xg3), 0, nrobs_xg3),
#            typegroup_small = str_c(typegroup, "_00")) %>%
#     relocate(nrobs_xg3, typegroup_small, .after = type) %>%
#     arrange(typegroup, desc(hydr_class), desc(groundw_dep), type) %>%
#     write_sheet(ss = gsheet, sheet = 1)

result <- read_sheet(gsheet)
result

# first screenings

result %>%
    count(typegroup_small) %>% as.data.frame()
result %>%
    group_by(typegroup_small) %>%
    summarise(ntypes = n(),
              nrobs = sum(nrobs_xg3)) %>% as.data.frame()
result %>%
    group_by(typegroup_small) %>%
    arrange(typegroup_small, desc(nrobs_xg3)) %>%
    mutate(nrobs = sum(nrobs_xg3),
           type_num = str_c("type", row_number()),
           type_obs = str_c(type, " (", nrobs_xg3, ")")) %>%
    pivot_wider(id_cols = c(typegroup_small, nrobs) ,
                names_from = type_num,
                values_from = type_obs,
                values_fill = "") %>% View()

result %>%
    mutate(small = str_match(typegroup_small, "_(\\d+)$")[,2]) %>%
    select(Dries, small) %>%
    mutate(test = Dries == small) %>%
    pull(test) %>% all

result %>%
    mutate(typegroup_test = str_match(typegroup_small, "^(.+)_\\d+$")[,2]) %>%
    transmute(test = typegroup == typegroup_test) %>%
    pull(test) %>% all

all(is.na(result$use_data_in_model) == (result$nrobs_xg3 == 0))
