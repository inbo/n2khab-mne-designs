library(dplyr)
library(tidyr)
library(stringr)
library(n2khab)

# defining names of typeclusters ('type_model') for GW_05.1_aq and writing to vc-file

targetpop %>%
    filter(scheme == "GW_05.1_aq") %>%
    distinct(type) %>%
    arrange(type) %>%
    mutate(type = droplevels(type),
           type_model =
               case_when(type == "3110" ~ "3130_aom",
                         type == "7220" ~ "3150",
                         TRUE ~ as.character(type)) %>%
               factor(levels = levels(type)) %>%
               droplevels,
           use_data_in_model = ifelse(type %in% c("3110", "7220"),
                                      FALSE,
                                      TRUE)) %>%
    git2rdata::write_vc("typeclusters_gw51a",
                        root = "data/10_input",
                        sorting = "type",
                        optimize = FALSE)



