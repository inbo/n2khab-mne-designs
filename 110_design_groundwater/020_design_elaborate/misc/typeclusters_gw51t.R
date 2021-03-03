library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)
library(n2khab)
gsheet <- as_sheets_id("1XTWmGThdZQcdIXOeQNFy6yAsultkVXZNjh0SfwxVPUg")

result <- read_sheet(gsheet)
result

glimpse(result)

# defining names of typeclusters ('type_model') and writing to vc-file

result %>%
    group_by(typegroup_small) %>%
    select(typegroup_small, type, nrobs_xg3, use_data_in_model_def) %>%
    arrange(typegroup_small) %>%
    mutate(type_model = ifelse(nrobs_xg3 == max(nrobs_xg3),
                               type,
                               NA_character_) %>% {first(.[!is.na(.)])},
           ntypes_model = sum(use_data_in_model_def, na.rm = TRUE)) %>%
    ungroup %>%
    mutate(type = factor(type, levels = levels(read_types()$type)),
           type_model = factor(type_model, levels = levels(type)) %>% droplevels,
           type_model = plyr::mapvalues(type_model, type_model,
                                        ifelse(ntypes_model > 1,
                                               str_c(type_model, "_clus"),
                                               as.character(type_model)),
                                        warn_missing = FALSE),
           use_data_in_model = use_data_in_model_def == 1) %>%
    select(type,
           type_model,
           use_data_in_model) %>%
    git2rdata::write_vc("typeclusters_gw51t",
                        root = "data/10_input",
                        sorting = "type",
                        optimize = FALSE)



