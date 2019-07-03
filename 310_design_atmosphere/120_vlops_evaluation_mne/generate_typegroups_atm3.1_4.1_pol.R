### Generating typegroups_atm3.1_4.1_pol

# Load packages

library(n2khab)
library(sf)
library(raster)
library(tidyverse)

# Settings

datapath <- "../data"
datasetpath <- file.path(datapath,
                         "20_processed/typegroups_atm3.1_4.1_pol")
dir.create(datasetpath, recursive = TRUE)

# Creating list of types to join to habitatmap_stdized, which will define the target population.
# For the Dutch narrative, see the folder '110_vlops_datapreparation'

types_targetpop <-
    read_scheme_types() %>%
    filter(scheme %in% c("ATM_03.1", "ATM_04.1")) %>%
    select(1:3) %>%
    inner_join(read_types() %>%
                   select(1:3))
nrsubtypes <-
    read_types() %>%
    filter(typelevel == "subtype") %>%
    count(main_type, name = "nrsubtypes_checklist")
main_types_to_add <-
    types_targetpop %>%
    filter(typelevel == "subtype") %>%
    count(scheme, main_type, name = "nrsubtypes_targpop") %>%
    inner_join(nrsubtypes) %>%
    filter(nrsubtypes_targpop == nrsubtypes_checklist) %>%
    select(scheme,
           type = main_type)
main_types_to_add_group <-
    main_types_to_add %>%
    inner_join(types_targetpop,
               by = c("type" = "main_type",
                      "scheme")) %>%
    distinct(scheme, type, typegroup)
main_types_to_add_group <-
    main_types_to_add_group %>%
    filter(!(type == "2190" &
                 typegroup == "ATM_03.1_group1"))
subtypes_to_add_group <-
    types_targetpop %>%
    filter(typelevel == "main_type") %>%
    select(scheme,
           main_type = type,
           typegroup) %>%
    inner_join(read_types() %>%
                   filter(typelevel == "subtype") %>%
                   select(main_type, type)) %>%
    select(scheme, type, typegroup)
types_targetpop_ext <-
    types_targetpop %>%
    select(scheme, type, typegroup) %>%
    union(main_types_to_add_group) %>%
    union(subtypes_to_add_group) %>%
    mutate(type = factor(type,
                         levels = types_targetpop$type %>%
                             levels))

# Make selection from habitatmap_stdized

    # long-formatted tibble:
polyg_attributes <-
    read_habitatmap_stdized(datapath) %>%
    .$habitatmap_patches %>%
    inner_join(types_targetpop_ext, .) %>%
    select(-patch_id, -code_orig)

    # the associated polygons:
polyg_sel <-
    read_habitatmap_stdized(datapath) %>%
    .$habitatmap_polygons %>%
    select(-description_orig) %>%
    semi_join(polyg_attributes)

# Making the end result

typegroups_atm3.1_4.1_pol <-
    polyg_attributes %>%
    mutate(dummy = 1) %>%
    distinct(polygon_id, typegroup, dummy) %>%
    spread(key = typegroup,
           value = dummy,
           fill = 0) %>%
    inner_join(polyg_sel, .)
typegroups_atm3.1_4.1_pol

# Writing the end result

typegroups_atm3.1_4.1_pol %>%
    st_write(file.path(datapath,
                       "20_processed/typegroups_atm3.1_4.1_pol",
                       "typegroups_atm3.1_4.1_pol.gpkg"),
             layer = "typegroups_atm3.1_4.1_pol",
             driver = "GPKG",
             layer_options = "OVERWRITE=YES")

