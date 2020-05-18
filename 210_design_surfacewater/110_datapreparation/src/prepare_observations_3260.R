library(googledrive)
library(tidyverse)
library(sf)

file_path <- file.path(tempdir(), "habstreams_full.gpkg")

###############################################################################
# Preparatory work to obtain a single-file layer on (private) GDrive:
# 1. manually export line layer from latest (large) ESRI personal geodatabase
#    as shapefile:
#    PRJ_Macrofyten\habitats\HT3260_Habitatkaart\Habitatkaart\Habitatkaart_3260_v_1_7.mdb > Habitatkaart > line features: H3260_v1_7_versie2
# 2. load it and write it as a GeoPackage + upload it to GDrive:
#
# habstreams_full <-
#     st_read("data/10_input/binary/HT3260/2020-05-18_HT3260.shp",
#             crs = 31370,
#             as_tibble = TRUE)
#
# habstreams_full %>% st_write(file_path)
#
# drive_update(media = file_path,
#              file = as_id("1siKWVAj9tCyE--0viJQr18PjZ8b8SfST"))
#
# The above steps are to be repeated for subsequent versions.
# Only the result of the below script is written into the git repo.
################################################################################

## 1. load the data
####################################################

drive_download(as_id("1siKWVAj9tCyE--0viJQr18PjZ8b8SfST"),
               path = file_path,
               overwrite = TRUE)

habstreams_full <- st_read(file_path,
                           as_tibble = TRUE,
                           stringsAsFactors = FALSE)

## 2. split the data in spatial sf and long tibble
####################################################

# spatial sf object:

lines_presabs_3260 <-
    habstreams_full %>%
    select(id = OBJECTID,
           name = naam) %>%
    arrange(id)

# long tibble (id = common identifier between both objects):

presabs_3260 <-
    habstreams_full %>%
    st_drop_geometry %>%
    select(id = OBJECTID,
           contains("_datum")) %>%
    pivot_longer(cols = contains("datum"),
                 names_to = "dates_orig",
                 values_to = "dates") %>%
    bind_cols(
        habstreams_full %>%
            st_drop_geometry %>%
            select(id2 = OBJECTID,
                   contains("_Typ")) %>%
            pivot_longer(cols = contains("Typ"),
                         names_to = "species_orig",
                         values_to = "species")
    ) %>%
    select(id, dates, species) %>%
    filter(!is.na(dates)) %>%
    # delete whitespaces:
    mutate_at(vars(dates, species),
              function(x) str_replace_all(x, " ", "")) %>%
    separate(dates,
             into = str_c("date", 1:10),
             sep = "/",
             fill = "right") %>%
    separate(species,
             into = str_c("species", 1:10),
             sep = "/",
             fill = "right") %>%
    {tibble(id = rep(.$id, 10),
            date =
                select(., contains("date")) %>%
                unlist(use.names = FALSE),
            species =
                select(., contains("species")) %>%
                unlist(use.names = FALSE)
            )} %>%
    # recycling species that are valid for multiple dates:
    separate(date,
             into = str_c("date", 1:10),
             sep = ",|en",
             fill = "right") %>%
    pivot_longer(cols = contains("date"),
                 names_to = "date_rank",
                 values_to = "date",
                 values_drop_na = TRUE) %>%
    mutate(year = str_sub(date, 1, 4)) %>%
    mutate(year = as.numeric(year),
           is_3260 = species != "-") %>%
    select(id, year, is_3260, species, date) %>%
    arrange(id, year, date, species)

# presabs_3260 aggregated by year:
presabs_3260_y <-
    presabs_3260 %>%
    group_by(id, year) %>%
    summarise(is_3260 = any(is_3260, na.rm = TRUE)) %>%
    ungroup

## 3. write the results
####################################################

### spatial object (git-ignored):

lines_presabs_3260 %>%
    st_write("data/20_output/lines_presabs_3260.gpkg",
             delete_dsn = TRUE)
drive_update(media = "data/20_output/lines_presabs_3260.gpkg",
             file = as_id("118j_Td_Xb0A9MVBG04QEACPuPXursUlp"))

# can be read back in with:
# filepath2 <- file.path(tempdir(), "lines_presabs_3260.gpkg")
# drive_download(as_id("118j_Td_Xb0A9MVBG04QEACPuPXursUlp"),
#                path = filepath2,
#                overwrite = TRUE)
# lines_presabs_3260 <- read_sf(filepath2)

### presabs_3260 (versioned in git):

presabs_3260 %>% write_tsv("data/20_output/presabs_3260.tsv")

# can be read back in with:
# presabs_3260 <- read_tsv("data/20_output/presabs_3260.tsv")
# note: presabs_3260_y can be regenerated from presabs_3260 as done
# above
