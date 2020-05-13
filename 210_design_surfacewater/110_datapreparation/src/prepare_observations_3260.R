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
#     st_read("data/10_input/binary/HT3260/2020-05-08_HT3260.shp",
#             crs = 31370,
#             as_tibble = TRUE)
#
# habstreams_full %>% st_write(file_path)
#
# drive_upload(media = file_path,
#              path = as_id("1DW4VGaITvBdgf-iJ9fwjZDDrW1LZGy9G"))
#
# The above steps are to be repeated (drive_update()) for subsequent versions.
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

observations_3260_sf <-
    habstreams_full %>%
    select(id = OBJECTID,
           name = naam) %>%
    arrange(id)

observations_3260 <-
    habstreams_full %>%
    st_drop_geometry %>%
    select(id = OBJECTID,
           contains(c("datum", "Typ")))
    # to be continued

## 3. write the results
####################################################



