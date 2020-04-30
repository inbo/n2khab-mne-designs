library(git2rdata)
library(tidyverse)
library(n2khab)
library(sf)
rivulets_7220 <-
    read_habitatsprings(filter_hab = TRUE) %>%
    filter(type == "7220",
           system_type %in% c("rivulet",
                              "unknown"))
riv7220_chem <-
    # de point_id attributen in onderstaande csv-file zullen we niet behouden,
    # want dat is beschikbaar in de authentieke 'habitatsprings' databron,
    # hierboven beperkt  tot de relevante locaties (rivulets_7220)
    read_csv2("data/10_input/habitatsprings_abio_v3.csv",
              col_types = cols(
                  X1 = col_double(),
                  Gebied = col_character(),
                  Labo_ID_db = col_character(),
                  Datum = col_date(format = "%d/%m/%Y"),
                  VEGID_INBOVEG = col_character(),
                  id_n2khab = col_double(),
                  system_type = col_character(),
                  habitattype = col_character(),
                  sbz = col_double(),
                  geometry = col_character(),
                  AfstandBron = col_character(),
                  Kalktuf = col_double(),
                  Schaduw = col_double(),
                  Periode = col_character(),
                  pH_veld = col_character(), # problem
                  pH_labo = col_double(),
                  EC_veld_25grC = col_double(),
                  EC_labo_25grC = col_double(),
                  Buffercapaciteit_TAP = col_double(),
                  Buffercapaciteit_TAM = col_double(),
                  HCO3 = col_double(),
                  CO3 = col_double(),
                  OH = col_double(),
                  SO4 = col_double(),
                  Cl = col_double(),
                  PO4 = col_double(),
                  NO2 = col_double(),
                  NO3 = col_double(),
                  NH4 = col_double(),
                  Ca = col_double(),
                  K = col_double(),
                  Mg = col_double(),
                  Na = col_double(),
                  Mn = col_character(), # problem
                  Al = col_character(), # problem
                  Fe = col_double(),
                  SO4_S = col_double(),
                  PO4_P = col_double(),
                  NO2_N = col_double(),
                  NO3_N = col_double(),
                  NH4_N = col_double()
              )) %>%
        mutate(Al = as.numeric(Al),
               Mn = as.numeric(Mn),
               pH_veld = as.numeric(pH_veld)) %>%
    select(point_id = id_n2khab,
           releve_code = VEGID_INBOVEG,
           period = Periode,
           date = Datum,
           lab_code = Labo_ID_db,
           11:last_col()) %>%
    rename(dist_to_source = AfstandBron) %>%
    inner_join(rivulets_7220 %>%
                   st_drop_geometry %>%
                   select(point_id, unit_id), .,
               by = "point_id")
glimpse(riv7220_chem)

# unit_id: de populatie-eenheid, dus in modellering alleszins een
# afhankelijkheid te voorzien tussen de point_id's binnen dezelfde unit_id.
# De interesse gaat naar de variabiliteit per unit_id.

riv7220_chem %>%
    select(dist_to_source, Kalktuf, Schaduw) %>%
    lapply(table)

# dist_to_source: A (bron zelf),B (begin kalktufafzetting),C (midden tot einde kalktufafzetting) en D (na de kalktufafzetting)

# We zijn meest geïnteresseerd in kwaliteit naar het einde van de habitatvlek toe,
# dus positie C of D t.ov.v. de bron (= point_id).
# Eventueel positie 'B' toevoegen om voldoende data te bekomen.

# check unieke rijen:
riv7220_chem %>%
    group_by(point_id, date, dist_to_source) %>%
    summarise(nr_rel = n_distinct(releve_code)) %>%
    filter(nr_rel > 1) %>%
    inner_join(riv7220_chem) %>%
    select(1:14, -unit_id, -nr_rel, -period)

# voorbereiding ivm toekenning tijdsafhankelijke bepaalbaarheidsgrenzen:
riv7220_chem %>%
    # filter(!is.na(PO4)) %>%
    count(date)

riv7220_chem %>%
    ggplot(aes(x = NO3)) +
    geom_histogram()

# nog niet uitgevoerd:
riv7220_chem %>% write_vc("riv7220_chem",
                          root = "data/20_output/",
                          sorting = c("point_id",
                                      "date",
                                      "dist_to_source"))

