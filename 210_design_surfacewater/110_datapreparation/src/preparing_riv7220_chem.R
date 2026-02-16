renv::restore()
library(git2rdata)
library(tidyverse)
library(n2khab)
library(sf)
library(lubridate)
rivulets_7220 <-
    read_habitatsprings(filter_hab = TRUE) %>%
    filter(type == "7220",
           system_type %in% c("rivulet",
                              "unknown"))

loq <-
    tribble(~date, ~loq_PO4, ~loq_NO3,
            dmy("19/04/2011"), 	0.2, 0.2,
            dmy("6/09/2011"), 	0.1, 0.1,
            dmy("16/11/2011"), 	0.1, 0.1,
            dmy("17/11/2011"), 	0.1, 0.1,
            dmy("28/02/2012"), 	0.05, 0.1,
            dmy("1/03/2012"), 	0.05, 0.1)

riv7220_chem <-
    # de point_id attributen in onderstaande csv-file zullen we niet behouden,
    # want dat is beschikbaar in de authentieke 'habitatsprings' databron,
    # hierboven beperkt  tot de relevante locaties (rivulets_7220)
    read_csv2("data/10_input/habitatsprings_abio_v3.csv",
              col_types = cols(
                  Datum = col_date(format = "%d/%m/%Y")
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
    inner_join(loq, by = "date") %>%
    # correcting non-constant N-P mass conversion:
    mutate(loq_PO4_P = loq_PO4 * mean(PO4_P / PO4),
           loq_NO3_N = loq_NO3 * mean(NO3_N / NO3),
           PO4_P = mean(PO4_P / PO4) * PO4,
           NO3_N = mean(NO3_N / NO3) * NO3) %>%
    select(-PO4, -NO3, -NO2, -NH4, -SO4_S, -loq_PO4, -loq_NO3) %>%
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

# dist_to_source: A (bron zelf),B (begin kalktufafzetting),C (midden tot einde
# kalktufafzetting) en D (na de kalktufafzetting). Dit geldt binnen 1 point_id,
# dus afhankelijkheid te voorzien.

# We zijn meest geïnteresseerd in kwaliteit naar het einde van de habitatvlek toe,
# dus positie C of D t.ov.v. de bron (= point_id).
# Eventueel positie 'B' toevoegen om voldoende data te bekomen.

# covariabelen kalktuf en schaduw: idd ordinaal met waarden 1 tem 5. De
# hoeveelheid kalktuf werd ingeschat op het niveau van de vegetatieplot in 5
# categoriën (1 = geen, 2 =  sporen, 3 =  weinig, 4 =  matig, 5 = veel). De
# beschaduwing werd in 5 klasses in te schatten, met name 1 = geen schaduw, 2 =
# weinig, 3 = matig, 4 = veel, 5 = volledig beschaduwd.

# De bepaalbaarheidsgrenzen variëren doorheen de tijd, zie variabelen loq_xxx
# (LOQ = limit of quantification). In de brondataset zijn de waarden beneden LOQ
# ingesteld op LOQ/2, wat niet ideaal is om mee te werken en dus beter anders
# wordt aangepakt.

# output dataset wegschrijven:
riv7220_chem %>% write_vc("riv7220_chem",
                          root = "data/20_output/",
                          sorting = c("point_id",
                                      "date",
                                      "dist_to_source"))

