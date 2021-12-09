# Databehandeling op basis van aanvankelijke verkenning:
##################################################################

## ---- envdata-preprocessing-discard-faulty-data



## ---- envdata-standardise-response

gw51a <-
    gw51a %>%
    group_by(loc_code) %>%
    arrange(loc_code, hydroyear) %>%
    mutate(
        hg3_first = first(hg3_ost),
        lg3_std = lg3_ost - hg3_first,
        hg3_std = hg3_ost - hg3_first
    ) %>%
    select(-hg3_first) %>%
    relocate(lg3_std, hg3_std, .before = cluster_id) %>%
    ungroup


# Data modifications for modelling purposes:
##################################################################

## ---- envdata-use-typeclusters-as-types-and-be-selective

gw51t_temp <-
    gw51t %>%
    inner_join(read_vc("typeclusters_gw51t",
                       root = "data/10_input"),
               by = "type")
if (sum(is.na(gw51t_temp$use_data_in_model)) > 0) {
    stop("gw51t provides data of types for which it's unclear whether ",
         "the data have to be used or not. ",
         "Make this clear in the typeclusters_gw51t table please ",
         "(use_data_in_model).\n",
         "It's about following type(s) and number of observations: \n",
         gw51t_temp %>% filter(is.na(use_data_in_model)) %>% count(type) %>%
             as.matrix %>% paste(collapse = " "))
}
gw51t <-
    gw51t_temp %>%
    mutate(type = type_model) %>%
    filter(use_data_in_model) %>%
    distinct %>%
    select(-type_model, -use_data_in_model)
rm(gw51t_temp)

## ---- envdata-dropfactorlevels

gw51t <-
    gw51t %>%
    mutate_if(is.factor, droplevels)
gw51a <-
    gw51a %>%
    mutate_if(is.factor, droplevels)
gw33 <-
    gw33 %>%
    mutate_if(is.factor, droplevels)

## ---- envdata-standardize-year

gw51t <-
    gw51t %>%
    mutate(hydroyear_std = hydroyear - 1989)
gw51a <-
    gw51a %>%
    mutate(hydroyear_std = hydroyear - 1989)
gw33 <-
    gw33 %>%
    mutate(year_std = year - 1989)



