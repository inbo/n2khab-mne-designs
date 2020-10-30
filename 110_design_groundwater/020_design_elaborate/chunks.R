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



