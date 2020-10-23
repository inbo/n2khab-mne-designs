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
