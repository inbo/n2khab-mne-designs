# Foute data weren op basis van aanvankelijke verkenning:
##################################################################

## ---- envdata-preprocessing

if (gw51t %>% filter(loc_code == "MOLP029", lg3_lcl < -15) %>% nrow > 0) {
    gw51t <- gw51t %>% filter(loc_code != "MOLP029" | lg3_lcl >= -15)
}

