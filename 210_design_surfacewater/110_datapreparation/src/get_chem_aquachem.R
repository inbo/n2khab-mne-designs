get_chem_aquachem <- function(locs,
         con,
         parameter= NULL,
         collect = FALSE) {



        if (inherits(locs, "data.frame")) {
        locs <-
            locs %>%
            distinct(.data$loc_code)

        try(db_drop_table(con, "##locs"),
            silent = TRUE)

        locs <-
            copy_to(con,
                    locs,
                    "##locs")%>%
            inner_join(tbl(con, "FactResultAqua") %>%
                           select(loc_code = .data$CODE),
                       .,
                       by = "loc_code")
    }



    chem <-
        tbl(con, "FactResultAqua") %>%
        select(.data$FieldSampleID,
               .data$FieldSamplingDate,
                .data$Component,
               loc_code = .data$CODE,
               .data$ResultFormattedNumeric,
               .data$Unit,
               #.data$MeetwaardeMEQ,
               .data$IsBelowLOQ) %>%
        inner_join(locs, by = "loc_code") %>% distinct

    if (!is.null(parameter)) {
        chem <-
            chem %>%
            filter(.data$Component %in% parameter)}else{
                chem}
    if(collect == FALSE){chem} else {chem = chem%>% collect()}

}
