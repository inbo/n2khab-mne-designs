#' @title Retrieve fysicochemical data from water samples in the aquachem database.
#' @description  The function can only be used from within the INBO network.
#' @export get_chem_aquachem
#' @import dplyr
#' @import sf
#' @import assertthat
#' @import stringr
#' @param locs exported locations from get_locs_aquachem command
#' @param con connection to aquachem database
#' @param variable search specific variable(s). To get an overview of the existing variables use aquachem_variables().
#' @param collect set to TRUE to collect data
#' @examples
#' \dontrun{get_chem_aquachem(locs,con, variable = c("NH4","PO4","NO3"))}


get_chem_aquachem <- function(locs,
         con = connect(aquachem),
         variable= NULL,
         collect = FALSE) {



        if (inherits(locs, "data.frame")) {
        locs <-
            locs %>%
            distinct(.data$loc_code)

        try(DBI::dbRemoveTable(con, "#locs"),
            silent = TRUE)

        locs <-
            copy_to(con,
                    locs,
                    "#locs") %>%
            dplyr::inner_join(tbl(con, "FactResultAqua",copy = TRUE) %>%
                           dplyr::select(loc_code = .data$CODE),
                       .,
                       by = "loc_code")
    }



    chem <-
        tbl(con, "FactResultAqua") %>%
        dplyr::select(.data$FieldSampleID,
               .data$FieldSamplingDate,
                .data$Component,
               loc_code = .data$CODE,
               .data$ResultFormattedNumeric,
               .data$Unit,
               #.data$MeetwaardeMEQ,
               .data$IsBelowLOQ) %>%
        dplyr::inner_join(locs, by = "loc_code",,copy = TRUE) %>% distinct

    if (!is.null(variable)) {
        chem <-
            chem %>%
            filter(.data$Component %in% variable)}else{
                chem}
    if(collect == FALSE){chem} else {chem = chem%>% collect()}
}
