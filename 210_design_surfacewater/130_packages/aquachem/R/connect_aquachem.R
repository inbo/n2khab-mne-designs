#' Connect to the INBO aquachem database
#'
#' Returns a connection to the INBO aquachem database.
#' The function can only be used from within the INBO network.
#' @export connect_aquachem
#' @importFrom inbodb connect_inbo_dbase
connect_aquachem <-function() {
    connect_inbo_dbase("M0003_00_Aquachem", autoconvert_utf8 = TRUE)
}
