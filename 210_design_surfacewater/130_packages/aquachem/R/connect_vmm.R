#' @title Connect to the local copy of the VMM database
#' @description Returns a connection to the INBO aquachem database.
#' @description The function can only be used from within the INBO network.
#' @export connect_vmm
#' @importFrom inbodb connect_inbo_dbase
connect_vmm <-function() {
  connect_inbo_dbase("D0113_00_VMMData")
}

