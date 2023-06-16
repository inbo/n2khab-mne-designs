#' @title Retrieve unique fysicochemical variables from aquachem database.
#' @description  The function can only be used from within the INBO network.
#' @export aquachem_variables
#' @import dplyr

aquachem_variables<- function(){tbl(connect_aquachem(), "FactResul=tAqua")%>%dplyr::select(Component)%>% dplyr::distinct ()%>% collect()%>% arrange()}
