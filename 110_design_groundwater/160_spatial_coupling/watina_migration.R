
# https://shrektan.com/post/2019/07/26/create-a-database-connection-that-can-be-disconnected-automatically/
# use close_fun so that it not only works for DBI but could also be used for RODBC, etc.
reg_conn_finalizer <- function(conn, close_fun, envir) {
  is_parent_global <- identical(.GlobalEnv, envir)
  if (isTRUE(is_parent_global)) {
    env_finalizer <- new.env(parent = emptyenv())
    env_finalizer$conn <- conn
    attr(conn, 'env_finalizer') <- env_finalizer
    reg.finalizer(env_finalizer, function(e) {
      # print('global finalizer!')
      try(close_fun(e$conn))
    }, onexit = TRUE)
  } else {
    withr::defer({
      # print('local finalizer!')
      try(close_fun(conn))
    }, envir = envir, priority = "last")
  }
  conn
}


#' Connect to the INBO Watina database
#'
#' Returns a connection to the INBO \strong{Watina} database,
#' namely `sql08.W0002_10_Watina`.
#' The function can only be used from within the INBO network.
#'
#' Don't forget to disconnect at the end of your R-script using
#' \code{\link{dbDisconnect}}!
#'
#' @return
#' A \code{DBIConnection} object.
#'
#' @examples
#' \dontrun{
#' watina <- connect_watina()
#' # Do your stuff.
#' # Disconnect:
#' dbDisconnect(watina)
#' }
#'
#' @export
#' @importFrom inbodb connect_inbo_dbase
connect_watina <- function() {
  # connect to the *new* data warehouse
  database_name <- "W0002_10_Watina"
  watina_dwh <- inbodb::connect_inbo_dbase(database_name,
                         autoconvert_utf8 = TRUE)

  # https://github.com/inbo/inbodb/issues/59
  reg_conn_finalizer(watina_dwh, DBI::dbDisconnect, parent.frame())

  # message(DBI::dbListConnections())
  message(paste0("succesfully connected to the new watina data warehouse, ", database_name))

  return(watina_dwh)
}
