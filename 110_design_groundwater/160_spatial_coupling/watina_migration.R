
#----------------
#--- HELPERS ----
#----------------

#' Wrapper to close idle database connection.
#'
#' Provides a finalizer for a database connection to close
#' automatically upon garbage collection.
#' reference: https://shrektan.com/post/2019/07/26/create-a-database-connection-that-can-be-disconnected-automatically/
#' currently in testing mode with extra verbosity to count
#' relevance and confirm functionality.
#'
#' @keywords internal
#'
#' @return
#' A \code{DBIConnection} object.
#'
reg_conn_finalizer <- function(conn, close_fun, envir) {
  is_parent_global <- identical(.GlobalEnv, envir)
  if (isTRUE(is_parent_global)) {
    env_finalizer <- new.env(parent = emptyenv())
    env_finalizer$conn <- conn
    attr(conn, 'env_finalizer') <- env_finalizer
    reg.finalizer(env_finalizer, function(e) {
      print('Closed watina connection (global env).')
      try(close_fun(e$conn))
    }, onexit = TRUE)
  } else {
    withr::defer({
      print('Closed watina connection (local env).')
      try(close_fun(conn))
    }, envir = envir, priority = "last")
  }
  return(conn)
}


#' Connect to the INBO Watina database
#'
#' Returns a connection to the INBO \strong{Watina} database,
#' namely `sql08.W0002_10_Watina`.
#' The function can only be used from within the INBO network.
#' Database connection will automatically disconnect upon GC.
#'
#' @param database_name choose a database, per default this will
#' connect to the new watina data warehouse ("watina10")
#'
#' @return
#' A \code{DBIConnection} object.
#'
#' @examples
#' \dontrun{
#' watina_dwh <- connect_watina()
#' # Do your stuff.
#' }
#'
#' @export
#' @importFrom inbodb connect_inbo_dbase
connect_watina <- function(database_name = "W0002_10_Watina") {
  # connect to the *new* data warehouse per default
  watina_dwh <- inbodb::connect_inbo_dbase(database_name,
                         autoconvert_utf8 = TRUE)

  # https://github.com/inbo/inbodb/issues/59
  # reg_conn_finalizer(watina_dwh, DBI::dbDisconnect, parent.frame())
  # ISSUE: closes when leaving parent.frame, e.g. in `source(...)`

  # message(DBI::dbListConnections())
  message(paste0("succesfully connected to the new watina data warehouse, ", database_name))

  return(watina_dwh)
}


#' Get table list from a database
#'
#' Returns a list of all tables in a database,
#' per default connecting to `watina` db.
#'
#' @return
#' A \code{list} with table names.
get_db_table_list <- function( conn = NULL ) {

  # availability of assertthat and other packages
  stopifnot(assertthat = require('assertthat'),
            DBI = require('DBI'),
            dbplyr = require('dplyr'),
            magrittr = require('magrittr')
            )

  # if no connection is given, open one
  if (is.null(conn)) {
    # NOTE: this is just a special case in the mne environment.
    conn <- connect_watina()
  }

  # confirm that the connection is a DBI connection
  assert_that(inherits(conn, "DBIConnection"))

  # query the list of tables
  table_list <- tbl(conn, "sysobjects") %>%
    filter(xtype == "U") %>%
    pull(name)
  return(table_list)
}


#---------------
#--- WATINA ----
#---------------

#' Get locations from the database
#'
#' Returns locations (and optionally, observation wells) from the \emph{Watina}
#' database that meet
#' several criteria, either as a lazy object or as a
#' local tibble.
#' Criteria refer to spatial or non-spatial physical attributes of the
#' location or the location's observation wells.
#' Essential metadata are included in the result.
#'
get_locs <- function(conn,
                     filterdepth_range = c(0, 3),
                     filterdepth_guess = FALSE,
                     filterdepth_na = FALSE,
                     obswells = FALSE,
                     obswell_aggr = c("latest",
                                      "latest_fd",
                                      "latest_sso",
                                      "mean"),
                     mask = NULL,
                     join_mask = FALSE,
                     buffer = 10,
                     bbox = NULL,
                     area_codes = NULL,
                     loc_type = c("P", "S", "R", "N", "W", "D", "L", "B"),
                     loc_validity = c("VLD", "ENT"),
                     loc_vec = NULL,
                     collect = FALSE) {

  # availability of assertthat and other packages
  stopifnot(assertthat = require("assertthat"),
            dbplyr = require("dplyr"),
            magrittr = require("magrittr")
            )


  # assert correct data types
  assert_that(inherits(conn, "DBIConnection"))

  assert_that(is.numeric(filterdepth_range),
              length(filterdepth_range) == 2,
              filterdepth_range[1] <= filterdepth_range[2])

  assert_that(is.number(buffer))

  # bbox is either null, or a vector with limits, or an st_bbox without NA.
  assert_that(
    is.null(bbox) ||
    all(sapply(c("xmax", "xmin", "ymax", "ymin"),
              function (lim) lim %in% names(bbox)
    )) &
    all(!sapply(bbox, is.na)),
    msg = paste("You did not correctly specify bbox.",
                toString(names(bbox)), ": ", toString(bbox)
                ))

  assert_that(is.null(area_codes) | all(is.character(area_codes)))
  assert_that(is.null(loc_vec) | all(is.character(loc_vec)),
              msg = "loc_vec must be a character vector.")
  assert_that(is.flag(join_mask), assertthat::noNA(join_mask))
  assert_that(is.flag(collect), assertthat::noNA(collect))
  assert_that(is.flag(obswells), assertthat::noNA(obswells))
  assert_that(is.flag(filterdepth_guess), assertthat::noNA(filterdepth_guess))
  assert_that(is.flag(filterdepth_na), assertthat::noNA(filterdepth_na))

  obswell_aggr <- match.arg(obswell_aggr)

  # mask demands collection
  if (!is.null(mask) & !collect) {
    message("As a mask always invokes a collect(), the argument 'collect = FALSE' will be ignored.")
  }

  # mask must be an sf object
  if (!is.null(mask)) {
    assert_that(inherits(mask, "sf"),
                msg = "mask must be an sf object.")
    stopifnot(sf = "sf") # ensure package sf is available
    assert_that(sf::st_crs(mask) == sf::st_crs(31370),
                msg = "The CRS of mask must be Belgian Lambert 72 (EPSG-code 31370).")
  }

  # assert correct use of bbox
  if (!is.null(bbox)) {
    assert_that(bbox["xmax"] >= bbox["xmin"],
                bbox["ymax"] >= bbox["ymin"])
  }

  # confirm that location type is provided correctly
  if (missing(loc_type)) {
    loc_type <- match.arg(loc_type)
  } else {
    assert_that(
      all(loc_type %in% c("P", "S", "R", "N", "W", "D", "L", "B")),
      msg = "You specified at least one unknown loc_type."
    )
  }

  # confirm location validity choice
  assert_that(
    all(loc_validity %in% c("VLD", "ENT", "DEL", "CLD")),
    msg = "You specified at least one unknown loc_validity."
  )

  # unpack filter depth range
  min_filterdepth <- filterdepth_range[1]
  max_filterdepth <- filterdepth_range[2]



  ### locations: initial query
  locs <-
      tbl(conn, "DimMeetpunt") %>%
      filter(MeetpuntTypeCode %in% loc_type,
             MeetpuntStatusCode %in% loc_validity
             ) %>%
      left_join(tbl(conn, "DimGebied") %>%
                    select(GebiedWID,
                           GebiedCode,
                           GebiedNaam),
                by = "GebiedWID")


  # location subsets
  # (1) specified loc vector
  if (!is.null(loc_vec)) {
    locs <- locs %>%
      filter(MeetpuntCode %in% loc_vec)
  }

  # (2) specified area codes
  if (!is.null(area_codes)) {
    locs <- locs %>%
      filter(GebiedCode %in% area_codes)
  }

  # within bounding box
  if (!is.null(bbox)) {
    bbox_xmin <- unname(bbox["xmin"])
    bbox_xmax <- unname(bbox["xmax"])
    bbox_ymin <- unname(bbox["ymin"])
    bbox_ymax <- unname(bbox["ymax"])
    locs <- locs %>%
      filter(MeetpuntXCoordinaat >= bbox_xmin,
             MeetpuntXCoordinaat <= bbox_xmax,
             MeetpuntYCoordinaat >= bbox_ymin,
             MeetpuntYCoordinaat <= bbox_ymax)
  }

  # query "peilpunt": site
  site <- tbl(conn, "DimPeilpunt") %>%
    filter(
      PeilpuntStatusCode %in% c("VLD", "ENT", "CLD"),
      OpenbaarheidWID == 4
    ) %>%
    mutate(
      PeilpuntPlaatsing = sql("CAST(PeilpuntPlaatsing AS date)"),
      PeilpuntStopzetting = sql("CAST(PeilpuntStopzetting AS date)")
    )
  # TODO compare old Openbaarheid (was filtered for "PLME" // "UNKWN")
  #      to new table logic (OpenbaarheidWID == 4)

  locs <- locs %>%
    left_join(
      site,
      by = "MeetpuntWID",
      relationship = "many-to-many"
    ) %>%
    mutate(tubelength = ifelse(PeilbuisLengte <= 0, NA, PeilbuisLengte),
      filterlength = ifelse(is.na(FilterLengte), 0.3, FilterLengte),
      filterdepth = tubelength -
        ReferentieNiveauMaaiveld -
        filterlength / 2,
      filtertop_depth = tubelength -
        ReferentieNiveauMaaiveld,
      soilsurf_ost = ReferentieNiveauTAW - ReferentieNiveauMaaiveld
    ) %>%
    select(id = PeilpuntWID,
      loc_wid = MeetpuntWID,
      loc_code = MeetpuntCode,
      area_code = GebiedCode,
      area_name = GebiedNaam,
      x = MeetpuntXCoordinaat,
      y = MeetpuntYCoordinaat,
      loc_validitycode = MeetpuntStatusCode,
      loc_validity = MeetpuntStatus,
      loc_typecode = MeetpuntTypeCode,
      loc_typename = MeetpuntType,
      obswell_code = PeilpuntCode,
      obswell_rank = PeilpuntVersie,
      obswell_statecode = PeilpuntToestandCode,
      obswell_state = PeilpuntToestandNaam,
      obswell_installdate = PeilpuntPlaatsing,
      obswell_stopdate = PeilpuntStopzetting,
      soilsurf_ost,
      measuringref_ost = ReferentieNiveauTAW,
      tubelength,
      filterlength,
      filterdepth,
      filtertop_depth
    )

  # guess the filter depth
  if (filterdepth_guess) {
    locs <- locs %>%
      mutate(
        filterdepth_guessed = is.na(filterdepth) & !is.na(tubelength),
        filterdepth = ifelse(
          filterdepth_guessed == 1, # (sql: logical stored as bit)
          tubelength - filterlength / 2,
          filterdepth
        )
      )
  }

  # optionally include obs wells with missing filter values
  if (filterdepth_na) {
    locs <- locs %>%
      filter(
        (loc_typecode != "P") | # non-piezometers
        ( (loc_typecode == "P") &
          ((filterdepth <= max_filterdepth &
            filterdepth >= min_filterdepth) |
            is.na(filterdepth)
          ) # NA- or within-range filter depth
        ) # piezometers
      ) # filter
  } else {
    locs <- locs %>%
      filter(
        (loc_typecode != "P") | # non-piezometers
        ( (loc_typecode == "P") &
          (filterdepth <= max_filterdepth) &
          (filterdepth >= min_filterdepth)
        ) # piezometers with filter depth within range
      ) # filter
  }

  # optionally return all observation wells
  # if (obswells == FALSE): distinguish location
  # if (obswells == TRUE): distinguish successive installations at a location
  if (!obswells) {
    locs <- locs %>%
      group_by(loc_code) %>%
      mutate(obswell_count = n(),
        obswell_maxrank = max(obswell_rank, na.rm = TRUE),
        obswell_maxrank_fd = max(
          ifelse(is.na(filterdepth), NA, obswell_rank), na.rm = TRUE),
        obswell_maxrank_sso = max(
          ifelse(is.na(soilsurf_ost), NA, obswell_rank), na.rm = TRUE),
        obswell_statecode = max(
          ifelse(obswell_rank == obswell_maxrank, obswell_statecode, NA),
          na.rm = TRUE),
        obswell_state = max(
          ifelse(obswell_rank == obswell_maxrank, obswell_state, NA),
          na.rm = TRUE)
      ) # /mutate

    locs <-
      switch(obswell_aggr,
        "latest" = locs %>%
          ungroup() %>%
          filter(obswell_count == 1 |
            obswell_rank == obswell_maxrank
          ),
        "latest_fd" = locs %>%
          ungroup() %>%
          filter(
            (obswell_count == 1) |
            (obswell_rank == obswell_maxrank_fd) |
            ( is.na(obswell_maxrank_fd) &
              (obswell_rank == obswell_maxrank)
            )
          ),
        "latest_sso" = locs %>%
          ungroup() %>%
          filter(
            (obswell_count == 1) |
            (obswell_rank == obswell_maxrank_sso) |
            ( is.na(obswell_maxrank_sso) &
              (obswell_rank == obswell_maxrank)
            )
          ),
        "mean" = locs %>%
          mutate(
            soilsurf_ost = mean(soilsurf_ost, na.rm = TRUE),
            measuringref_ost = mean(measuringref_ost, na.rm = TRUE),
            filterdepth = mean(filterdepth, na.rm = TRUE),
            filtertop_depth = mean(filtertop_depth, na.rm = TRUE),
            filterlength = mean(filterlength, na.rm = TRUE),
            tubelength = mean(tubelength, na.rm = TRUE)
          ) %>%
          {if ("filterdepth_guessed" %in% colnames(.)) {
            mutate(.,
              filterdepth_guessed = max(
                ifelse(filterdepth_guessed == 1, # (sql: logical stored as bit)
                  1, 0),
              na.rm = TRUE)
            ) %>%
            mutate(
              filterdepth_guessed = sql("CAST(filterdepth_guessed AS bit)"))
            } else .
          } %>%
          ungroup() %>%
          filter((obswell_count == 1) | (obswell_rank == obswell_maxrank))
        ) %>% # / switch
        select(-obswell_code,
               -obswell_rank,
               -obswell_installdate,
               -obswell_stopdate,
               -obswell_count,
               -obswell_maxrank,
               -obswell_maxrank_fd,
               -obswell_maxrank_sso
        )
  } # /if (obswells)

  # apply mask
  if (!is.null(mask)) {
    locs <- locs %>%
      select(-loc_wid) %>%
      collect

    nr_dropped_locs <- locs %>%
      filter(is.na(x) | is.na(y)) %>%
      count %>%
      .$n

    if (nr_dropped_locs > 0) {
          warning("Dropped ",
                  nr_dropped_locs,
                  " locations from which x or y coordinates were missing.\n")
    }

    locs <- locs %>%
      filter(!is.na(x), !is.na(y)) %>%
      arrange(area_code, loc_code) %>%
      as_points(warn_dupl = FALSE)

    if (buffer != 0) {
      mask_expand <- mask %>%
        sf::st_buffer(dist = buffer)
    } else {
      mask_expand <- mask
    }

    if (join_mask) {
      locs <- locs %>%
        sf::st_join(mask_expand, left = FALSE) %>%
        sf::st_drop_geometry()
    } else {
      locs <- locs %>%
        .[mask_expand, ] %>%
        sf::st_drop_geometry()
    }

  } # /if(mask)


  # collect (incase not already done for mask)
  if (collect & is.null(mask)) {
    locs <- locs %>%
      select(-loc_wid) %>%
      collect %>%
      arrange(area_code, loc_code)
  }

  # check & report position duplicates
  # TODO: re-activate
  if (FALSE && inherits(locs, "data.frame")) {
    warn_xy_duplicates(locs$x, locs$y)
  }

  return(locs)

} #/get_locs
