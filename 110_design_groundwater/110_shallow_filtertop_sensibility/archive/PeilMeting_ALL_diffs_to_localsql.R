library("tidyverse")
library("DBI")
library("watina")
library("sf")

# remotes::install_github("r-dbi/RPostgres")
DROP_EXISTING <- TRUE


######### Connections #########
watinatje_conn <- dbConnect(
  RPostgres::Postgres(),
  dbname = "watinatje",
  host = "192.168.247.4",
  port = "2408",
  password = "sesam",
  user = "falk",
)

watina_conn <- connect_watina()

if (DROP_EXISTING) {
  dbRemoveTable(watinatje_conn, "difflevels")
}

######### Helper Functions #########
query <- function(query_string) {
  rs <- dbGetQuery(watina_conn, query_string)
  return(rs)
}


check_exists <- function(focal_loc, reference_loc) {
  query_string <- paste0(
    " SELECT COUNT(DISTINCT t) ",
    " FROM difflevels AS dl ",
    " WHERE (i1 = ", focal_loc,
    "   AND i2 = ", reference_loc,
    "   ) OR (i1 = ", reference_loc,
    "   AND i2 = ", focal_loc,
    "   ) ",
    " GROUP BY i1, i2 ",
    " ;"
  )
  rs <- dbGetQuery(watinatje_conn, query_string)
  return(nrow(rs) > 0)
}


query_cluster <- function(focal_loc, reference_loc) {
  query_string <- paste0(
    " SELECT obs1.t, ",
    "        i1, ",
    "        i2, ",
    "        l1, ",
    "        l2 ",
    " FROM (",
    "     SELECT MeetpuntWID as i1, TijdWID AS t, Niveau AS l1 ",
    "     FROM FactPeilMeting ",
    "     WHERE MeetpuntWID = ", focal_loc,
    " ) obs1 ",
    " INNER JOIN (",
    "     SELECT MeetpuntWID as i2, TijdWID AS t, Niveau AS l2 ",
    "     FROM FactPeilMeting",
    "     WHERE MeetpuntWID = ", reference_loc,
    " ) obs2 ",
    "  ON (obs1.t = obs2.t) ",
    " ;"
  )
  query(query_string)
}


transfer_waterlevels <- function(loc, ref) {
  if (!check_exists(loc, ref)) {
    diff_data <- query_cluster(loc, ref)
    if (nrow(diff_data) != 0) {
      dbAppendTable(watinatje_conn, "difflevels", diff_data)
    }
    return(diff_data)
  }
}


count_existing <- function() {
  query_string <- paste0(
    " SELECT i1, i2, COUNT(DISTINCT t) ",
    " FROM difflevels AS dl ",
    " GROUP BY i1, i2 ",
    " ;"
  )
  rs <- dbGetQuery(watinatje_conn, query_string)
  return(rs)
}


try_correlation <- function(l1, l2) {
  if (length(l1) < 10) {
    return(invisible(NA))
  }
  return(cor.test(l1, l2)$estimate)
}

######### Procedure #########

obswells <- get_locs(
  watina_conn,
  filterdepth_range = c(0, 5),
  filterdepth_guess = FALSE,
  loc_type = "P",
  loc_validity = c("VLD", "ENT"),
  obswells = FALSE
) %>%
  filter(!if_any(c(x, y), is.na)) %>%
  select(loc_wid, x, y, filterdepth, filterlength) %>%
  mutate(filtertop_depth = filterdepth - filterlength / 2) %>%
  collect()

if (DROP_EXISTING && dbExistsTable(watinatje_conn, "obswells")) {
  dbRemoveTable(watinatje_conn, "obswells")
}
dbWriteTable(watinatje_conn, "obswells", obswells)

# glimpse(obswells)
obswells_coords <- obswells %>%
  as_points()
get_distance <- function(i, j) {
  distance_m <- obswells_coords[c(i, j), ] %>%
    st_distance()
  return(as.numeric(distance_m[1, 2]))
}

n = nrow(obswells)
N_totaal = as.integer(n^2 /2 -n)
print(paste0("Computing ", n, " rows -> ", N_totaal, " iterations." ))

progress <- utils::txtProgressBar(min = 0, max = N_totaal, style = 3)


loc_wids <- obswells %>% select(loc_wid)


# prepare difflevels
if (DROP_EXISTING && dbExistsTable(watinatje_conn, "difflevels")) {
  dbRemoveTable(watinatje_conn, "difflevels")
}
difflevels <- tibble(
  t  = integer(),
  i1 = integer(),
  i2 = integer(),
  l1 = numeric(),
  l2 = numeric()
)
dbWriteTable(watinatje_conn, "difflevels", difflevels)

# prepare well_pairs
if (DROP_EXISTING && dbExistsTable(watinatje_conn, "well_pairs")) {
  dbRemoveTable(watinatje_conn, "well_pairs")
}
well_pairs <- tibble(
  l1 = integer(),
  l2 = integer(),
  distance_m = numeric(),
  d_fd = numeric(),
  d_ftd = numeric(),
  corr = numeric(),
  n = integer()
)
dbWriteTable(watinatje_conn, "well_pairs", well_pairs)

counter <- 0
for (i in 1:n) {
    for (j in (i+1):n) {
        # print progress
        counter <- counter + 1
        utils::setTxtProgressBar(progress, counter)

        # skip same
        if (i == j) {
            next
        }

        # distance criterium
        distance_m <- get_distance(i, j)
        if (distance_m > 1000) {
            next
        }


        i1 <- loc_wids[i,][[1]]
        i2 <- loc_wids[j,][[1]]

        # transfer level measurements
        diff_data <- transfer_waterlevels(i1, i2)

        if (nrow(diff_data) == 0) {
            next
        }

        # store computed info
        well_pairs <- tibble(
            i1 = i1,
            i2 = i2,
            distance_m = distance_m,
            d_fd = obswells[j,"filterdepth"][[1]] -
                   obswells[i,"filterdepth"][[1]],
            d_ftd = obswells[j,"filtertop_depth"][[1]] -
                   obswells[i,"filtertop_depth"][[1]],
            corr = try_correlation(diff_data$l1, diff_data$l2),
            n = nrow(diff_data)
        )
        dbAppendTable(watinatje_conn, "well_pairs", well_pairs)

    # skip same
    if (i == j) {
      next
    }

    # distance criterium
    distance_m <- get_distance(i, j)
    if (distance_m > 1000) {
      next
    }


    l1 <- loc_wids[i, ][[1]]
    l2 <- loc_wids[j, ][[1]]

    # transfer level measurements
    diff_data <- transfer_waterlevels(l1, l2)

    if (nrow(diff_data) != 0) {
      next
    }

    # store computed info
    well_pairs <- tibble(
      l1 = l1,
      l2 = l2,
      distance_m = distance_m,
      d_fd = obswells[j, "filterdepth"][[1]] -
        obswells[i, "filterdepth"][[1]],
      d_ftd = obswells[j, "filtertop_depth"][[1]] -
        obswells[i, "filtertop_depth"][[1]],
      corr = try_correlation(diff_data$l1, diff_data$l2),
      n = nrow(diff_data)
    )
    dbAppendTable(watinatje_conn, "well_pairs", well_pairs)
  }
}


# on-the-fly inspection
if (FALSE) {
    # test <- count_existing()
    well_pairs <- dbReadTable(watinatje_conn, "well_pairs")
    well_pairs %>%
        ggplot(aes(x=distance_m)) +
        geom_histogram(bins = 128)
    well_pairs %>%
        ggplot(aes(x=corr)) +
        geom_histogram(bins = 128)
    # unique(well_pairs$l1)
    # difflevels <- dbReadTable(watinatje_conn, "difflevels")
    # difflevels %>%
    #     group_by(t) %>%
    #     count() %>%
    #     arrange(desc(n))
    # difflevels %>%
    #     mutate(dl = abs(l2-l1)) %>%
    #     ggplot(aes(x=dl)) +
    #     geom_histogram(bins = 128)

    # started 20240920 ~21:00
}

# disconnect databases
dbDisconnect(watinatje_conn)
dbDisconnect(watina_conn)

# [1] "Computing 7566 rows -> 28614612 calculations."
# |                                                                      |   0%
# |================                                                      |  23%Error in `.rs.sourceWithProgress()`:
#   ! ODBC failed with error IMC01 from [Microsoft][ODBC Driver 13 for SQL
#                                                   Server].
# ✖ Communication link failure
# • The connection is broken and recovery is not possible. The client driver
# attempted to recover the connection one or more times and all attempts
# failed. Increase the value of ConnectRetryCount to increase the number of
# recovery attempts.
# • <SQL> ' SELECT obs1.t, i1, i2, l1, l2 FROM ( SELECT MeetpuntWID as i1,
#   TijdWID AS t, Niveau AS l1 FROM FactPeilMeting WHERE MeetpuntWID = 1499 )
#   obs1 INNER JOIN ( SELECT MeetpuntWID as i2, TijdWID AS t, Niveau AS l2 FROM
#   FactPeilMeting WHERE MeetpuntWID = 8121 ) obs2 ON (obs1.t = obs2.t) ;'
# ℹ From nanodbc/nanodbc.cpp:1722.
# Backtrace:
#   ▆
# 1. ├─.rs.sourceWithProgress(...)
# 2. │ └─base::eval(statements[[idx]], envir = globalenv()) at R/modules/SourceWithProgress.R:82:7
# 3. │   └─base::eval(statements[[idx]], envir = globalenv())
# 4. ├─global transfer_waterlevels(l1, l2)
# 5. │ └─global query_cluster(loc, ref) at 110_design_groundwater/110_shallow_filtertop_sensibility/PeilMeting_ALL_diffs_to_localsql.R:76:9
# 6. │   └─global query(query_string) at 110_design_groundwater/110_shallow_filtertop_sensibility/PeilMeting_ALL_diffs_to_localsql.R:70:5
# 7. │     ├─DBI::dbGetQuery(watina_conn, query_string) at 110_design_groundwater/110_shallow_filtertop_sensibility/PeilMeting_ALL_diffs_to_localsql.R:28:5
# 8. │     └─odbc::dbGetQuery(watina_conn, query_string)
# 9. │       └─odbc (local) .local(conn, statement, ...)
# 10. │         ├─DBI::dbSendQuery(...)
# 11. │         └─odbc::dbSendQuery(...)
# 12. │           └─odbc (local) .local(conn, statement, ...)
# 13. │             └─odbc:::OdbcResult(...)
# 14. │               └─odbc:::new_result(p = connection@ptr, sql = statement, immediate = immediate)
# 15. └─odbc (local) `<fn>`("nanodbc/nanodbc.cpp:1722: IMC01\n[Microsoft][ODBC Driver 13 for SQL Server]Communication link failure \n[Microsoft][ODBC Driver 13 for SQL Server]The connection is broken and recovery is not possible. The client driver attempted to recover the connection one or more times and all attempts failed. Increase the value of ConnectRetryCount to increase the number of recovery attempts. \n<SQL> ' SELECT obs1.t,         i1,         i2,         l1,         l2  FROM (     SELECT MeetpuntWID as i1, TijdWID AS t, Niveau AS l1      FROM FactPeilMeting      WHERE MeetpuntWID = 1499 ) obs1  INNER JOIN (     SELECT MeetpuntWID as i2, TijdWID AS t, Niveau AS l2      FROM FactPeilMeting     WHERE MeetpuntWID = 8121 ) obs2   ON (obs1.t = obs2.t)  ;'")
# 16.   └─cli::cli_abort(...)
# 17.     └─rlang::abort(...)
# Warning message:
#   In warn_xy_duplicates(get(xvar, .), get(yvar, .)) :
#   344 different coordinate pairs occur more than once.
#
# Execution halted
