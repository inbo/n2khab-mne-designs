library("tidyverse")
library("DBI")
library("watina")
library("sf")

# remotes::install_github("r-dbi/RPostgres")
DROP_EXISTING <- FALSE


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

n <- nrow(obswells)
N_totaal <- as.integer(n^2 / 2 - n)
print(paste0("Computing ", n, " rows -> ", N_totaal, " calculations."))

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
  for (j in (i + 1):n) {
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



test <- count_existing()
well_pairs <- dbReadTable(watinatje_conn, "well_pairs")

# disconnect databases
dbDisconnect(watinatje_conn)
dbDisconnect(watina_conn)
