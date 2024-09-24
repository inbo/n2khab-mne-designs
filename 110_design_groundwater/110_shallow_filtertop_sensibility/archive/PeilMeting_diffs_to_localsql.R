library("DBI")
library("watina")

# remotes::install_github("r-dbi/RPostgres")

######### Connections #########
watinatje_conn <- dbConnect(
  RPostgres::Postgres(),
  dbname = "watinatje",
  host = "143.169.13.164",
  port = "2408",
  password = "sesam",
  user = "falk",
)

watina_conn <- connect_watina()


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


transfer_cluster <- function(loc, ref) {
  if (!check_exists(loc, ref)) {
    diff_data <- query_cluster(loc, ref)
    dbAppendTable(watinatje_conn, "difflevels", diff_data)
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

######### Procedure #########
cluster_metadata <- readRDS(file = "cluster_metadata.RDS")
cluster_pairs <- cluster_metadata %>%
  filter(!(loc_wid == deep_ref)) %>%
  select(loc_wid, deep_ref)

for (i in 1:nrow(cluster_pairs)) {
  if (i %% 25 == 0) {
    print(paste0(i, "/", nrow(cluster_pairs)))
  }
  transfer_cluster(cluster_pairs[i, 1], cluster_pairs[i, 2])
}



if (FALSE) {
  test <- query_cluster(679, 678)
  check_exists(679, 678)
  check_exists(678, 679)
  dbWriteTable(watinatje_conn, "difflevels", test)
  dbAppendTable(watinatje_conn, "difflevels", test)
  test2 <- dbReadTable(watinatje_conn, "difflevels")
}


test <- count_existing()

# disconnect databases
dbDisconnect(watinatje_conn)
dbDisconnect(watina_conn)
