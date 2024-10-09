library("DBI")
library("dplyr")
library("ggplot2")
library("sf")
library("terra")
library("mapview")

db <- file.path("./dhmv_points.db")
conn <- dbConnect(RSQLite::SQLite(), db, synchronous = NULL)

vlaanderen <- tbl(conn, "points")
glimpse(vlaanderen)

x_test <- 148600
y_test <- 208900
radius <- 10 # (+/- m)
bbox <- st_bbox(c(
  xmin = x_test - radius,
  xmax = x_test + radius,
  ymin = y_test - radius,
  ymax = y_test + radius
  ),
  crs = st_crs(31370))



bbox_xmin <- unname(bbox["xmin"])
bbox_xmax <- unname(bbox["xmax"])
bbox_ymin <- unname(bbox["ymin"])
bbox_ymax <- unname(bbox["ymax"])

hopo <- vlaanderen %>%
  filter(x >= bbox_xmin, x <= bbox_xmax,
         y >= bbox_ymin, y <= bbox_ymax
  ) %>%
  collect
glimpse(hopo)

dbDisconnect(conn)

# hopo %>%
#   mapview(
#     # map.types = c("OpenStreetMap", "OpenTopoMap"),
#     zcol = "h"
#   )

# pts <- hopo %>% select(x, y, h) %>% data.matrix
# pts
# hopo_raster <- rast(pts, type = "xyz") # via `terra`
# ggplot(hopo, aes(x, y)) +
#   geom_raster(aes(fill = h))
ggplot(hopo, aes(x, y)) +
 geom_raster(aes(fill = h), interpolate = TRUE)
# ggplot(hopo, aes(x, y)) +
#   geom_tile(aes(fill = h))

library("gstat")
hopo_sf <- st_as_sf(hopo, coords = c("x", "y"), crs = 31370)
g = gstat(formula = h ~ 1, data = hopo_sf)

grid <- hopo_sf %>% st_make_grid(cellsize = 1, square = TRUE)
elevation <- predict(g, grid)

# class(elevation)

# plot(elevation["var1.pred"])
# plot(hopo_sf, color = "black", fill = "black", add = TRUE)

elevation %>%
  ggplot() +
  geom_sf(aes(fill = var1.pred)) +
  geom_point(data = hopo, aes(x = x, y = y))
