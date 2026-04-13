# Exploring the degree to which bog-mire-fen (BMF typeclass) polygons from
# habitatmap_terr overlap with watersurfaces

library(dplyr)
library(forcats)
library(stringr)
library(sf)
library(units)
library(n2khab)
library(ggplot2)

hmt <- read_habitatmap_terr()
ws <- read_watersurfaces()

# BMF occurrences in habitatmap_terr
hmt_occ_bmf <-
  hmt$habitatmap_terr_types %>%
  semi_join(
    read_types() %>% filter(typeclass == "BMF"),
    join_by(type)
  )

# corresponding polygons in habitatmap_terr
hmt_pol_bmf <-
  hmt$habitatmap_terr_polygons %>%
  semi_join(hmt_occ_bmf, join_by(polygon_id))

# subset of polygons that overlaps with watersurfaces
hmt_pol_bmf_ws <-
  hmt_pol_bmf %>%
  st_filter(ws, .predicate = st_overlaps) %>%
  mutate(has_watersurface = TRUE)

# calculate the relative (optionally absolute) areas per type in habitatmap
# polygons that overlap a watersurface versus those that don't, and plot it
rbind(
  hmt_pol_bmf_ws,
  # add the complement:
  hmt_pol_bmf %>%
    anti_join(
      hmt_pol_bmf_ws %>% st_drop_geometry(),
      join_by(polygon_id)
    ) %>%
    mutate(has_watersurface = FALSE)
) %>%
  # add polygon area:
  mutate(pol_area = st_area(.) %>% set_units("ha")) %>%
  st_drop_geometry() %>%
  select(polygon_id, pol_area, has_watersurface) %>%
  inner_join(
    hmt_occ_bmf %>%
      select(polygon_id, type, phab),
    join_by(polygon_id),
    relationship = "one-to-many",
    unmatched = "error"
  ) %>%
  # add type area:
  mutate(area = pol_area * phab / 100) %>%
  # optionally turn off this filter to include rbb types:
  filter(!str_detect(type, "rbb")) %>%
  # sum the type areas:
  summarize(
    area = sum(area),
    .by = c(type, has_watersurface)
  ) %>%
  mutate(type = fct_rev(type)) %>%
  ggplot(aes(x = type, y = area, fill = has_watersurface)) +
  # set position = "stack" (the default) to get absolute values:
  geom_col(position = "fill") +
  coord_flip()


# following code (calculating the actual intersections) took way too much RAM; I
# considered it not worth the trouble to optimize this
if (FALSE) {
  hmt_pol_bmf %>%
    st_intersection(ws) %>%
    mutate(is_watersurface = TRUE) %>%
    rbind(
      hmt_pol_bmf %>%
        st_difference(ws) %>%
        mutate(is_watersurface = FALSE)
    ) %>%
    mutate(pol_area = st_area(.) %>% drop_units()) %>%
    st_drop_geometry() %>%
    select(polygon_id, pol_area, is_watersurface) %>%
    inner_join(
      hmt_occ_bmf %>%
        select(polygon_id, type, phab),
      join_by(polygon_id),
      relationship = "one-to-many",
      unmatched = "error"
    ) %>%
    mutate(area = pol_area * phab / 100) %>%
    summarize(
      area = sum(area),
      .by = c(type, is_watersurface)
    ) %>%
    mutate(type = fct_rev(type)) %>%
    ggplot(aes(x = type, y = area, fill = is_watersurface)) +
    # set position = "stack" (the default) to get absolute values
    geom_col(position = "fill") +
    coord_flip()
}

