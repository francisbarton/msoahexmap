
# libraries ---------------------------------------------------------------

library(dplyr)
library(jogger)
library(purrr)
library(sf)
library(tmap)


tmap_mode("plot")

# get some data to start from ---------------------------------------------


sussex_centroids <- bind_rows(

  geo_get("msoa", "West Sussex", "utla", return_style = "minimal", return_centroids = TRUE, spatial_ref = 7405),
  geo_get("msoa", "East Sussex", "utla", return_style = "minimal", return_centroids = TRUE, spatial_ref = 7405),
  geo_get("msoa", "Brighton and Hove", "lad", return_style = "minimal", return_centroids = TRUE, spatial_ref = 7405)

) %>%
  st_transform(crs = 7405)


sussex_bounds <- bind_rows(

  geo_get("msoa", "West Sussex", "utla", return_style = "minimal", spatial_ref = 7405),
  geo_get("msoa", "East Sussex", "utla", return_style = "minimal", spatial_ref = 7405),
  geo_get("msoa", "Brighton and Hove", "lad", return_style = "minimal", spatial_ref = 7405)

)

tm_shape(sussex_bounds) +
  tm_borders("grey20", lwd = 2) +
  tm_shape(sussex_centroids) +
  tm_dots(col = "red")


grid1 <- st_make_grid(sussex_bounds, what = "polygons", square = FALSE, flat_topped = TRUE, n = c(36, 30))

grid1_centroids <- st_centroid(grid1)

tm_shape(sussex_bounds) +
  tm_borders("grey20", lwd = 2) +
  tm_shape(grid1) +
  tm_borders("green", lwd = 1) +
  tm_shape(sussex_centroids) +
  tm_dots(col = "orange") +
  tm_shape(grid1_centroids) +
  tm_dots("red")

nearest_points <- st_nearest_feature(sussex_centroids, grid1_centroids) %>%
  map(~ `[[`(grid1_centroids, .x)) %>%
  st_as_sfc() %>%
  st_set_crs(7405)

just_centroid_points <- st_geometry(sussex_centroids)

st_distance(just_centroid_points, nearest_points, by_element = TRUE)
