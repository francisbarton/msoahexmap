
# libraries ---------------------------------------------------------------

library(here)
library(dplyr)
library(jogger)
library(purrr)
library(sf)
library(tmap)


tmap_mode("plot")

tmap_options()


# get some data to start from ---------------------------------------------

sussex_lad_bounds <- c("Brighton and Hove", "East Sussex", "West Sussex") %>%
  purrr::map_df(~ jogger::geo_get("lad", ., "utla", return_style = "simple", return_boundaries = FALSE))

sussex_bounds <- sussex_lad_bounds %>%
  dplyr::pull(2) %>%
  purrr::map_df(~ geo_get("msoa", ., "lad", spatial_ref = 7405, shape_fields = TRUE)) %>%
  dplyr::relocate(msoa11hclnm, .after = msoa11nm) %>%
  dplyr::left_join(., sussex_lad_bounds, by = c("lad20cd" = "ltla20cd", "lad20nm" = "ltla20nm")) %>%
  order_along(lad20cd, shape_area)

save_it(sussex_bounds)

sussex_bounds <- readRDS(here::here("rds_data", "sussex_bounds.Rds"))

sussex1 <- tm_shape(sussex_bounds) +
  tm_borders("grey33", lwd = 2)
sussex1
tmap_save(sussex1, "sussex1.png")


sussex_centroids <- st_centroid(sussex_bounds)

sussex2 <- tm_shape(sussex_bounds) +
  tm_borders("grey33", lwd = 2) +
  tm_shape(sussex_centroids) +
  tm_dots(col = "orange", size = 0.1)
sussex2
tmap_save(sussex2, "sussex2.png")



grid <- st_make_grid(
    sussex_bounds,
    what = "polygons",
    square = FALSE,
    flat_topped = TRUE,
    n = 36
  ) %>%
  sf::st_sfc(crs = 7405) %>%
  sf::st_as_sf()


sussex3 <- tm_shape(sussex_bounds) +
  tm_borders("grey33", lwd = 2) +
  tm_shape(grid) +
  tm_borders("green", lwd = 1) +
  tm_shape(sussex_centroids) +
  tm_dots(col = "orange", size = 0.1)
sussex3
tmap_save(sussex3, "sussex3.png")


grid_centroids <- st_centroid(grid)

sussex4 <- tm_shape(sussex_bounds) +
  tm_borders("grey20", lwd = 2) +
  tm_shape(grid) +
  tm_borders("green", lwd = 1) +
  tm_shape(sussex_centroids) +
  tm_dots(col = "orange", size = 0.1) +
  tm_shape(grid_centroids) +
  tm_dots("green")
sussex4
tmap_save(sussex4, "sussex4.png")

sussex5 <- tm_shape(sussex_bounds) +
  tm_borders("grey20", lwd = 2) +
  tm_shape(grid) +
  tm_borders("cadetblue2", lwd = 1) +
  tm_shape(sussex_centroids) +
  tm_dots(col = "orange", size = 0.1) +
  tm_shape(grid_centroids) +
  tm_dots("cadetblue2")
sussex5
tmap_save(sussex5, "sussex5.png")



choose_grid <- function(areas_df, grid_dims) {

  # areas_df <- areas_df %>%
  #   dplyr::arrange(shape_area) %>%
  #   dplyr::select(!dplyr::any_of(c("shape_area", "shape_length")))

  grid <- sf::st_make_grid(
    areas_df,
    what = "polygons",
    square = FALSE,
    flat_topped = TRUE,
    n = grid_dims
  ) %>%
    sf::st_sfc(crs = 7405) %>%
    sf::st_as_sf()

  areas_df %>%
    split(forcats::fct_inorder(.$lad20nm)) %>%
    purrr::reduce(
      gather_hexes,
      .init = list(NULL, grid)
    ) %>%
    purrr::pluck(1)

}

gather_hexes <- function(input, areas_df, n = 1) {

  results <- input[[1]]
  grid <- input[[2]]

  msoa <- areas_df %>%
    dplyr::slice(n)
  areas_df <- areas_df %>%
    dplyr::slice(-n)
  msoa_centroid <- sf::st_centroid(msoa)


  grid_centroids <- sf::st_centroid(grid)


  nearest_index <- sf::st_nearest_feature(msoa_centroid, grid_centroids)
  nearest_hex <- dplyr::slice(grid, nearest_index)

  grid <- dplyr::slice(grid, -nearest_index)

  area_centroids <- sf::st_centroid(areas_df)
  next_area_index <- sf::st_nearest_feature(msoa_centroid, area_centroids)


  results <- dplyr::bind_rows(
    results,
    dplyr::bind_cols(
      sf::st_drop_geometry(msoa),
      hex_geometry = nearest_hex
    )
  ) %>%
    sf::st_sf()

  if (nrow(areas_df) == 0) {
    list(results, grid)
  } else {

    # gather_hexes(results, areas_df, grid, n = (n + 1))
    gather_hexes(list(results, grid), areas_df, n = next_area_index)
  }
}


out <- choose_grid(sussex_bounds, 36)

hex_out <- out %>%
  dplyr::mutate(msoa11no = stringr::str_extract(msoa11nm, "[0-9]{3}$")) %>%
  dplyr::mutate(msoa11hclnm = stringr::str_extract(msoa11hclnm, "^.{3}")) %>%
  dplyr::mutate(fill_col = dplyr::case_when(
    utla20nm == "Brighton and Hove" ~ "cadetblue2",
    utla20nm == "West Sussex" ~ "darkorchid4",
    utla20nm == "East Sussex" ~ "limegreen",
    TRUE ~ "grey50"
  ))

tm_shape(grid) +
  tm_borders("olivedrab4", lwd = 1) +
  tm_shape(hex_out) +
  tm_borders("grey65") +
  tm_fill("lad20nm", alpha = 1, palette = "Set3") +
  tm_text("msoa11hclnm", size = 0.5) +
  # tm_text("msoa11no", size = 0.5)
  tm_layout(title = "MSOAs in Sussex, using hexgrid n = 36",
            legend.show = FALSE)

sussex_hexmap11 <- choose_grid(sussex_bounds, 34) %>%
  dplyr::mutate(msoa11no = stringr::str_extract(msoa11nm, "[0-9]{3}$")) %>%
  dplyr::mutate(msoa11hclnm = stringr::str_extract(msoa11hclnm, "^.{3}")) %>%
  dplyr::mutate(fill_col = dplyr::case_when(
    utla20nm == "Brighton and Hove" ~ "cadetblue2",
    utla20nm == "West Sussex" ~ "darkorchid4",
    utla20nm == "East Sussex" ~ "limegreen",
    TRUE ~ "grey50"
  )) %>%
  tm_shape() +
  tm_borders("grey65") +
  # tm_fill("grey95") +
  tm_fill("fill_col", alpha = 0.3) +
  tm_text("msoa11hclnm", size = 0.5) +
  # tm_text("msoa11no", size = 0.5)
  tm_layout(title = "MSOAs in Sussex, using hexgrid n = 34")


sussex_hexmap11


tmap_save(sussex_hexmap1, "sussex_hexmap1.png")
tmap_save(sussex_hexmap2, "sussex_hexmap2.png")
tmap_save(sussex_hexmap3, "sussex_hexmap3.png")
tmap_save(sussex_hexmap4, "sussex_hexmap4.png")
tmap_save(sussex_hexmap5, "sussex_hexmap5.png")
tmap_save(sussex_hexmap7, "sussex_hexmap7.png")
tmap_save(sussex_hexmap8, "sussex_hexmap8.png")
tmap_save(sussex_hexmap9, "sussex_hexmap9.png")
tmap_save(sussex_hexmap10, "sussex_hexmap10.png")

sussex_bounds %>%
  dplyr::mutate(msoa11hclnm = stringr::str_extract(msoa11hclnm, "^.{5}")) %>%
  tm_shape() +
  tm_borders("grey65", lwd = 1) +
  tm_text("msoa11hclnm", size = 0.5)


