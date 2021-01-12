
# libraries ---------------------------------------------------------------
{
  library(here)
  library(dplyr)
  library(jogger)
  library(purrr)
  library(sf)
  library(tmap)
}

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



# choose_grid <- function(areas_df, grid_dims) {
choose_grid <- function(areas_df, cell_size) {

  grid <- sf::st_make_grid(
    areas_df,
    what = "polygons",
    square = FALSE,
    flat_topped = TRUE,
    # n = grid_dims
    cellsize = cell_size
  ) %>%
    sf::st_sfc(crs = 7405) %>%
    sf::st_as_sf()

  results <- areas_df %>%
    split(forcats::fct_inorder(.$lad20nm)) %>%
    purrr::reduce(
      # gather_hexes,
      hex_array,
      .init = list(NULL, grid, NULL, NULL)
    )

  hexes <- results %>%
    purrr::pluck(1)

  union <- results %>%
    purrr::pluck(3)

  list(grid, hexes, union)
}


hex_array <- function(input, areas_df, first_pass = TRUE) {

  results <- input[[1]]
  grid <- input[[2]]
  prev_union <- input[[3]]
  prev_results <- input[[4]]

  if (first_pass) {
    prev_results <- results
    results <- NULL
  }

  msoa <- areas_df %>%
    dplyr::slice(1)
  msoa_centroid <- sf::st_centroid(msoa)
  areas_out <- areas_df %>%
    dplyr::slice(-1)
  # area_centroids <- sf::st_centroid(areas_out)


  grid_centroids <- sf::st_centroid(grid)
  nearest_index <- sf::st_nearest_feature(msoa_centroid, grid_centroids)
  nearest_hex <- dplyr::slice(grid, nearest_index)
  grid <- dplyr::slice(grid, -nearest_index)



  results <- dplyr::bind_rows(
    results,
    dplyr::bind_cols(
      sf::st_drop_geometry(msoa),
      hex_geometry = nearest_hex
    )
  )

  if (nrow(areas_out) == 0) {
    results <- dplyr::bind_rows(prev_results, sf::st_sf(results))
    union <- purrr::reduce(list(x = prev_union, sf::st_union(results)), dplyr::bind_rows)
    list(results, grid, union, NULL)
  } else {
    hex_array(list(results, grid, prev_union, prev_results), areas_out, first_pass = FALSE)
  }


}




gather_hexes <- function(input, areas_df, n = NULL) {

  results <- input[[1]]
  grid <- input[[2]]

  area_centroids <- sf::st_centroid(areas_df)

  if (is.null(n)) {

    n <- sf::st_nearest_feature(
      sf::st_centroid(sf::st_union(areas_df)),
      area_centroids
    )

  }



  msoa <- areas_df %>%
    dplyr::slice(n)
  msoa_centroid <- sf::st_centroid(msoa)
  areas_out <- areas_df %>%
    dplyr::slice(-n)
  area_centroids <- sf::st_centroid(areas_out)



  if (!is.null(results) && any(sf::st_touches(x = grid, y = results, sparse = FALSE)[, 1])) {
  filter_grid <- grid %>%
    dplyr::filter(sf::st_touches(x = ., y = results, sparse = FALSE)[, 1])
  remainder <- grid %>%
    dplyr::filter(!sf::st_touches(x = ., y = results, sparse = FALSE)[, 1])
  grid_centroids <- sf::st_centroid(filter_grid)
  nearest_index <- sf::st_nearest_feature(msoa_centroid, grid_centroids)
  nearest_hex <- dplyr::slice(filter_grid, nearest_index)
  filter_grid <- dplyr::slice(filter_grid, -nearest_index)
  grid <- dplyr::bind_rows(filter_grid, remainder)
} else {
  grid_centroids <- sf::st_centroid(grid)
  nearest_index <- sf::st_nearest_feature(msoa_centroid, grid_centroids)
  nearest_hex <- dplyr::slice(grid, nearest_index)
  grid <- dplyr::slice(grid, -nearest_index)
}


  results <- dplyr::bind_rows(
    results,
    dplyr::bind_cols(
      sf::st_drop_geometry(msoa),
      hex_geometry = nearest_hex
    )
  ) %>%
    sf::st_sf()



  next_area_index <- sf::st_nearest_feature(msoa_centroid, area_centroids)



  if (nrow(areas_out) == 0) {
    list(results, grid)
  } else {
    # gather_hexes(results, areas_df, grid, n = (n + 1))
    gather_hexes(input = list(results, grid), areas_df = areas_out, n = next_area_index)
  }
}



out <- choose_grid(sussex_bounds, cell_size = 3600)

sussex_hexmap23 <- tm_shape(out[[1]]) +
  tm_borders("olivedrab4", lwd = 1) +
  tm_fill("grey95") +
  tm_shape(out[[2]] %>%
      dplyr::mutate(msoa11no = stringr::str_extract(msoa11nm, "[0-9]{3}$")) %>%
      dplyr::mutate(msoa11hclnm = stringr::str_extract(msoa11hclnm, "^.{3}"))
    ) +
  tm_borders() +
  tm_fill("lad20nm", alpha = 1, palette = "Paired") +
  tm_text("msoa11hclnm", size = 0.5) +
  tm_shape(out[[3]]) +
  tm_borders("grey20", lwd = 2) +
  # tm_text("msoa11no", size = 0.5)
  tm_layout(title = "MSOAs in Sussex\ncell size = 3600",
            legend.show = FALSE)


sussex_hexmap23


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


