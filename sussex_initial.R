
tmap_mode("plot")

tmap_options()




# get some data to start from ---------------------------------------------


sussex_lads <- c("Brighton and Hove", "East Sussex", "West Sussex") %>%
  purrr::map_df(~ jogger::geo_get("lad", ., "utla", return_style = "simple", spatial_ref = 27700)) %>%
  sf::st_as_sf() # shouldn't need this - possibly a (now fixed) jogger bug

save_it(sussex_lads)

sussex_bounds <- sussex_lads %>%
  dplyr::pull(2) %>%
  purrr::map(~ geo_get("msoa", ., "lad", spatial_ref = 27700, shape_fields = TRUE)) %>%
  purrr::reduce(bind_rows) %>%
  dplyr::left_join(., sf::st_drop_geometry(sussex_lads), by = c("lad20cd" = "ltla20cd", "lad20nm" = "ltla20nm")) %>%
  dplyr::relocate(c(shape_area, shape_length, geometry), .after = last_col()) %>%
  sf::st_as_sf()
  # # not needed now - was previous way of sorting areas by smallest MSOA area
  # %>%
  # order_along(lad20cd, shape_area)

save_it(sussex_bounds)

sussex_bounds <- readRDS(here::here("rds_data", "sussex_bounds.Rds"))


sussex1 <- tm_shape(sussex_bounds) +
  tm_borders("grey33", lwd = 2)
sussex1
tmap_save(sussex1, "sussex1.png")


# #### sussex_centroids <- st_centroid(sussex_bounds)
# instead of geometric centroids (above), use ONS population-weighted centroids
sussex_centroids <- sussex_bounds %>%
  dplyr::select(starts_with(c("msoa", "lad"))) %>%
  dplyr::mutate(geometry = jogger::geo_get_bounds("msoa11cd", msoa11cd, spatial_ref = 27700, return_centroids = TRUE))

save_it(sussex_centroids)

# sort each lad group by proximity to combined lad geo centroid

sussex_lads_split <- sussex_bounds %>%
  split(forcats::fct_inorder(.$lad20nm))
sussex_centroids_split <- sussex_centroids %>%
  split(forcats::fct_relevel(forcats::fct_inorder(.$lad20nm), names(sussex_lads_split)))

sussex_lad_centroids <- sussex_lads_split %>%
  purrr::map(sf::st_union) %>%
  purrr::map(sf::st_centroid)
  # purrr::map(sf::st_as_sf)


# make a map why not -------------------------

grid <- st_make_grid(
  sussex_bounds,
  what = "polygons",
  square = FALSE,
  flat_topped = TRUE,
  cellsize = 3750
) %>%
  sf::st_sfc(crs = 27700)

grid_centroids <- st_centroid(grid)




sussex3 <- tm_shape(sussex_bounds) +
  tm_borders("grey33", lwd = 2) +
  tm_shape(grid) +
  tm_borders("green", lwd = 1) +
  tm_shape(sussex_centroids) +
  tm_dots(col = "orange", size = 0.1)
sussex3
tmap_save(sussex3, "sussex3.png")



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




sussex6 <- tm_shape(grid) +
  tm_borders("olivedrab4", lwd = 2, alpha = 0.5) +
  tm_fill("grey85", alpha = 0.3) +
  tm_shape(sussex_lads) +
  tm_borders("grey45", lwd = 2) +
  tm_fill("ltla20nm", alpha = 0.8, palette = my_pal(13)) +
  # tm_shape(sussex_bounds) +
  # tm_borders("grey75", lwd = 1) +
  tm_shape(sussex_centroids) +
  tm_dots(col = "white", size = 0.2) +
  tm_shape(sussex_centroids) +
  tm_dots(col = "tomato2", size = 0.1) +
  tm_layout(legend.outside = TRUE)
sussex6
tmap_save(sussex6, "sussex6.png")


sussex5 <- tm_shape(sussex_bounds) +
  tm_borders("grey75", lwd = 1) +
  tm_shape(sussex_lads) +
  tm_borders("grey45", lwd = 2) +
  tm_shape(sussex_centroids) +
  tm_dots(col = "orange", size = 0.1) +
  tm_shape(purrr::map_dfr(sussex_lad_centroids, sf::st_sf)) +
  tm_dots(col = "firebrick", size = 0.2)

sussex5
tmap_save(sussex5, "sussex5.png")


# ---



# Arrange the list of MSOAs in each LAD according to how close their centroid
# is to the LAD's centroid. The intention here is to try to centre to some
# extent each cluster of MSOAs more faithfully over the extent of its LAD

sussex_lads_by_centroid_proximity <- sussex_centroids_split %>%
  purrr::map2(., sussex_lad_centroids, ~ sf::st_distance(.x, .y)) %>%
  purrr::map(as.vector) %>%
  purrr::map2(., sussex_centroids_split, ~ dplyr::bind_cols(.y, proximity = .x)) %>%
  purrr::map(~ arrange(., proximity))


# Use the "density" of MSOAs in each LAD as the way to order the hexing process.
# Density here is total LAD area divided by number of MSOAs in that LAD
sussex_lads_by_centroid_proximity <- sussex_lads_split %>%
  reduce(bind_rows) %>%
  st_drop_geometry() %>%
  group_by(lad20nm) %>%
  summarise(density = sum(shape_area)/n()) %>%
  ungroup() %>%
  arrange(density) %>%
  pull(lad20nm) %>%
  `[`(sussex_lads_by_centroid_proximity, .)

# order LADs by fewest MSOAs only
# sussex_lads_by_centroid_proximity2 <- sussex_lads_split %>%
#   map_int(nrow) %>%
#   sort() %>%
#   names() %>%
#   `[`(sussex_lads_by_centroid_proximity, .)






# FUNCTIONS TIME! ---------------------------------------------------------





choose_grid <- function(area_bounds, msoas_list, cell_size) {

  grid <- sf::st_make_grid(
    area_bounds,
    what = "polygons",
    square = FALSE,
    flat_topped = TRUE,
    # n = grid_dims
    cellsize = cell_size
  ) %>%
    sf::st_sfc(crs = 27700)

  # results <- areas_df %>%
    # split(forcats::fct_inorder(.$lad20nm)) %>%
  results <- msoas_list %>%
    purrr::reduce(
      # gather_hexes,
      # hex_array,
      hex_array2,
      .init = list(NULL, grid)
    )

  hexes <- results %>%
    purrr::pluck(1)

  list(grid, hexes)
}


hex_array2 <- function(input, msoas_sf, first_pass = TRUE) {

  results <- input[[1]]
  grid <- input[[2]]

  if (first_pass) {
    prev_results <- results
    results <- NULL
  } else {
    prev_results <- input[[3]]
  }



  msoa <- msoas_sf %>%
    dplyr::slice(1)
  # msoa_centroid <- sussex_centroids %>%
  #   dplyr::filter(msoa11cd == msoa$msoa11cd)
  msoas_out <- msoas_sf %>%
    dplyr::slice(-1)




  if (!is.null(results) && any(
      sf::st_touches(
        x = results,
        y = grid,
        sparse = FALSE)[1, ]
      # sf::st_touches(x = grid, y = results, sparse = FALSE)[, 1]
    )) {
      filter_grid <- grid %>%
        `[`(which(sf::st_touches(x = results, y = grid, sparse = FALSE)[1, ]))
      remainder <- grid %>%
        `[`(which(!sf::st_touches(x = results, y = grid, sparse = FALSE)[1, ]))

      grid_centroids <- sf::st_centroid(filter_grid)
      nearest_index <- sf::st_nearest_feature(msoa, grid_centroids)
      nearest_hex <- `[`(filter_grid, nearest_index)
      filter_grid <- `[`(filter_grid, -nearest_index)
      grid_out <- c(filter_grid, remainder)
    } else {
      grid_centroids <- sf::st_centroid(grid)
      nearest_index <- sf::st_nearest_feature(msoa, grid_centroids)
      nearest_hex <- `[`(grid, nearest_index)
      grid_out <- `[`(grid, -nearest_index)
  }




  results <- dplyr::bind_rows(
    results,
    sf::st_set_geometry(msoa, nearest_hex)
  )

  if (nrow(msoas_out) == 0) {
    results <- dplyr::bind_rows(prev_results, results)
    list(results, grid_out)
  } else {
    hex_array2(list(results, grid_out, prev_results), msoas_out, first_pass = FALSE)
  }


}






hex_array <- function(input, areas_df, first_pass = TRUE) {

  results <- input[[1]]
  grid <- input[[2]]
  prev_results <- input[[3]]

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
    list(results, grid, NULL)
  } else {
    hex_array(list(results, grid, prev_results), areas_out, first_pass = FALSE)
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





my_pal <- ggsci::pal_futurama()

my_pal <- ggsci::pal_d3("category20")

tmap_design_mode()

out <- choose_grid(sussex_bounds, sussex_lads_by_centroid_proximity, cell_size = 4000)

sussex_hexmap <- tm_shape(out[[1]]) +
  tm_borders("olivedrab4", lwd = 1, alpha = 0.1) +
  tm_fill("grey65", alpha = 0.3) +
  tm_shape(sf::st_union(sussex_bounds)) +
  tm_borders("grey45") +
  tm_shape(out[[2]] %>%
             dplyr::mutate(
               label = toupper(
                 paste0(
                   stringr::str_extract(lad20nm, "^.{2,4}"),
                   ".\n",
                   stringr::str_extract(msoa11hclnm, "^.{2,5}"),
                   "\n",
                   stringr::str_extract(msoa11nm, "[0-9]{3}$")
                 )
               )
             )
  ) +
  tm_borders() +
  tm_fill("lad20nm", alpha = 0.7, palette = my_pal(12)) +
  tm_text("label", size = 0.6) +
  # tm_text("msoa11no", size = 0.5) +
  # tm_shape(out[[3]]) +
  # tm_borders("grey20", lwd = 2) +
  tm_layout(title = paste0(
    "MSOAs in Sussex"
    , nrow(out[[2]])
    , "\n"
    , "Cells (hexagons): "
    , nrow(out[[1]])
    , "Cell size = 4000m\n"
    )
    , legend.show = FALSE)



sussex_bounds %>%
  dplyr::mutate(msoa11hclnm = stringr::str_extract(msoa11hclnm, "^.{5}")) %>%
  tm_shape() +
  tm_borders("grey65", lwd = 1) +
  tm_text("msoa11hclnm", size = 0.5)


