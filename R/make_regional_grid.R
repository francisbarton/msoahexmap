make_regional_grid <- function(area, cell_size) {
  sf::st_make_grid(
    area,
    what = "polygons",
    square = FALSE,
    flat_topped = TRUE,
    cellsize = cell_size
  )
}
