#' Make a regional grid
#'
#' @param area an sf object (eg sfc collection) that provides the overall shape
#' @param cell_size size of grid cells in m
#'
#' @return an sf grid object
make_regional_grid <- function(area, cell_size) {
  sf::st_make_grid(
    area,
    what = "polygons",
    square = FALSE,
    flat_topped = TRUE,
    cellsize = cell_size
  )
}
