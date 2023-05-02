#' Make a regional grid
#'
#' @param area An `sf` object (eg sfc collection) that provides the overall
#'  shape
#' @param cell_size numeric. The size of grid cells in metres.
#'
#' @returns An `sf` grid object
make_regional_grid <- function(area, cell_size) {
  sf::st_make_grid(
    area,
    what = "polygons",
    square = FALSE,
    flat_topped = TRUE,
    cellsize = cell_size
  )
}
