#' Create hexgrid
#'
#' Combines grid creation function and hexgrid creation
#'
#' @param area_bounds an sf object (eg sfc collection) that provides the overall shape
#' @param msoas_list grouped and sorted list of MSOAs by LAD area or whatever
#' @param cell_size size of grid cells in m
#'
#' @return a list with two elements: a grid and a hexgrid
#' @export
create_hexgrid <- function(area_bounds, msoas_list, cell_size) {

  # make a grid of the whole area, given cell_size as a variable
  grid <- sf::st_make_grid(
    area_bounds,
    what = "polygons",
    square = FALSE,
    flat_topped = TRUE,
    cellsize = cell_size
  )

  # use the hex_array function to create results for each area given the full
  # grid as a starting point...
  # ... this works through each list of MSOAs iteratively and uses `reduce`
  # to produce a neat set of results
  hexes <- msoas_list %>%
    purrr::reduce(
      hex_array,
      .init = list(NULL, grid)
    ) %>%
    purrr::pluck(1)

  # return:
  list(grid, hexes)
}



#' Hex array: the guts of create_hexgrid()
#'
#' @param data_inputs a list created by previous run: previous results and a grid
#' @param msoas_list grouped and sorted list of MSOAs by LAD area or whatever
#'
#' @return a list: current results and a grid
hex_array <- function(data_inputs, msoas_list) {

  # split out data_inputs (data to work with) as the basis for the function
  prev_results <- data_inputs[[1]]
  grid <- data_inputs[[2]]

  lad_hexes <- msoas_list %>%
    split( ~ msoa11cd) %>%
    purrr::reduce(collect_hexes, .init = list(NULL, grid))

  # add results to stored previous results
  results <- dplyr::bind_rows(prev_results, lad_hexes[[1]])

  # return results so far and remaining grid:
  list(results, lad_hexes[[2]])

}


#' Collect hexes
#'
#' @param data_inputs a list created by previous run: previous results and a grid
#' @param msoa a particular MSOA whose hex location is to be calculated
#'
#' @return a list: current results and currently remaining grid
collect_hexes <- function(data_inputs, msoa) {

  # split out data_inputs (data to work with) as the basis for the function
  results <- data_inputs[[1]]
  grid <- data_inputs[[2]]

  # if we have any "results" produced already...
  # ...and at least one of the remaining grid hexes touches the results hexes
  if (!is.null(results) && any(
    sf::st_touches(
      x = sf::st_union(results),
      y = grid,
      sparse = FALSE)[1, ]
  )) {

    # pull out a group of hexes from "grid" that touch "results"...
    touching_grid <- grid %>%
      `[`(which(sf::st_touches(x = sf::st_union(results), y = grid, sparse = FALSE)[1, ]))

    touching_centroids <- touching_grid %>%
      sf::st_centroid()

    nearest_index <- sf::st_nearest_feature(msoa, touching_centroids)
    nearest_hex <- sf::st_set_geometry(msoa, `[`(touching_grid, nearest_index))

    # if this is the first loop, or there's no touching hexes available:
  } else {
    grid_centroids <- sf::st_centroid(grid)
    nearest_index <- sf::st_nearest_feature(msoa, grid_centroids)
    nearest_hex <- sf::st_set_geometry(msoa, `[`(grid, nearest_index))
  }


  # add the chosen hex to "results"
  results <- dplyr::bind_rows(results, nearest_hex)

  # and subtract it from the grid
  grid_out <- sf::st_difference(grid, nearest_hex)

  # return
  list(results, grid_out)

}
