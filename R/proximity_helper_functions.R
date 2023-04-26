#' Sort LADs list by density
#'
#' Use the "density" of MSOAs in each LAD as the way to order the hexgrid
#' creation/allocation process.
#' Density here means total LAD area divided by number of MSOAs in that LAD.
#'
#' @param msoas_list_by_proximity A named list of MSOAs already split by LAD name
#' @param msoas_list_orig An `sf` tibble of all MSOAs in area
#'
#' @returns the original list sorted
sort_lads_list_by_density <- function(msoas_list_by_proximity, msoas_list_orig) {

  ordered_lad_names <- msoas_list_orig |>
    sf::st_drop_geometry() |>
    dplyr::summarise(density = sum(shape_area)/n(), .by = "lad20nm") |>
    # order LADs according to density...
    dplyr::arrange(density) |>
    dplyr::pull(lad20nm)

  # Return the LSOAs list ordered by LAD density
  msoas_list_by_proximity[ordered_lad_names]
}



#' Sort MSOAs by proximity to the closest ("initial") MSOA to the LAD centroid
#'
#' @param dtf An `sf` tibble of MSOA centroids in a LAD
#'
#' @return
sort_by_init_proximity <- function(dtf) {

  top <- dplyr::slice(dtf, 1)

  # poss nicer version than below, but wait until I can test it:
  # dtf |>
  #   dplyr::rowwise() |>
  #   dplyr::mutate(top_proximity = sf::st_distance(geometry, top)) |>
  #   dplyr::ungroup() |>
  #   # dplyr::mutate(across(top_proximity, units::drop_units)) |>
  #   dplyr::arrange(top_proximity)


  dists <- dtf |>
    split( ~ msoa11cd) |>
    purrr::map_dbl(\(x) sf::st_distance(x, top)) |>
    as.vector()

  dtf |>
    dplyr::bind_cols(top_proximity = dists) |>
    dplyr::arrange(top_proximity)

}
