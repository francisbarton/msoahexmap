#' Create sorted MSOA batches
#'
#' Uses helper functions `sort_by_init_proximity()` and
#' `sort_lads_list_by_density()` to create the grouped and sorted MSOA lists
#' ready for the hex calculation process.
#'
#' @param msoa_centroids An `sf` tibble of MSOA centroids
#' @param lad_centroids A list of LAD centroids
#' @param msoas_list_orig An `sf` tibble of MSOAs (with boundary geometries),
#'  sorted by density
create_sorted_msoa_batches <- function(
    msoa_centroids,
    lad_centroids,
    msoas_list_orig) {

  # Create a list of MSOAs (with centroid geometry) for each LAD
  msoa_centroids_split <- msoa_centroids |>
    tidyr::nest(.by = "lad22nm", .key = "value") |>
    dplyr::pull(all_of("value"), name = "lad22nm")

  lad_centroids_list <- lad_centroids |>
    dplyr::pull(all_of("geometry")) |>
    purrr::map(\(x) sf::st_sfc(x, crs = 27700))

  # Arrange the list of MSOAs in each LAD according to how close their centroid
  # is to the LAD's centroid. The intention here is to try to centre to some
  # extent each cluster of MSOAs more faithfully over the extent of its LAD.

  msoa_centroids_split |>

    # Create a list of distances from the LAD's overall centroid
    purrr::map2(lad_centroids_list,
      \(x, y) x |>
        dplyr::mutate(lad_ctr_proximity = as.vector(sf::st_distance(x, y)))) |>

    # Convert the outputs from `st_distance` to vectors
    # purrr::map(as.vector) |>

    # Join these vectors back to the source dfs
    # purrr::map2(msoa_centroids_split,
      # \(x, y) dplyr::bind_cols(y, lad_ctr_proximity = x)) |>

    # Sort each df by proximity of the MSOA centroids to the LAD centroid
    purrr::map(\(x) dplyr::arrange(x, "lad_ctr_proximity")) |>

    # Use `sort_by_init_proximity()` to sort each df by proxim. to initial MSOA
    purrr::map(sort_by_init_proximity) |>

    # Return
    sort_lads_list_by_density(msoas_list_orig)
}


#' Sort LADs list by density
#'
#' Use the "density" of MSOAs in each LAD as the way to order the hexgrid
#' creation/allocation process.
#' Density here means total LAD area divided by number of MSOAs in that LAD.
#'
#' @param msoas_list_by_proximity A named list of MSOAs already split by LAD
#'  name
#' @param msoas_list_orig An `sf` tibble of all MSOAs in area
#'
#' @returns the original list, sorted
sort_lads_list_by_density <- function(
  msoas_list_by_proximity,
  msoas_list_orig) {
  ordered_lad_names <- msoas_list_orig |>
    sf::st_drop_geometry() |>
    dplyr::summarise(across("shape_area", mean), .by = "lad22nm") |>
    dplyr::rename(density = "shape_area") |>
    # order LADs according to density...
    dplyr::arrange("density") |>
    dplyr::pull(all_of("lad22nm"))

  # Return the LSOAs list ordered by LAD density
  msoas_list_by_proximity[ordered_lad_names]
}



#' Sort MSOAs by proximity to the closest ("initial") MSOA to the LAD centroid
#'
#' @param dtf An `sf` tibble of MSOA centroids in a LAD
sort_by_init_proximity <- function(dtf) {
  top <- dtf |>
    dplyr::slice(1) |>
    dplyr::pull(all_of("geometry"))

  all_geometries <- dtf |>
    dplyr::pull(all_of("geometry"))

  distances_to_top <- sf::st_distance(all_geometries, top)

  dtf |>
    dplyr::mutate(top_proximity = distances_to_top) |>
    # dplyr::mutate(across(top_proximity, units::drop_units)) |>
    dplyr::arrange("top_proximity")
}
