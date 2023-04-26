#' Create sorted MSOA batches
#'
#' Uses helper functions `sort_by_init_proximity()` and
#' `sort_lads_list_by_density()` to create the grouped and sorted MSOA lists
#' ready for the hex calculation process.
#'
#' @param msoa_centroids An `sf` tibble of MSOA centroids
#' @param lad_centroids A list of LAD centroids
#' @param msoas_list_orig An `sf` tibble of MSOAs (with boundary geometries)
create_sorted_msoa_batches_by_lad <- function(
    msoa_centroids,
    lad_centroids,
    msoas_list_orig) {

  # Create a list of MSOAs (with centroid geometry) for each LAD
  msoa_centroids_split <- msoa_centroids |>
    split( ~lad20nm)

  # Arrange the list of MSOAs in each LAD according to how close their centroid
  # is to the LAD's centroid. The intention here is to try to centre to some
  # extent each cluster of MSOAs more faithfully over the extent of its LAD.

  msoa_centroids_split |>

    # Create a list of distances from the LAD's overall centroid
    purrr::map2(lad_centroids, \(x, y) sf::st_distance(x, y)) |>

    # Convert the outputs from `st_distance` to vectors
    purrr::map(as.vector) |>

    # Join these vectors back to the source dfs
    purrr::map2(msoa_centroids_split,
                \(x, y) dplyr::bind_cols(y, lad_ctr_proximity = x)) |>

    # Sort each df by proximity of the MSOA centroids to the LAD centroid
    purrr::map(\(x) dplyr::arrange(x, lad_ctr_proximity)) |>

    # Use `sort_by_init_proximity()` to sort each df by proxim. to initial MSOA
    purrr::map(sort_by_init_proximity) |>

    # Return
    sort_lads_list_by_density(msoas_list_orig)

}
