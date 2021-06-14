create_sorted_msoa_batches_by_lad <- function(msoa_centroids, lad_centroids, msoas_list_orig) {

  source(here("R/proximity_helper_functions.R"))

  # Create a list of MSOAs (with centroid geometry) for each LAD
  msoa_centroids_split <- msoa_centroids %>%
    split( ~ lad20nm) # R 4.1.0 style!

  # Arrange the list of MSOAs in each LAD according to how close their centroid
  # is to the LAD's centroid. The intention here is to try to centre to some
  # extent each cluster of MSOAs more faithfully over the extent of its LAD

  msoa_centroids_split %>%

    # create a list of distances from the LAD's overall centroid...
    purrr::map2(., lad_centroids, ~ sf::st_distance(.x, .y)) %>%

    # convert the outputs from st_distance to vectors...
    purrr::map(as.vector) %>%

    # join these vectors back to the source dfs...
    purrr::map2(., msoa_centroids_split, ~ dplyr::bind_cols(.y, lad_ctr_proximity = .x)) %>%

    # and then sort each df by proximity of the MSOAs
    purrr::map(~ arrange(., lad_ctr_proximity)) %>%

    # use sort_by_init_proximity() to
    purrr::map(sort_by_init_proximity) %>%

    # return:
    sort_lads_list_by_density(msoas_list_orig, .)

}
