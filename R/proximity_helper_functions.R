#' Sort LADs list by density
#'
#' @param msoas_list_orig sf tibble of all MSOAs in area
#' @param msoas_list_by_proximity List of MSOAs already split by LAD name
#'
#' @return
# Use the "density" of MSOAs in each LAD as the way to order the hexing process
# Density here is total LAD area divided by number of MSOAs in that LAD
sort_lads_list_by_density <- function(msoas_list_orig, msoas_list_by_proximity) {

  msoas_list_orig %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(lad20nm) %>%
    dplyr::summarise(density = sum(shape_area)/n()) %>%
    dplyr::ungroup() %>%
    # order LADs according to density...
    dplyr::arrange(density) %>%
    dplyr::pull(lad20nm) %>%
    # and then re-order this list accordingly
    `[`(msoas_list_by_proximity, .)
}



#' Sort MSOAs by proximity to the closest ("initial") MSOA to the LAD centroid
#'
#' @param dtf sf tibble of MSOA centroids in a LAD
#'
#' @return
sort_by_init_proximity <- function(dtf) {

  top <- dplyr::slice(dtf, 1)

  dists <- dtf %>%
    split( ~ msoa11cd) %>%
    purrr::map_dbl( ~ sf::st_distance(., top)) %>%
    as.vector()

  dplyr::bind_cols(dtf, top_proximity = dists) %>%
    dplyr::arrange(top_proximity)

}
