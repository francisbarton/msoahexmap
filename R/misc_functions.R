#' Just a chooser for one of the 9 English regions
#'
#' @param regions_list tibble of 9 English regions
#' @param n which region to choose (1-9, alphabetical order)
#'
#' @return string with region name
get_current_region <- function(regions_list, n) {
  regions_list %>%
    dplyr::slice(n) %>%
    dplyr::pull(rgn20nm)
}


#' Get regional LADs
#'
#' @param region name of region
#'
#' @return sf tibble of all LADs in a region, with boundaries
get_regional_lads <- function(region) {
  jogger::geo_get("lad", region, "rgn", return_style = "minimal", spatial_ref = 7405)
}


#' Get MSOAs with boundaries
#'
#' @param lads_list tibble of LADs including column of LAD names (strings)
#'
#' @return sf tibble of MSOAs with boundaries
get_msoas <- function(lads_list) {
  lads_list %>%
    dplyr::pull(lad20nm) %>%
    purrr::map_df(~ geo_get("msoa", ., "lad", shape_fields = TRUE, spatial_ref = 7405)) %>%
    dplyr::relocate(c(shape_area, shape_length), .before = last_col())
}


#' Get MSOA centroids
#'
#' @param region name of region
#'
#' @return sf tibble of MSOAs in a region with centroid geometries
get_msoa_centroids <- function(region) {
  jogger::geo_get("msoa", region, "rgn", return_centroids = TRUE, return_style = "minimal", spatial_ref = 7405)
}
