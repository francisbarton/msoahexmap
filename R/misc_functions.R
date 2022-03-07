# #' Just a chooser for one of the 9 English regions
# #'
# #' @param regions_list tibble of 9 English regions
# #' @param n which region to choose (1-9, alphabetical order)
# #'
# #' @return string with region name
# get_current_region <- function(regions_list, n) {
#   regions_list %>%
#     dplyr::slice(n) %>%
#     dplyr::pull(rgn20nm)
# }


#' Get regional LADs
#'
#' @param region name of region
#'
#' @return sf tibble of all LADs in a region, with boundaries
get_regional_lads <- function(region) {
  lu1 <- jogger::geo_get("utla", region, "rgn", return_boundaries = FALSE)
  lu1 %>%
    dplyr::pull(utla21nm) %>%
    purrr::map_df( ~ jogger::geo_get("lad", ., "utla", spatial_ref = 7405)) %>%
    dplyr::left_join(lu1)
}


#' Get MSOAs with boundaries
#'
#' @param lads_df tibble of LADs including column of LAD names
#' @return sf tibble of MSOAs with boundaries
get_msoas <- function(lads_df) {
  lads_df %>%
    dplyr::pull(utla21cd) %>%
    unique() %>%
    purrr::map_df(~ jogger::geo_get("msoa", ., "utla", shape_fields = TRUE, spatial_ref = 7405, within_cd = TRUE)) %>%
    dplyr::relocate(c(shape_area, shape_length), .before = last_col())
}


#' Get MSOA centroids
#'
#' @param region name of region
#' @return sf tibble of MSOAs in a region with centroid geometries
get_msoa_centroids <- function(region) {
  centroids <- jogger::geo_get("msoa", region, "rgn", return_centroids = TRUE, return_style = "minimal") %>%
    sf::st_transform(crs = 7405)
  lu <- eng_msoas %>%
    purrr::reduce(dplyr::bind_rows) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(1:9)
  centroids %>%
    dplyr::left_join(lu)
}


#' Calculate hex size
#'
#' For a df of MSOAs with a `shape_area` column, return hex sizes that
#' correspond to the mean and/or median actual MSOA area from the df.
#' The number returned is fit to be used as the `cell_size` parameter in
#' `create_hexgrid()` (which is then passed to the `cellsize` parameter in
#' `sf::st_make_grid`). This figure relates to the *distance across* (from top
#' to bottom with flat-topped hexagons) the grid hexagons generated.
#'
#' @param msoas_df A data frame with each row being an MSOA. Must contain a
#'   `shape_area` column.
#' @param type whether to return the cellsize figure relating to the mean MSOA
#'   size or the median, or both. Defaults to both.
#' @param efficiency The efficiency level to aim for, when calculating
#'   hex size
#'
#' @return a named integer vector of length 1 or 2.
calculate_hex_size <- function(msoas_df, type = c("mean", "median"), efficiency = 40) {

  # convert MSOA actual area to a length for the distance across a grid hexagon
  # based on the formula for the area of a hexagon
  convert_to_hex <- function(x) {
    x %>%
      `*`(., 2) %>%
      `/`(., 3^0.5) %>%
      `^`(., 0.5) %>%
      round()
  }

  mean_hex_size <- msoas_df %>%
    dplyr::pull(shape_area) %>%
    mean() %>%
    convert_to_hex()

  median_hex_size <- msoas_df %>%
    dplyr::pull(shape_area) %>%
    stats::median() %>%
    convert_to_hex()

  efficiency_hex_size <- msoas_df %>%
    dplyr::pull(shape_area) %>%
    mean() %>%
    `*`(., efficiency/100) %>%
    convert_to_hex()


  if (!"mean" %in% type) {
    mean_hex_size <- NULL
  }
  if (!"median" %in% type) {
    median_hex_size <- NULL
  }

  c(mean_hex_size = mean_hex_size, median_hex_size = median_hex_size, efficiency = efficiency, efficiency_hex_size = efficiency_hex_size)

}
