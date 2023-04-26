#' Just a chooser for one of the 9 English regions
#'
#' @param regions_list A tibble of 9 English regions
#' @param n numeric. Which region to choose (1-9, alphabetical order)
#'
#' @returns The region name as a string
get_current_region <- function(regions_list, n) {
  regions_list |>
    dplyr::slice(n) |>
    dplyr::pull(rgn20nm)
}


#' Get regional LADs
#'
#' @param region string. The name of region
#'
#' @returns `sf` tibble of all LADs in a region, with boundaries
get_regional_lads <- function(region) {
  jogger::geo_get("lad", region, "rgn", return_style = "minimal", spatial_ref = 7405)
}


#' Get MSOAs with boundaries
#'
#' @param lads_list A tibble of LADs including column of LAD names (strings)
#'
#' @returns `sf` tibble of MSOAs with boundaries
get_msoas <- function(lads_list) {
  lads_list |>
    dplyr::pull(lad20nm) |>
    purrr::map_df(~ geo_get("msoa", ., "lad", shape_fields = TRUE, spatial_ref = 7405)) |>
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


order_along <- function(df, order_along, order_by) {

  df2 <- df |>
    sf::st_drop_geometry()

  cols <- colnames(df2)

  df2 <- df2 |>
    dplyr::arrange({{ order_by }})

  df2 |>
    dplyr::select({{ order_along }}) |>
    dplyr::distinct() |>
    dplyr::full_join(df2) |>
    dplyr::select(dplyr::all_of(cols)) |>
    dplyr::left_join(df) |>
    sf::st_sf()
}
