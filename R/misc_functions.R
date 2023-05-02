#' Just a chooser for one of the 9 English regions
#'
#' @param regions_list A tibble of 9 English regions
#' @param n numeric. Which region to choose (1-9, alphabetical order)
#'
#' @returns The region name as a string
get_current_region <- function(regions_list, n) {
  regions_list |>
    dplyr::slice(n) |>
    dplyr::pull(all_of("rgn22nm"))
}


#' Get regional LADs
#'
#' @param region string. The name of region
#'
#' @returns `sf` tibble of all LADs in a region, with boundaries
get_regional_lads <- function(region) {
  boundr::bounds(
    "lad", "rgn", region,
    return_width = "tidy",
    resolution = "BGC",
    crs = 27700)
}

get_all_region_lads <- function(eng_regions) {
  region_names <- eng_regions |>
      dplyr::pull(all_of("rgn22nm"))
  region_names |>
    purrr::map(get_regional_lads, .progress = TRUE) |>
    rlang::set_names(region_names)
}

get_all_lad_centroids <- function(lads_by_region) {
  lads_by_region |>
    purrr::map(sf::st_centroid) |>
    purrr::set_names(names(lads_by_region))
}


#' Get MSOAs with boundaries
#'
#' @param lads_list A tibble of LADs including column of LAD names (strings)
#'
#' @returns `sf` tibble of MSOAs with boundaries
get_msoas <- function(lads_list) {
  lads_list |>
    dplyr::pull(all_of("lad22cd")) |>
    purrr::map(
      \(x) boundr::bounds(
        "msoa", "lad", within_codes = x,
        return_width = "full",
        lookup_year = "2021",
        within_year = "2022",
        country_filter = "EW",
        resolution = "BGC",
        option = 4,
        crs = 27700),
      .progress = TRUE
      ) |>
    purrr::list_rbind() |>
    janitor::clean_names() |>
    dplyr::relocate(
      any_of(c("shape_area", "shape_length")),
      .before = last_col())
}

get_msoas_by_region <- function(lads_by_region, eng_regions) {
  region_names <- eng_regions |>
      dplyr::pull(all_of("rgn22nm"))
  lads_by_region |>
    purrr::map(get_msoas, .progress = TRUE) |>
    rlang::set_names(region_names)
}


#' Get MSOA centroids
#'
#' @param region name of region
#'
#' @return sf tibble of MSOAs in a region with centroid geometries
get_msoa_centroids <- function(lad_code) {
  boundr::bounds(
    "msoa", "lad",
    within_codes = lad_code,
    return_width = "tidy",
    lookup_year = "2021",
    within_year = "2022",
    country_filter = "EW",
    option = 4,
    centroids = TRUE,
    crs = 27700)
}

get_msoa_centroids_by_rgn <- function(lads_by_region, eng_regions) {
  region_names <- eng_regions |>
      dplyr::pull(all_of("rgn22nm"))
  lads_by_region |>
    purrr::map(get_msoa_centroids_by_lad, .progress = TRUE) |>
    rlang::set_names(region_names)
}


get_msoa_centroids_by_lad <- function(lads_df) {
  lad_codes <- lads_df |>
    dplyr::pull(all_of("lad22cd"))
  lad_codes |>
    purrr::map(get_msoa_centroids) |>
    purrr::list_rbind() |>
    sf::st_sf()
}

sort_msoas_by_proximity <- function(
  msoa_centroids_by_region,
  eng_lad_centroids,
  msoas_by_region) {
  
  list(
    msoa_centroids_by_region,
    eng_lad_centroids,
    msoas_by_region
  ) |>
  purrr::pmap(create_sorted_msoa_batches) |>
  rlang::set_names(names(msoas_by_region))
}

# make a grid of the whole country, given `cell_size` as a variable.
create_base_grid <- function(eng_regions, cell_size) {
  england <- sf::st_union(eng_regions)
  sf::st_make_grid(
    # 15826 hexes
    england,
    what = "polygons",
    square = FALSE,
    flat_topped = TRUE,
    cellsize = cell_size
  ) |>
    purrr::keep(
      \(x) as.vector(sf::st_intersects(x, england, sparse = FALSE)))
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
