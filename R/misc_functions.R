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

  sorted_list <- list(
    msoa_centroids_by_region,
    eng_lad_centroids,
    msoas_by_region
  ) |>
  purrr::pmap(create_sorted_msoa_batches) |>
  rlang::set_names(names(msoas_by_region))

  rgns_sorted_by_density <- msoas_by_region |>
    purrr::imap_dbl(
      \(x, y) y = sum(x[["shape_area"]]) / nrow(x)) |>
    sort() |>
    names()

  sorted_list[rgns_sorted_by_density]
}

# make a grid of the whole country, given `cell_size` as a variable.
create_base_grid <- function(eng_regions, cell_size) {
  sf::st_make_grid(
    # 15826 hexes
    sf::st_union(eng_regions),
    what = "polygons",
    square = FALSE,
    flat_topped = TRUE,
    cellsize = cell_size
  )
}


fettle_edges <- function(base_grid) {
  estuaries_etc <- list(
    solent = c(915, 962, 1106, 1154, 1207, 1262, 1059, 1060),
    thames = c(
      4445, 4488, 4444, 4487, 4443, 4353, 4305, 4352, 4304, 4351, 4396),
    severn = c(4758, 4805, 4898, 4945, 4992),
    mersey = c(11538, 11457, 11373, 11331, 11245, 11203),
    humber = c(12077, 12115, 12190, 12228, 12303, 12341, 12302, 12264),
    borders = c(
      15804, 15770, 15722, 15620, 15584, 15534, 15282, 15243, 10945, 10903,
      10820, 10572, 10413, 10115, 10016, 9687, 9447, 9267, 9030, 8182, 8304,
      7995, 7871, 7507, 7448, 6858, 6271, 6157, 5824, 5716, 5662, 5458, 4712)
  ) |>
    purrr::list_c()

  base_grid[-estuaries_etc]
}


extract_london_index <- function(base_grid, eng_regions) {
  london <- eng_regions |>
    dplyr::slice(7)

  hex_area <- sf::st_area(base_grid[1])

  base_grid |>
    purrr::map(\(x) sf::st_sfc(x, crs = 27700)) |>
    purrr::map(\(x) sf::st_intersection(x, london)) |>
    purrr::map_lgl(
      \(x) ifelse(length(x), sf::st_area(x) > (hex_area / 10), FALSE)) |>
    which()
}



extract_london <- function(base_grid_full, london_index) {
  london_orig_hex <- base_grid_full[london_index]
  london_union <- sf::st_union(london_orig_hex)

  # big enough buffer so we get >1001 hexes from the shape
  # (1001 2021 MSOAs in London)
  london_swole <- sf::st_buffer(london_union, 26000)
  london_swole_hex <- base_grid_full[london_swole]

  london_trim <- c(
    19, 28, 39, 49, 75, 61, 89, 74, 102, 88, 116,
    228, 459, 585, 995, 1009, # west edge
    989, 957, 922, 1017, # Lea Valley
    1048, 1029, 1018, 1040, 1030, 1005, 1041, 1019, 992,
    991, 1006, 1031, 1020, 1042, 1021, 1032, 1007,
    992, 29, 51, 12, 20,
    2, 8, 9, 3, 15,
    417, 438, 478, 437, 518, 496, 497, 519, 498,  # Thames estuary
    164, 180, 197 # Richmond Park
  )
  london_trimmed <- london_swole_hex[-london_trim]


  # london_index <- base_grid_full |>
  #   sf::st_intersects(london_trimmed, sparse = FALSE) |>
  #   as.vector() |>
  #   which()
  # to move London into the North Sea, move the index + 26493
  # london_inset <- base_grid_full[london_swole_index + 26493]

  # return:
  # london_inset
  london_trimmed

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
