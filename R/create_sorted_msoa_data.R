create_sorted_msoa_data <- function(
    msoa_centroids,
    msoa_bounds,
    area_bounds) { # currently itl2 areas
  area_centroids <- suppressWarnings(sf::st_centroid(area_bounds))
  
  assertthat::assert_that(
    ncol(msoa_centroids) == 10,
    nrow(msoa_centroids) == 6856,
    identical(
      colnames(msoa_centroids),
      c("msoa21cd", "msoa21nm", "msoa21hclnm", "lad23cd", "lad23nm",
        "itl221cd", "itl221nm", "rgn23cd", "rgn23nm", "geometry")
    ),
    sf::st_geometry_type(msoa_centroids[1,]) == "POINT",
    
    ncol(area_centroids) == 5,
    nrow(area_centroids) == 33, # England only
    identical(
      colnames(area_centroids),
      c("itl221cd", "itl221nm", "rgn23cd", "rgn23nm", "geometry")
    ),
    sf::st_geometry_type(area_centroids[1,]) == "POINT",
    
    ncol(msoa_bounds) == 15,
    nrow(msoa_bounds) == nrow(msoa_centroids),
    all(sf::st_geometry_type(msoa_bounds) %in% c("POLYGON", "MULTIPOLYGON")),
    all(c("msoa21cd", "lad23cd", "itl221cd", "rgn23cd", "shape_area") %in% colnames(msoa_bounds))
  )

  msoa_centroids_grouped_list <- msoa_centroids |>
    dplyr::nest_by(itl221cd, .keep = TRUE) |> # nest sorts in alpha order
    tibble::deframe()
  area_centroids_list <- area_centroids |>
    dplyr::nest_by(itl221cd, .keep = TRUE) |>
    tibble::deframe()
  
  # MSOAS sorted by proximity to parent area centroid (arranged by area)
  msoa_centroids_tbl <- msoa_centroids_grouped_list |>
    purrr::map2(area_centroids_list, create_sorted_batches) |>
    dplyr::bind_rows()
  
  msoa_bounds_tbl <- msoa_centroids_tbl |>
    sf::st_drop_geometry() |>
    dplyr::select("msoa21cd") |> 
    dplyr::left_join(msoa_bounds, "msoa21cd") |>
    sf::st_sf()

  # Regions and areas sorted by density (smallest mean area of MSOAs)
  density_sorted_tbl <- msoa_bounds |>
    sf::st_drop_geometry() |>
    dplyr::mutate(
      rgn_density = mean(shape_area),
      .by = c("rgn23cd", "rgn23nm")
    ) |>
    dplyr::summarise(
      across("shape_area", mean),
      .by = c("rgn23cd", "rgn23nm", "rgn_density", "itl221cd", "itl221nm")
    ) |>
    dplyr::arrange(pick(c("rgn_density", "shape_area"))) |>
    dplyr::select(!c("rgn_density", "shape_area")) |>
    move_merseyside() # Prioritise Merseyside over Greater Manchester

  join_cols <- colnames(density_sorted_tbl)
  density_sorted_tbl |>
    dplyr::nest_join(area_bounds, join_cols, name = "itl2_boundary") |>
    dplyr::nest_join(msoa_centroids_tbl, join_cols, name = "centroids") |>
    dplyr::nest_join(msoa_bounds_tbl, join_cols, name = "bounds")
}



#' Create density-sorted MSOA batches per area
#'
#' Uses helper functions `sort_by_init_proximity()` and
#' `sort_by_density()` to create the grouped and sorted MSOA lists
#' ready for the hex calculation process.
#'
#' @param msoa_centroids_tbl An `sf` tibble of MSOA centroids for a parent area
#' @param area_centroid The centroid point for the parent area
create_sorted_batches <- function(msoa_centroids_tbl, area_centroid) {
  assertthat::assert_that(nrow(area_centroid) == 1)

  # Arrange the list of MSOAs in each area according to how close their centroid
  # is to the centroid for the area as a whole. The intention here is to try to
  # centre to some extent each cluster of MSOAs more faithfully over the extent
  # of its parent area.
  area_ctr_prox <- sf::st_distance(msoa_centroids_tbl, area_centroid)
  
  msoa_centroids_tbl |>
    # Add col of distance of each MSOA centroid from the area's overall centroid
    dplyr::bind_cols(area_ctr_prox = area_ctr_prox) |>
    # Use `sort_by_init_proximity()` to sort each df by proxim. to initial MSOA
    sort_by_init_proximity()
}


#' Sort MSOAs by proximity to the closest ("initial") MSOA to the parent area
#'  centroid
#'
#' @param dtf An `sf` tibble of MSOA centroids in a parent area
sort_by_init_proximity <- function(dtf) {
  dtf |>
    # Sort df by proximity of the MSOA centroids to the area overall centroid
    dplyr::arrange(pick("area_ctr_prox")) |>
    dplyr::mutate(top = .data[["geometry"]][1]) |>
    dplyr::mutate(top_prox = purrr::map2_dbl(geometry, top, sf::st_distance)) |>
    # but still group by LAD as this helps keep LADs together when allocating
    order_along("lad23cd", "top_prox") |>
    dplyr::select(!c("area_ctr_prox", "top", "top_prox"))
}


order_along <- function(df, order_along, order_by) {
  assertthat::assert_that(order_along %in% colnames(df))
  assertthat::assert_that(order_by %in% colnames(df))
  df |>
    sf::st_drop_geometry() |>
    dplyr::arrange(pick({{ order_by }})) |>
    dplyr::select({{ order_along }}) |>
    dplyr::distinct() |>
    dplyr::left_join(df, {{ order_along }}) |>
    sf::st_sf()
}

move_merseyside <- function(tbl) {
  mn <- which(tbl[["itl221nm"]] == "Merseyside")
  gm <- which(tbl[["itl221nm"]] == "Greater Manchester")
  nr <- nrow(tbl)
  tbl |>
    dplyr::slice(c(seq(gm - 1), mn, gm, seq(mn + 1, nr)))
}
