allocate <- function(sorted_regions_data) {
  sorted_regions_data |>
    purrr::reduce(allocate_region, .init = NULL)
}

create_sorted_regions_data <- function(sorted_msoa_data) {
  sorted_col <- sorted_msoa_data |>
    # Leave London out for now (we already have it removed from base grid)
    dplyr::filter(rgn23nm != "London") |>
    dplyr::select("rgn23cd") |>
    dplyr::distinct()
  nested_data <- sorted_msoa_data |>
    dplyr::nest_by(rgn23cd, .keep = TRUE)
  sorted_col |>
    dplyr::left_join(nested_data, "rgn23cd") |>
    tibble::deframe()
}

allocate_region <- function(prev_hexes, region_data, grid = base_grid_orig) {
  if (!is.null(prev_hexes)) {
    hex_union <- sf::st_union(prev_hexes)
    border <- sf::st_union(grid[which(sf::st_touches(grid, hex_union, FALSE))])
    grid <- sf::st_difference(grid, sf::st_union(hex_union, border))
  }
  sorted_col <- region_data |>
    dplyr::select("itl221cd") |>
    dplyr::distinct()
  nested_data <- region_data |>
    dplyr::nest_by(itl221cd, .keep = TRUE)
  sorted_list <- sorted_col |>
    dplyr::left_join(nested_data, "itl221cd") |>
    tibble::deframe()

  region_hexes <- sorted_list |>
    purrr::reduce(allocate_itl, .init = list(NULL, grid)) |>
    purrr::pluck(1)
  dplyr::bind_rows(prev_hexes, region_hexes)
}

allocate_itl <- function(prev_res, itl_data) {
  itl_name <- itl_data[["itl221nm"]]
  base_grid <- prev_res[[2]]
  init_cols <- c("rgn23cd", "rgn23nm", "itl221cd", "itl221nm")
  itl_bound <- itl_data[["itl2_boundary"]][[1]]
  mcentroids_tbl <- itl_data |>
    dplyr::select(c(all_of(init_cols), "centroids")) |>
    tidyr::unnest("centroids") |>
    sf::st_sf()
  mbounds_tbl <- itl_data |>
    dplyr::select(c(all_of(init_cols), "bounds")) |>
    tidyr::unnest("bounds") |>
    sf::st_sf()

  # buffered_bbox <- sf::st_buffer(sf::st_as_sfc(sf::st_bbox(itl_bound)), 5000)
  # itl_grid <- base_grid[buffered_bbox]
  itl_grid <- base_grid

  itl_hexes <- allocate_hexes(mcentroids_tbl, mbounds_tbl, itl_grid)
  bounds_n <- nrow(mbounds_tbl)
  hexes_n <- nrow(itl_hexes)

  if (bounds_n == hexes_n) {
    cli::cli_alert_success(
      "ITL2 area {itl_name}: {bounds_n} MSOAs, {hexes_n} hexes allocated"
    )
  } else {
    cli::cli_alert_warning(
      "ITL2 area {itl_name}: {bounds_n} MSOAs, {hexes_n} hexes allocated"
    )
  }

  hex_union <- sf::st_union(itl_hexes)
  itl_border <- base_grid[which(sf::st_touches(base_grid, hex_union, FALSE))]

  rem_grid <- base_grid |>
    sf::st_difference(sf::st_union(hex_union, sf::st_union(itl_border)))
  list(dplyr::bind_rows(prev_res[[1]], itl_hexes), rem_grid)
}


allocate_hexes <- function(mcentroids, mbounds, grid, results = NULL) {
  assertthat::assert_that(nrow(mcentroids) == nrow(mbounds))
  if (length(grid) == 0) {
    cli::cli_alert_info("No more grid hexes available.")
    return(results)
  }
  
  
  this_centroid <- dplyr::slice(mcentroids, 1)
  mcentroids <- dplyr::slice(mcentroids, -1)
  this_bound <- dplyr::slice(mbounds, 1)
  remaining_bounds <- dplyr::slice(mbounds, -1)
  
  # set up first MSOA of ITL area
  if (is.null(results)) {
    touching_grid <- grid
  } else {
  # Thereafter we can try to join on new hexes to existing results
    focus_area <- sf::st_union(results)
    g_touches <- which(sf::st_touches(grid, focus_area, FALSE))
    touching_grid <- if (length(g_touches) > 0) grid[g_touches] else grid
  }
  
  nearest_index <- sf::st_nearest_feature(this_centroid, touching_grid)
  nearest_hex <- touching_grid[nearest_index]
  this_msoa_hex <- sf::st_set_geometry(this_centroid, nearest_hex)
  grid <- sf::st_difference(grid, nearest_hex)
  
  results <- dplyr::bind_rows(results, this_msoa_hex)

  if (nrow(remaining_bounds) > 0) {
    focus_area <- sf::st_union(results)
    
    g_touches <- which(sf::st_touches(grid, focus_area, FALSE))
    touching_grid <- if (length(g_touches) > 0) {
      grid[g_touches]
    } else {
      grid[sf::st_nearest_feature(nearest_hex, grid)]
    }
    
    b_touches <- which(sf::st_touches(remaining_bounds, this_bound, FALSE))
    touching_msoas <- if (length(b_touches) > 0) {
      dplyr::slice(remaining_bounds, b_touches)
    } else {
      remaining_bounds |>
        dplyr::slice(sf::st_nearest_feature(this_bound, remaining_bounds))
    }
  
    assertthat::assert_that(
      length(touching_grid) > 0,
      nrow(touching_msoas) > 0
    )

    to_allocate <- touching_msoas |>
      dplyr::slice_head(n = length(touching_grid)) |>
      dplyr::select(!matches("^(bng_|shape_)"))
  
    distance_mat <- sf::st_distance(to_allocate, touching_grid)
    solved_mat <- RcppHungarian::HungarianSolver(distance_mat)
    allocated_hexes <- touching_grid[solved_mat[["pairs"]][,2]]
    assertthat::assert_that(length(allocated_hexes) == nrow(to_allocate))
    msoas_allocated <- sf::st_set_geometry(to_allocate, allocated_hexes)
  

    new_grid <- sf::st_difference(grid, sf::st_union(allocated_hexes))
    cluster_results <- dplyr::bind_rows(results, msoas_allocated)
  
    msoa_alloc_codes <- msoas_allocated[["msoa21cd"]]
    rem_cens <- mcentroids |>
      dplyr::filter(!if_any("msoa21cd", \(x) x %in% msoa_alloc_codes))
    rem_bnds <- remaining_bounds |>
      dplyr::filter(!if_any("msoa21cd", \(x) x %in% msoa_alloc_codes))
    assertthat::assert_that(nrow(rem_cens) == nrow(rem_bnds))

    if (nrow(rem_bnds) == 0) {
      cluster_results
    } else {
      allocate_hexes(rem_cens, rem_bnds, new_grid, cluster_results)
    }
  } else {
    results
  }
}
