#' Create hexgrid
#'
#' Combines grid creation function and hexgrid creation
#'
#' @param area_bounds An sf object (eg `sfc` collection) that provides the
#'   overall shape. With polygon boundaries.
#' @param msoas_list A regional list of MSOAs by LAD. With centroids.
#' @param cell_size The distance from top-bottom of each grid cell (metres).
#' @param ... Params to pass through to collect_hexes (currently just
#'   `compact`).
#'
#' @return A list with four elements: 1: the original area bounds, 2: a base hexgrid cropped to those hexagons that intersect with `area_bounds`, 3: just the hexes from the grid that have been allocated to MSOAs, and 4: a list of MSOA hexagons grouped by LAD.
#' @export
create_hexgrid <- function(area_bounds, msoas_list, cell_size, ...) {

  # st <- lubridate::now()
  # usethis::ui_info(
  #   stringr::str_glue("Command started at {st}"))

  # make a grid of the whole area, given cell_size as a variable.
  base_grid <- sf::st_make_grid(
    area_bounds,
    what = "polygons",
    square = FALSE,
    flat_topped = TRUE,
    cellsize = cell_size
  )

  # grid covering whole regional area - all intersecting hexagons
  cropped_grid <- base_grid %>%
    sf::st_intersects(sf::st_union(area_bounds), sparse = FALSE) %>%
    which() %>%
    `[`(base_grid, .)

  # commented out for now as only needed for original London grid
  # (and I have a better way of doing this now anyway)
  # cropped_grid <- cropped_grid[-omit_cells]


  # fill in any holes (we may need them!)
  # remainder <- base_grid %>%
  #   sf::st_difference(sf::st_union(cropped_grid))
  # remainder <- remainder %>%
  #   sf::st_touches(sf::st_union(cropped_grid), sparse = FALSE) %>%
  #   which() %>%
  #   `[`(remainder, .)
  # holes <- which(lengths(sf::st_touches(remainder, cropped_grid)) == 6)
  #
  # if (length(holes) > 0) {
  #   cropped_grid <- c(cropped_grid, remainder[holes])
  # }


  # use the hex_array() function to create results for each LAD given a
  # regional grid as a starting point...
  # ... this works through each LAD's list of MSOAs iteratively and
  # uses `reduce` to produce a neat set of results
  msoa_hexes <- msoas_list %>%
    purrr::reduce(
      hex_array,
      .init = list(NULL, cropped_grid),
      lads = length(msoas_list),
      ...
    ) %>%
    purrr::pluck(1)

  # hex_centroids <- msoa_hexes %>%
  #   sf::st_centroid() %>%
  #   split( ~ msoa11cd)
  #
  # msoa_centroids <- msoas_list %>%
  #   purrr::reduce(dplyr::bind_rows) %>%
  #   split( ~ msoa11cd)
  #
  # distances <- purrr::map2_dbl(
  #   hex_centroids,
  #   msoa_centroids,
  #   ~ sf::st_distance(.x, .y)
  # )
  #
  # median_accuracy <- distances %>%
  #   stats::median() %>%
  #   round()
  #
  # mean_accuracy <- distances %>%
  #   mean() %>%
  #   round()
  #
  # efficiency <- (100 * length(hex_centroids)/length(cropped_grid)) %>%
  #   round(1)

  lad_hex_groups <- msoa_hexes %>%
    dplyr::group_by(ltla21cd, ltla21nm) %>%
    # dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")
    dplyr::summarise(.groups = "drop")

  # et <- lubridate::now()
  # dur <- lubridate::as.duration(et - st)
  # usethis::ui_info(
  #   stringr::str_glue("Command ended at {et}. Duration: {dur}"))


  # return:
  list(bounds = area_bounds,
       grid = cropped_grid,
       hexes = msoa_hexes,
       lads = lad_hex_groups
       # cell_size = cell_size,
       # median_accuracy = median_accuracy,
       # mean_accuracy = mean_accuracy,
       # efficiency = efficiency
       )
}





# create_hexgrid_national -------------------------------------------------



#' Create a hexgrid for a group of regions or a whole nation
#'
#' Each region calculations needs to take into account any hexes from a
#' national grid already claimed by previous regions. Uses a grid already
#' created by previous runs, or manually as an .`init`. Designed for use
#' within a `map` loop (hence returns `n` as `n + 1`, etc)
#'
#' @param data_inputs A list created by the previous run, or from `.init`.
#' @param area_bounds An sf object (eg `sfc` collection) that provides the
#'   overall shape. With polygon boundaries.
#' @param msoas_list A regional list of MSOAs by LAD. With centroids.
#' @param ... params to pass through to `collect_hexes` (currently just
#'   `compact`).
#' @return
#' @export
create_hexgrid_national <- function(data_inputs, area_bounds, msoas_list, ...) {

  if (require(lubridate) & require(usethis) & require(stringr)) {
  st <- lubridate::now()
  usethis::ui_info(
    stringr::str_glue("Command started at {st}"))
  }

  prev_hexes <- data_inputs[[1]]
  prev_lads <- data_inputs[[2]]
  master_grid <- data_inputs[[3]]
  n <- data_inputs[[4]]

  # compact_score <- 2

  lads <- length(msoas_list)

  if (require(usethis)) {
  usethis::ui_info(
    paste0("Processing region ", n, ": ", names(eng_rgns_sorted[n]),
           "\nTotal LADs: ", lads,
           "\nTotal MSOAs: ", sum(purrr::map_dbl(msoas_list, nrow)))
  )
  }


  # grid covering as much of regional area as is left available
  # (should be nearly all!)
  cropped_grid <- master_grid %>%
    sf::st_intersects(sf::st_union(area_bounds), sparse = FALSE) %>%
    which() %>%
    `[`(master_grid, .)

  # avail <- length(cropped_grid)

  # use the hex_array() function to create results for each LAD given a
  # regional grid as a starting point...
  # ... this works through each LAD's list of MSOAs iteratively and
  # uses `reduce` to produce a neat set of results
  msoa_hexes <- msoas_list %>%
    purrr::reduce(
      hex_array,
      .init = list(NULL, cropped_grid),
      compact = compact_score,
      lads = lads
    ) %>%
    purrr::pluck(1)


  lad_hex_groups <- msoa_hexes %>%
    dplyr::group_by(ltla21cd, ltla21nm) %>%
    dplyr::summarise(.groups = "drop")


  remaining_grid <- sf::st_difference(master_grid, sf::st_union(msoa_hexes))

  result_hexes <- prev_hexes %>%
    dplyr::bind_rows(msoa_hexes) %>%
    dplyr::relocate(msoa11hclnm) # 1st col so that hover in view mode shows name

  result_lads <- prev_lads %>%
    dplyr::bind_rows(lad_hex_groups) %>%
    dplyr::relocate(ltla21nm) # 1st col, so that hover in view mode shows name


  if (require(lubridate) & require(usethis) & require(stringr)) {
  et <- lubridate::now()
  dur <- lubridate::as.duration(et - st)
  usethis::ui_info(
    stringr::str_glue("Command ended at {et}. Duration: {dur}"))
  }

  # return:
  list(
       hexes = result_hexes,
       lads = result_lads,
       grid = remaining_grid,
       n = n + 1
       # cells = avail,
       # efficiency = round(nrow(result_hexes)*100/avail, 1)
  )
}



# hex_array ---------------------------------------------------------------



#' Hex array: the guts of create_hexgrid()
#'
#' @param data_inputs A list created by previous run:
#'   `list(previous results, grid)`.
#' @param msoas_list A list of MSOAs in a LAD, sorted by proximity to
#'   LAD centroid or by shape_area.
#' @param lads total number of areas being processed in current batch
#' @param ... Params to pass on to `collect_hexes` (currently just `compact`).
#'
#' @return A list: current results and a grid
hex_array <- function(data_inputs, msoas_list, lads, ...) {

  # split out data_inputs (data to work with) as the basis for the function
  prev_results <- data_inputs[[1]]
  grid <- data_inputs[[2]]

  if (require(usethis)) {
  usethis::ui_info(paste0(
    unique(msoas_list$ltla21nm),
    ": ",
    nrow(msoas_list),
    " MSOAs"))
  }

  lad_hexes <- msoas_list %>%
    # splits the df by row, in order
    dplyr::group_split(dplyr::row_number(), .keep = FALSE) %>%
    purrr::reduce(collect_hexes, .init = list(NULL, grid), ...)

  # add results to stored previous results
  results <- dplyr::bind_rows(prev_results, lad_hexes[[1]])

  if (require(usethis)) {
  usethis::ui_info(paste0(
    "*************************************\n",
    "***** ",
    stringr::str_pad(
      length(unique(results$ltla21nm)),
      width = 2,
      side = "left",
      ),
    " LADs of ", lads, " completed *****\n",
    "*************************************\n"
  ))
  }

  # return results so far and remaining grid:
  list(results, lad_hexes[[2]])

}


# collect_hexes -----------------------------------------------------------



#' Collect hexes
#'
#' @param data_inputs list created by previous run:
#' list(previous results, grid)
#' @param msoa a particular MSOA (with centroid point geography),
#' whose best hex location from the hex grid is to be calculated
#' @param compact the degree to which the choice of which hex to allocate to
#' the current MSOA hshould prefer to touch any existing hexes in the group
#' (eg a local authority group). 1 means that any hex touching 1 or more
#' existing hexes in the group will always be preferred to a hex that isn't
#' touching any previous hexes in the current group.
#'
#' @return a list: current results and the remaining grid
collect_hexes <- function(data_inputs, msoa, compact = 1) {

  # split out data_inputs (data to work with) as the basis for the function
  prev_results <- data_inputs[[1]]
  grid <- data_inputs[[2]]

  # if we have any "results" produced already...
  # ...and at least one of the remaining grid hexes touches the results hexes:
  if (!is.null(prev_results)) {
    unified_results <- sf::st_union(prev_results)
  }
  if (!is.null(prev_results) && any(
    sf::st_touches(grid, unified_results, sparse = FALSE)
    )) {

    # indices of all grid items that touch results (equiv. compact == 1)
    touching <- which(sf::st_touches(grid, unified_results, sparse = FALSE))
    touching_out <- NULL

    while (compact > 1 && is.null(touching_out)) {
      # indices of all grid hexes that touch results "compact" times or more
      touching_multiple <- which(
        lengths(sf::st_touches(grid, prev_results)) >= compact)

      # prefer using hexes touching multiple existing results, if available
      # or else touching_out remains NULL and we try again
      if (length(touching_multiple) > 0) {
        touching_out <- touching_multiple
      }

      compact <- compact - 1
    }

    # if the compact procedure found anything, then use it,
    # otherwise just use the original touching index (any hexes that touch)
    if (length(touching_out) > 0) touching <- touching_out

    # pull out a group of hexes from "grid" that touch "results"...
    touching_grid <- grid[touching]
    touching_centroids <- sf::st_centroid(touching_grid)
    nearest_index <- sf::st_nearest_feature(msoa, touching_centroids)
    nearest_hex <- sf::st_set_geometry(msoa, touching_grid[nearest_index])

  } else {
    # if this is the first loop, or there's no touching hexes available:
    grid_centroids <- sf::st_centroid(grid)
    nearest_index <- sf::st_nearest_feature(msoa, grid_centroids)
    nearest_hex <- sf::st_set_geometry(msoa, grid[nearest_index])
  }

  # add the chosen hex to "results"
  results <- dplyr::bind_rows(prev_results, nearest_hex)

  # and subtract it from the grid
  grid_out <- sf::st_difference(grid, nearest_hex)


  # notify console
  if (require(usethis)) {
  usethis::ui_info(paste0(
    stringr::str_pad(
    nrow(results),
    width = 3,
    side = "left"
    ),
    " MSOAs processed"
  ))
  }

  # return
  list(results, grid_out)
}
