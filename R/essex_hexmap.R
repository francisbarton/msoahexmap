
# libraries ---------------------------------------------------------------
{
  library(here)
  library(dplyr)
  library(extrafont)
  library(jogger)
  library(myrmidon)
  library(purrr)
  library(sf)
  library(tmap)
  library(wesanderson)
  library(ggsci)
}


tmap_mode("plot")

tmap_options(fontfamily = "Source Sans Pro")


# get some data to start from ---------------------------------------------


essex_lads <- jogger::geo_get("lad", "Essex", "cty", return_style = "minimal", spatial_ref = 27700)
save_it(essex_lads)

# essex_lads <- readRDS(here::here("rds_data", "essex_lads.Rds"))


essex_bounds <- essex_lads %>%
  dplyr::pull(lad20nm) %>%
  purrr::map_df(~ geo_get("msoa", ., "lad", shape_fields = TRUE, spatial_ref = 27700)) %>%
  dplyr::relocate(c(shape_area, shape_length), .before = last_col())
save_it(essex_bounds)

# essex_bounds <- readRDS(here::here("rds_data", "essex_bounds.Rds"))


essex1 <- tm_shape(essex_bounds) +
  tm_borders("grey33", lwd = 2)
essex1
tmap_save(essex1, "essex1.png")


# instead of using sf::st_centroid to calculate geometric centroids,
# use ONS population-weighted centroids:
essex_centroids <- jogger::geo_get("msoa", "Essex", "cty", return_centroids = TRUE, return_style = "minimal", spatial_ref = 27700)

essex_centroids <- essex_bounds %>%
  sf::st_drop_geometry() %>%
  dplyr::select(starts_with(c("msoa", "lad"))) %>%
  dplyr::left_join(essex_centroids, .)
save_it(essex_centroids)

# essex_centroids <- readRDS(here::here("rds_data", "essex_centroids.Rds"))

# Create a list of MSOAs (with actual MSOA boundaries) for each LAD (n = 12)
# no longer needed????
# essex_lads_split <- essex_bounds %>%
#   split(forcats::fct_inorder(.$lad20nm))
# R 4.1.0 style:
# essex_lads_split <- essex_bounds %>%
#   split( ~ lad20nm)

# Create a list of MSOAs (with centroid geometry) for each LAD
essex_centroids_split <- essex_centroids %>%
  split( ~ lad20nm)

# more complex way I used previously to ensure that the order was the same
# for the split list of LADs and the split list of centroids:
# essex_centroids_split <- essex_centroids %>%
#   split(forcats::fct_relevel(forcats::fct_inorder(.$lad20nm), names(essex_lads_split)))

essex_lad_centroids <- essex_lads %>%
  split( ~ lad20nm) %>%
  purrr::map(sf::st_centroid)


# make some maps, why not ----------------------------------------------

essex_grid <- sf::st_make_grid(
  essex_bounds,
  what = "polygons",
  square = FALSE,
  flat_topped = TRUE,
  cellsize = 3750
)
essex_grid_centroids <- sf::st_centroid(essex_grid)


essex_grid1 <- tm_shape(essex_bounds) +
  tm_borders("grey33", lwd = 2) +
  tm_shape(essex_grid) +
  tm_borders("green", lwd = 1) +
  tm_shape(essex_centroids) +
  tm_dots(col = "orange", size = 0.1)
essex_grid1
tmap_save(essex_grid1, "essex_grid1.png")


essex_grid2 <- tm_shape(essex_grid) +
  tm_borders("olivedrab4", lwd = 2, alpha = 0.5) +
  tm_fill("grey85", alpha = 0.3) +
  tm_shape(essex_lads) +
  tm_borders("grey45", lwd = 2) +
  tm_fill("lad20nm", alpha = 0.8, palette = my_pal(13)) +
  # tm_shape(essex_bounds) +
  # tm_borders("grey75", lwd = 1) +
  tm_shape(essex_centroids) +
  tm_dots(col = "white", size = 0.2) +
  tm_shape(essex_centroids) +
  tm_dots(col = "tomato2", size = 0.1) +
  tm_layout(legend.outside = TRUE)
essex_grid2
tmap_save(essex_grid2, "essex_grid2.png")


essex_grid3 <- tm_shape(essex_bounds) +
  tm_borders("grey75", lwd = 1) +
  tm_shape(essex_lads) +
  tm_borders("grey45", lwd = 2) +
  tm_shape(essex_centroids) +
  tm_dots(col = "orange", size = 0.1) +
  tm_shape(purrr::map_dfr(essex_lad_centroids, sf::st_sf)) +
  tm_dots(col = "firebrick", size = 0.2)

essex_grid3
tmap_save(essex_grid3, "essex_grid3.png")


# ---



# Arrange the list of MSOAs in each LAD according to how close their centroid
# is to the LAD's centroid. The intention here is to try to centre to some
# extent each cluster of MSOAs more faithfully over the extent of its LAD

# for each LAD's batch of MSOA centroids...
essex_lads_by_centroid_proximity <- essex_centroids_split %>%

  # create a list of distances from the LAD's overall centroid...
  purrr::map2(., essex_lad_centroids, ~ sf::st_distance(.x, .y)) %>%

  # convert the outputs from st_distance to vectors...
  purrr::map(as.vector) %>%

  # join these vectors back to the source dfs...
  purrr::map2(., essex_centroids_split, ~ dplyr::bind_cols(.y, proximity = .x)) %>%

  # and then sort each df by proximity of the MSOAs
  purrr::map(~ arrange(., proximity))

# TODO This is all very well but ideally we should have the MSOA closest to the
# LAD centroid at the top, as here, but then the rest of the MSOAs listed in
# order of closeness to the *top MSOA*, not in order of proximity to the
# LAD centroid. This should help keep adjacent MSOAs together.



# Use the "density" of MSOAs in each LAD as the way to order the hexing process.
# Density here is total LAD area divided by number of MSOAs in that LAD
essex_lads_by_density <- essex_bounds %>%
  st_drop_geometry() %>%
  group_by(lad20nm) %>%
  summarise(density = sum(shape_area)/n()) %>%
  ungroup() %>%
  # order LADs according to density...
  arrange(density) %>%
  pull(lad20nm) %>%
  # and then re-order this list accordingly
  `[`(essex_lads_by_centroid_proximity, .)



# using the functions below to generate grids and hex collections more smoothly


my_pal <- ggsci::pal_futurama()
my_pal <- ggsci::pal_d3("category20")

# tmap_design_mode()
essex_hexmap <- create_hexmap(essex_bounds, essex_lads_by_density, 4500)
essex_hexmap

tmap_save(essex_hexmap, "essex_hexmap_4500.png")


# —————————————————————————————————————————————————————————————————————————



# FUNCTIONS TIME! ---------------------------------------------------------


# create_hexmap: combining create_grid and tmapping  ----------------------


create_hexmap <- function(msoa_bounds, lads_list, cell_size) {

  grid_fill <- create_grid(msoa_bounds, lads_list, cell_size)

  tm_shape(grid_fill[[1]]) + # base grid
    tm_borders("olivedrab4", lwd = 1, alpha = 0.1) +
    tm_fill("grey65", alpha = 0.3) +
    tm_shape(sf::st_union(msoa_bounds)) + # county boundary
    tm_borders("grey45") +
    tm_shape(grid_fill[[2]] %>% # MSOA hexes
               dplyr::mutate(
                 label = toupper(
                   paste0(
                     stringr::str_extract(lad20nm, "^.{2,4}"),
                     ".\n",
                     stringr::str_extract(msoa11hclnm, "^.{2,5}"),
                     "\n",
                     stringr::str_extract(msoa11nm, "[0-9]{3}$")
                   )
                 )
               )
    ) +
    tm_borders() +
    tm_fill("lad20nm", alpha = 0.7, palette = my_pal(12)) +
    tm_text("label", size = 0.5) +
    tm_layout(title = paste0(
      "MSOAs in Essex: "
      , nrow(grid_fill[[2]])
      , "\n"
      , "Cells (hexagons): "
      , length(grid_fill[[1]])
      , "\n"
      , "Cell size = "
      , cell_size
      , "m"
    )
    , legend.show = FALSE)

}

# create_grid: create hex blocks for each lad -----------------------------


# create_hexmap(essex_bounds, essex_lads_by_density, 4000)
create_grid <- function(area_bounds, msoas_list, cell_size) {

  # make a grid of the whole area, given cell_size as a variable
  grid <- sf::st_make_grid(
    area_bounds,
    what = "polygons",
    square = FALSE,
    flat_topped = TRUE,
    cellsize = cell_size
  ) %>%
    sf::st_sfc(crs = 27700)

  # use the hex_array function to create results for each area given the full
  # grid as a starting point...
  # ... this works through each list of MSOAs iteratively and uses `reduce`
  # to produce a neat set of results
  hexes <- msoas_list %>%
    purrr::reduce(
      hex_array,
      .init = list(NULL, grid)
    ) %>%
    purrr::pluck(1)

  # output a list of the original grid and the hexes so far, to be used for
  # next run of the function
  list(grid, hexes)
}



# hex_array: the guts of create_grid --------------------------------------

essex_grid <- sf::st_make_grid(
  essex_lads,
  what = "polygons",
  square = FALSE,
  flat_topped = TRUE,
  cellsize = 3750
) %>%
  sf::st_sfc(crs = 27700)

test_out <- hex_array(list(test_out, essex_grid), essex_lads_by_density[["Harlow"]], first_pass = FALSE)


tm_shape(essex_grid) + # base grid
  tm_borders("olivedrab4", lwd = 1, alpha = 0.1) +
  tm_fill("grey65", alpha = 0.3) +
  tm_shape(sf::st_union(essex_lads)) + # county boundary
  tm_borders("grey45") +
  tm_shape(test_out[[1]] %>%         # MSOA hexes
             dplyr::mutate(
               label = toupper(
                 paste0(
                   stringr::str_extract(lad20nm, "^.{2,4}"),
                   ".\n",
                   stringr::str_extract(msoa11hclnm, "^.{2,5}"),
                   "\n",
                   stringr::str_extract(msoa11nm, "[0-9]{3}$")
                 )
               )
             )
  ) +
  tm_borders() +
  tm_text("label", size = 0.5) +
  tm_shape(test_out[[1]]) +
  tm_borders("red") +
  tm_shape(sf::st_centroid(test_out[[2]])) +
  tm_dots("purple", size = 0.1)



hex_array <- function(data_inputs, msoas_list, first_pass = TRUE) {

  # split out data_inputs (data to work with) as the basis for the function
  # results <- data_inputs[[1]] # list(*results*, grid_out, prev_results)
  grid <- data_inputs[[2]]    # list(results, *grid_out*, prev_results)

  if (first_pass) {   # if this is the first loop through for this LAD
    # store all previous results safely!
    prev_results <- data_inputs[[1]]
  } else {
    prev_results <- data_inputs[[3]] # list(results, grid_out, *prev_results*)
  }

  lad_hexes <- msoas_list %>%
    split( ~ msoa11cd) %>%
    purrr::reduce(find_hexes, .init = list(NULL, grid))

  # add results to stored previous results
  results <- dplyr::bind_rows(prev_results, lad_hexes[[1]])

  # return results so far and remaining grid:
  list(results, lad_hexes[[2]])

}


# find_hexes: engine func of hex_array ------------------------------------
# (refactored out for clarity)


find_hexes <- function(data_inputs, msoa) {

  # split out data_inputs (data to work with) as the basis for the function
  results <- data_inputs[[1]]
  grid <- data_inputs[[2]]

  # if we have any "results" produced already...
  # ...and at least one of the remaining grid hexes touches the results hexes
  if (!is.null(results) && any(
    sf::st_touches(
      x = sf::st_union(results),
      y = grid,
      sparse = FALSE)[1, ]
  )) {

    # pull out a group of hexes from "grid" that touch "results"...
    touching_grid <- grid %>%
      `[`(which(sf::st_touches(x = sf::st_union(results), y = grid, sparse = FALSE)[1, ]))

    touching_centroids <- touching_grid %>%
      sf::st_centroid()

    nearest_index <- sf::st_nearest_feature(msoa, touching_centroids)
    nearest_hex <- sf::st_set_geometry(msoa, `[`(touching_grid, nearest_index))

    # if we haven't yet got any results, or there's no touching hexes left:
  } else {
    grid_centroids <- sf::st_centroid(grid)
    nearest_index <- sf::st_nearest_feature(msoa, grid_centroids)
    nearest_hex <- sf::st_set_geometry(msoa, `[`(grid, nearest_index))
  }


  # add the recently calculated hex to "results"
  results <- dplyr::bind_rows(
    results,
    nearest_hex
  )

  # and subtract it from the grid
  grid_out <- sf::st_difference(grid, nearest_hex)

  # return
  list(results, grid_out)

}





























## previous version: still not quite working

# hex_array <- function(input_list, msoas_sf, first_pass = TRUE) {
#
#
#   # split out input_list (data to work with) as the basis for the function
#   results <- input_list[[1]] # list(*results*, grid_out, prev_results)
#   grid <- input_list[[2]]    # list(results, *grid_out*, prev_results)
#
#   if (first_pass) {
#     prev_results <- results
#     results <- NULL
#   } else {
#     prev_results <- input_list[[3]] # list(results, grid_out, *prev_results*)
#   }
#
#
#   # take the list of MSOAs and slice off the first item; pass on the remainder
#   msoa <- msoas_sf %>%
#     dplyr::slice(1)
#   msoas_out <- msoas_sf %>%
#     dplyr::slice(-1)
#
#
#
#   # if we have any "results" produced already...
#   if (!is.null(results) && any(
#     sf::st_touches(
#       x = sf::st_combine(results),
#       y = grid,
#       sparse = FALSE)[1, ]
#   )) {
#
#     # pull out a group of hexes from "grid" that touch "results"...
#     # (discovered that I need to use st_combine to make this work as intended)
#     filter_grid <- grid %>%
#       `[`(which(sf::st_touches(x = sf::st_combine(results), y = grid, sparse = FALSE)[1, ]))
#     # ...and save the rest of the "grid" for later
#     remainder <- grid %>%
#       `[`(which(!sf::st_touches(x = sf::st_combine(results), y = grid, sparse = FALSE)[1, ])) %>%
#       # need to subtract the already "taken" hexes (results) too!
#       sf::st_difference(sf::st_combine(results))
#
#     # just get the centroids of the touching part of the grid
#     grid_centroids <- sf::st_centroid(filter_grid)
#
#     # which is the nearest centroid to the MSOA we're dealing with?
#     nearest_index <- sf::st_nearest_feature(msoa, grid_centroids)
#     nearest_hex <- `[`(filter_grid, nearest_index)
#
#     # update filter_grid to remove the hex we just selected...
#     filter_grid <- `[`(filter_grid, -nearest_index)
#
#     # ...and make a new grid to pass to the next cycle
#     grid_out <- c(filter_grid, remainder)
#
#   # if we haven't yet got any results, do this first:
#   } else {
#     grid_centroids <- sf::st_centroid(grid)
#     nearest_index <- sf::st_nearest_feature(msoa, grid_centroids)
#     nearest_hex <- `[`(grid, nearest_index)
#     grid_out <- `[`(grid, -nearest_index)
#   }
#
#
#
#   # add the recently calculated hex to "results"
#   results <- dplyr::bind_rows(
#     results,
#     sf::st_set_geometry(msoa, nearest_hex)
#   )
#
#   # once the last MSOA in the LAD has been calculated export the results
#   # and the remaining grid...
#   if (nrow(msoas_out) == 0) {
#     results <- dplyr::bind_rows(prev_results, results)
#     list(results, grid_out)
#   } else {
#
#   # otherwise send the intermediate products back through the cycle
#     hex_array2(list(results, grid_out, prev_results), msoas_out, first_pass = FALSE)
#   }
#
#
# }




# older function - kept just in case :-/

# hex_array <- function(input, areas_df, first_pass = TRUE) {
#
#   results <- input[[1]]
#   grid <- input[[2]]
#   prev_results <- input[[3]]
#
#   if (first_pass) {
#     prev_results <- results
#     results <- NULL
#   }
#
#   msoa <- areas_df %>%
#     dplyr::slice(1)
#   msoa_centroid <- sf::st_centroid(msoa)
#   areas_out <- areas_df %>%
#     dplyr::slice(-1)
#   # area_centroids <- sf::st_centroid(areas_out)
#
#
#   grid_centroids <- sf::st_centroid(grid)
#   nearest_index <- sf::st_nearest_feature(msoa_centroid, grid_centroids)
#   nearest_hex <- dplyr::slice(grid, nearest_index)
#   grid <- dplyr::slice(grid, -nearest_index)
#
#
#
#   results <- dplyr::bind_rows(
#     results,
#     dplyr::bind_cols(
#       sf::st_drop_geometry(msoa),
#       hex_geometry = nearest_hex
#     )
#   )
#
#   if (nrow(areas_out) == 0) {
#     results <- dplyr::bind_rows(prev_results, sf::st_sf(results))
#     list(results, grid, NULL)
#   } else {
#     hex_array(list(results, grid, prev_results), areas_out, first_pass = FALSE)
#   }
#
#
# }



# gather_hexes: alternative function to the above?
#
# this was a previous function, replaced by hex_array (which uses the "sorted
# by proximity" set up of msoas_df just to slice off MSOAs one by one, rather
# than using the next_index / n system here)


# gather_hexes <- function(input, areas_df, n = NULL) {
#
#   # previous results of function, if any (hexes and remaining grid)
#   results <- input[[1]]
#   grid <- input[[2]]
#
#   # centroids for all MSOAs
#   area_centroids <- sf::st_centroid(areas_df)
#
#   # not sure need this bit (check)
#   if (is.null(n)) {
#
#     # which MSOA centroid is nearest to the LAD centroid?
#     n <- sf::st_nearest_feature(
#       sf::st_centroid(sf::st_union(areas_df)),
#       area_centroids
#     )
#
#   }
#
#
#
#   # select nearest remaining MSOA
#   msoa <- areas_df %>%
#     dplyr::slice(n)
#
#   # hmmm but we already have popn-weighted centroids so why this?
#   msoa_centroid <- sf::st_centroid(msoa)
#
#   areas_out <- areas_df %>%
#     dplyr::slice(-n)
#   # need to regenerate list of centroids?
#   area_centroids <- sf::st_centroid(areas_out)
#
#
#
#   if (!is.null(results) && any(sf::st_touches(x = grid, y = results, sparse = FALSE)[, 1])) {
#
#     # filter out hexes that are touching results...
#     filter_grid <- grid %>%
#       dplyr::filter(sf::st_touches(x = ., y = results, sparse = FALSE)[, 1])
#     # ...and save the remainder
#     remainder <- grid %>%
#       dplyr::filter(!sf::st_touches(x = ., y = results, sparse = FALSE)[, 1])
#
#
#     grid_centroids <- sf::st_centroid(filter_grid)
#     nearest_index <- sf::st_nearest_feature(msoa_centroid, grid_centroids)
#     nearest_hex <- dplyr::slice(filter_grid, nearest_index)
#     filter_grid <- dplyr::slice(filter_grid, -nearest_index)
#     grid <- dplyr::bind_rows(filter_grid, remainder)
#   } else {
#     grid_centroids <- sf::st_centroid(grid)
#     nearest_index <- sf::st_nearest_feature(msoa_centroid, grid_centroids)
#     nearest_hex <- dplyr::slice(grid, nearest_index)
#     grid <- dplyr::slice(grid, -nearest_index)
#
#   }
#
#
#   # generate new set of results to pass on
#   results <- dplyr::bind_rows(
#     results,
#     dplyr::bind_cols(
#       sf::st_drop_geometry(msoa),
#       hex_geometry = nearest_hex
#     )
#   ) %>%
#     sf::st_sf()
#
#
#
#   # calculate the index to be passed to next cycle as "n" - but is this
#   # right? why am I using msoa-centroid, when this is the one we've just done?
#   next_area_index <- sf::st_nearest_feature(msoa_centroid, area_centroids)
#
#
#
#   # finish when have gone through all MSOAs...
#   if (nrow(areas_out) == 0) {
#     list(results, grid)
#   } else {
#   # ...or send the output data through the cycle again
#     gather_hexes(input = list(results, grid), areas_df = areas_out, n = next_area_index)
#   }
# }

