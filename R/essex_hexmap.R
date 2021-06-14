
# libraries ---------------------------------------------------------------
{
  library(here)
  library(dplyr)
  library(extrafont)
  library(ggsci)
  library(jogger)   # remotes::install_github("francisbarton/jogger")
  library(myrmidon) # remotes::install_github("francisbarton/myrmidon")
  library(purrr)
  library(sf)
  library(tmap)
  # library(wesanderson)
}


tmap_mode("plot")

tmap_options(fontfamily = "Source Sans Pro",
             output.format = "tiff",
             output.size = 98)


# my_pal <- ggsci::pal_futurama()
my_pal <- ggsci::pal_d3("category20")


heatmap_data <- readr::read_csv(here("data/eng_msoa_data_202101.csv"))


# get some data to start from ---------------------------------------------


essex_lads <- jogger::geo_get("lad", "Essex", "cty", return_style = "minimal", spatial_ref = 7405)
save_it(essex_lads)

# essex_lads <- readRDS(here::here("rds_data", "essex_lads.Rds"))


essex_bounds <- essex_lads %>%
  dplyr::pull(lad20nm) %>%
  purrr::map_df(~ geo_get("msoa", ., "lad", shape_fields = TRUE, spatial_ref = 7405)) %>%
  dplyr::relocate(c(shape_area, shape_length), .before = last_col())
save_it(essex_bounds)

# essex_bounds <- readRDS(here::here("rds_data", "essex_bounds.Rds"))


essex1 <- tm_shape(essex_bounds) +
  tm_borders("grey33", lwd = 2)
essex1
tmap_save(essex1, "essex1.png")


# instead of using sf::st_centroid to calculate geometric centroids,
# use ONS population-weighted centroids:
essex_centroids <- jogger::geo_get("msoa", "Essex", "cty", return_centroids = TRUE, return_style = "minimal", spatial_ref = 7405)

# the above doesn't include LAD columns, so use a join to add these in:
essex_centroids <- essex_bounds %>%
  sf::st_drop_geometry() %>%
  dplyr::select(starts_with(c("msoa", "lad"))) %>%
  dplyr::left_join(essex_centroids, .)
save_it(essex_centroids)

# essex_centroids <- readRDS(here::here("rds_data", "essex_centroids.Rds"))


# Create a list of MSOAs (with centroid geometry) for each LAD
essex_centroids_split <- essex_centroids %>%
  split( ~ lad20nm) # R 4.1.0 style!

# Calculate the spatial centre of each LAD
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
  purrr::map2(., essex_centroids_split, ~ dplyr::bind_cols(.y, lad_ctr_proximity = .x)) %>%

  # and then sort each df by proximity of the MSOAs
  purrr::map(~ arrange(., lad_ctr_proximity))



sort_by_init_proximity <- function(dtf) {

  top <- dplyr::slice(dtf, 1)

  dists <- dtf %>%
    split( ~ msoa11cd) %>%
    purrr::map_dbl( ~ sf::st_distance(., top)) %>%
    as.vector()

  dplyr::bind_cols(dtf, top_proximity = dists) %>%
    dplyr::arrange(top_proximity)

}


essex_msoas_by_proximity <- essex_lads_by_centroid_proximity %>%
  purrr::map(sort_by_init_proximity)



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
  `[`(essex_msoas_by_proximity, .)




my_pal <- ggsci::pal_futurama()
my_pal <- ggsci::pal_d3("category20")

essex_hexmap <- create_hexmap(essex_bounds, essex_lads_by_density, 4333)
essex_hexmap

tmap_save(essex_hexmap, "essex_hexmap_4333.tiff")




# FUNCTIONS TIME! ---------------------------------------------------------


# create_hexmap: combining create_grid and tmapping  ----------------------


create_hexmap <- function(msoa_bounds, lads_list, cell_size) {

  grid_fill <- create_hexgrid(msoa_bounds, lads_list, cell_size)

  tm_shape(grid_fill[[1]]) + # base grid
    tm_borders("olivedrab4", lwd = 1, alpha = 0.1) +
    tm_fill("grey65", alpha = 0.3) +
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
    tm_shape(sf::st_union(msoa_bounds)) + # county boundary
    tm_borders("gold1", lwd = 2) +
    tm_layout(title = paste0(
      "Cell size = "
      , cell_size
      , "m\nMSOAs in Essex: "
      , nrow(grid_fill[[2]])
      , "\nGrid cells: "
      , length(grid_fill[[1]])
      , "\nUsage: "
      , round(nrow(grid_fill[[2]])*100/length(grid_fill[[1]]))
      , "%"
    )
    , legend.show = FALSE)

}

# create_hexgrid: create hex blocks for each lad -----------------------------


create_hexgrid <- function(area_bounds, msoas_list, cell_size) {

  # make a grid of the whole area, given cell_size as a variable
  grid <- sf::st_make_grid(
    area_bounds,
    what = "polygons",
    square = FALSE,
    flat_topped = TRUE,
    cellsize = cell_size
  )

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

  # return:
  list(grid, hexes)
}



# hex_array: the guts of create_hexgrid --------------------------------------


hex_array <- function(data_inputs, msoas_list, first_pass = TRUE) {

  # split out data_inputs (data to work with) as the basis for the function
  prev_results <- data_inputs[[1]]
  grid <- data_inputs[[2]]

  lad_hexes <- msoas_list %>%
    split( ~ msoa11cd) %>%
    purrr::reduce(collect_hexes, .init = list(NULL, grid))

  # add results to stored previous results
  results <- dplyr::bind_rows(prev_results, lad_hexes[[1]])

  # return results so far and remaining grid:
  list(results, lad_hexes[[2]])

}


# find_hexes: engine func of hex_array ------------------------------------
# (refactored out, for clarity)


collect_hexes <- function(data_inputs, msoa) {

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

    # if this is the first loop, or there's no touching hexes available:
  } else {
    grid_centroids <- sf::st_centroid(grid)
    nearest_index <- sf::st_nearest_feature(msoa, grid_centroids)
    nearest_hex <- sf::st_set_geometry(msoa, `[`(grid, nearest_index))
  }


  # add the chosen hex to "results"
  results <- dplyr::bind_rows(results, nearest_hex)

  # and subtract it from the grid
  grid_out <- sf::st_difference(grid, nearest_hex)

  # return
  list(results, grid_out)

}
