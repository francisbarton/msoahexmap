
# libraries ---------------------------------------------------------------
{
  library(here)
  library(dplyr)
  library(extrafont)
  library(jogger)
  library(myrmidon) # remotes::install_github("francisbarton/myrmidon")
  library(purrr)
  library(sf)
  library(tmap)
  # library(wesanderson)
  library(ggsci)
}


tmap_mode("plot")

tmap_options(fontfamily = "Source Sans Pro",
             output.format = "tiff",
             output.size = 98)


# my_pal <- ggsci::pal_futurama()
my_pal <- ggsci::pal_d3("category20")


heatmap_data <- readr::read_csv(here("data/eng_msoa_data_202101.csv"))


source(here("R/misc_functions.R"))

# get some data to start from ---------------------------------------------

# weird jogger bug https://github.com/francisbarton/jogger/issues/2
eng_regions <- jogger::geo_get("region", "England", "ctry", return_boundaries = FALSE, return_style = "simple") %>%
  dplyr::arrange(rgn20nm)



southwest_lads <- get_current_region(eng_regions, 7) %>%
  get_regional_lads()
myrmidon::save_it(southwest_lads)

# southwest_lads <- readRDS(here::here("rds_data", "southwest_lads.Rds"))


sw_lads_map <- tm_shape(southwest_lads) +
  tm_borders("grey33", lwd = 1)
sw_lads_map
tmap_save(sw_lads_map, "sw_lads_map.png")


sw_msoas <- get_msoas(southwest_lads)
save_it(sw_msoas)

# sw_msoas <- readRDS(here::here("rds_data", "sw_msoas.Rds"))

sw_msoas_map <- tm_shape(sw_msoas) +
  tm_borders("grey33", lwd = 1)
sw_msoas_map
tmap_save(sw_msoas_map, "sw_msoas_map.png")

sw_msoas_heatmap <- heatmap_data %>%
  dplyr::filter(rgn20nm == "South West") %>%
  dplyr::select(c(msoa11cd, last_col())) %>%
  dplyr::rename(risk_decile = 2) %>%
  dplyr::mutate(across(2, as.factor)) %>%
  dplyr::left_join(sw_msoas, .) %>%
  tmap::tm_shape() +
  tm_borders("grey66", lwd = 1) +
  tm_fill("risk_decile", palette = "viridis")


sw_msoas_heatmap
tmap_save(sw_msoas_heatmap, "sw_msoas_heatmap.png")

# instead of using sf::st_centroid to calculate geometric centroids,
# use ONS population-weighted centroids:

# weirdly doesn't work (only returned 427 MSOAs out of 700 in SW region)
# get_msoa_centroids <- function(region) {
#   jogger::geo_get("msoa", region, "rgn", return_centroids = TRUE, return_style = "simple", spatial_ref = 7405)
# }
#
# sw_msoa_centroids <- get_current_region(7) %>%
#   get_msoa_centroids()

# try this way instead:
sw_msoa_centroids <- sw_msoas %>%
  pull(msoa11cd) %>%
  jogger::geo_get_bounds(
    bounds_query_level = "msoa",
    area_codes = .,
    spatial_ref = 7405,
    centroid_fields = FALSE,
    shape_fields = FALSE,
    return_centroids = TRUE,
    quiet_read = TRUE
  )


# the above doesn't include LAD columns, so use a join to add these in:
sw_msoa_centroids <- sw_msoas %>%
  sf::st_drop_geometry() %>%
  dplyr::select(starts_with(c("msoa", "lad"))) %>%
  dplyr::left_join(sw_msoa_centroids, .) %>%
  sf::st_transform(7405)
save_it(sw_msoa_centroids)

# sw_msoa_centroids <- readRDS(here::here("rds_data", "sw_msoa_centroids.Rds"))



# Calculate the spatial centre of each LAD
sw_lad_centroids <- southwest_lads %>%
  split( ~ lad20nm) %>%
  purrr::map(sf::st_centroid)

source(here("R/make_regional_grid.R"))
sw_grid <- make_regional_grid(southwest_lads, 5000)
# sw_grid_centroids <- sf::st_centroid(sw_grid)


sw_grid1 <- tm_shape(southwest_lads) +
  tm_borders("grey33", lwd = 2) +
  tm_shape(sw_grid) +
  tm_borders("green", lwd = 1) +
  tm_shape(sw_msoa_centroids) +
  tm_dots(col = "orange", size = 0.1)
sw_grid1
tmap_save(sw_grid1, "sw_grid1.png")


# create the MSOA data ready for the hex mapping process:
sw_batched_msoas <- create_sorted_msoa_batches_by_lad(
  sw_msoa_centroids,
  sw_lad_centroids,
  sw_msoas)

myrmidon::save_it(sw_batched_msoas)

# sw_batched_msoas <- readRDS(here::here("rds_data", "sw_batched_msoas.Rds"))

source(here("R/hexmap_creation_helpers.R"))

sw_hexgrid <- create_hexgrid(sw_msoas, sw_batched_msoas, 5000)

saveRDS(sw_hexgrid, here::here("rds_data", "sw_hexgrid.Rds"))


sw_map3
tmap_save(sw_map3, "sw_map3_5000.tiff")

sw_map3 <- tm_shape(sw_hexgrid[[1]]) + # base grid
  tm_borders("olivedrab4", lwd = 1, alpha = 0.1) +
  tm_fill("grey65", alpha = 0.1) +
  tm_shape(sw_hexgrid[[2]] %>% # MSOA hexes
             dplyr::left_join(
               heatmap_data %>%
                 dplyr::filter(rgn20nm == "South West") %>%
                 dplyr::select(c(msoa11cd, last_col())) %>%
                 dplyr::rename(risk_decile = 2) %>%
                 dplyr::mutate(across(2, as.factor))
             ) %>%
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
  tm_borders("grey66", lwd = 1) +
  # tm_fill("lad20nm", alpha = 0.7, palette = my_pal(12)) +
  tm_fill("risk_decile", palette = "viridis") +
  # tm_text("label", size = 0.5) +
  tm_shape(southwest_lads) +
  tm_borders("grey33", lwd = 2, alpha = 0.6) +
  tm_layout(
    title = paste0(
    "Cell size = "
    , 5000
    , "m\nMSOAs in area: "
    , nrow(sw_hexgrid[[2]])
    # , "\nGrid cells: "
    # , length(sw_hexgrid[[1]])
  ),
  legend.show = FALSE)



sw_hexmap <- create_hexmap(sw_msoas, sw_batched_msoas, 4333)
sw_hexmap
tmap_save(sw_hexmap, "sw_hexmap_4333.tiff")


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


