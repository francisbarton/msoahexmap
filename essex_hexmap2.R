{
  library(here)
  library(dplyr)
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



essex_msoas <- readRDS(here::here("rds_data", "essex_msoas.Rds"))


# essex_msoa_centroids <- essex_msoas %>%
#   dplyr::pull(msoa11cd) %>%
#   jogger::geo_get_bounds(
#     bounds_query_level = "msoa",
#     area_codes = .,
#     spatial_ref = 7405,
#     centroid_fields = FALSE,
#     shape_fields = FALSE,
#     return_centroids = TRUE,
#     quiet_read = TRUE
#   )
#
# essex_msoa_centroids <- essex_msoas %>%
#   sf::st_drop_geometry() %>%
#   dplyr::select(starts_with(c("msoa", "lad"))) %>%
#   dplyr::left_join(essex_msoa_centroids, .) %>%
#   sf::st_transform(7405)
# save_it(essex_msoa_centroids)


essex_msoa_centroids <- readRDS(here::here("rds_data", "essex_msoa_centroids.Rds"))
essex_lads <- readRDS(here::here("rds_data", "essex_lads.Rds"))


heatmap_data <- readr::read_csv(here("data/eng_msoa_data_202101.csv"))
essex_heatmap_data <- heatmap_data %>%
  dplyr::filter(utla20nm == "Essex") %>%
  dplyr::select(c(msoa11cd, last_col())) %>%
  dplyr::rename(risk_decile = 2) %>%
  dplyr::mutate(across(2, as.factor))


essex_msoas_heatmap <- essex_heatmap_data %>%
  dplyr::left_join(essex_bounds, .) %>%
  tmap::tm_shape() +
  tm_borders("grey66", lwd = 1) +
  tm_fill("risk_decile", palette = "viridis")


essex_msoas_heatmap

tmap_save(essex_msoas_heatmap, here("essex_msoas_heatmap.tiff"))

# Calculate the spatial centre of each LAD
essex_lad_centroids <- essex_lads %>%
  split( ~ lad20nm) %>%
  purrr::map_df(sf::st_centroid)


essex_batched_msoas <- create_sorted_msoa_batches_by_lad(
  essex_msoa_centroids,
  essex_lad_centroids,
  essex_msoas)
myrmidon::save_it(essex_batched_msoas)



essex_grid <- make_regional_grid(essex_lads, 4333)
essex_hexgrid <- create_hexgrid(essex_msoas, essex_batched_msoas, 4333)

save_it(essex_hexgrid)
essex_hexgrid <- readRDS(here::here("rds_data", "essex_hexgrid.Rds"))

essex_hexgrid_lads <- essex_hexgrid[[2]] %>%
  dplyr::select(1:5) %>%
  dplyr::group_by(lad20cd, lad20nm) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::ungroup()
save_it(essex_hexgrid_lads)

essex_hexgrid_heatmap <- essex_hexgrid[[2]] %>%
  dplyr::select(1:5) %>%
  dplyr::left_join(essex_heatmap_data)


essex_hexgrid_heatmap_out1 <- tm_shape(essex_grid) +
  tm_borders("olivedrab4", lwd = 1, alpha = 0) +
  # tm_fill("grey65", alpha = 0.1) +


  tm_shape(essex_hexgrid_heatmap) +
  tm_borders("white", lwd = 1) +
  tm_fill("risk_decile", palette = "viridis", title = "Risk decile") +

  tm_shape(essex_hexgrid_lads) +
  tm_borders("violetred1", lwd = 5) +

  tm_shape(essex_hexgrid_lads) +
  tm_borders("white", lwd = 1) +
  tm_text("lad20nm", col = "white", bg.color = "grey33", bg.alpha = 0.8, fontface = 2, shadow = TRUE, size = 0.75) +

  tm_shape(essex_lads) +
  tm_borders("grey50", lwd = 2, alpha = 0.2) +

  tm_layout(
    # title = "Digital Exclusion risk by MSOA, Essex",
    # title.color = "white",
    # title.bg.color = "grey20",
    # legend.show = FALSE,
    legend.position = c("right", "bottom"),
    legend.frame = TRUE,
    frame = FALSE
  )
essex_hexgrid_heatmap_out1
tmap_save(essex_hexgrid_heatmap_out1, here("essex_hexgrid_heatmap_out2.tiff"))
