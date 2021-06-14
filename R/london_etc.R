london_lads <- get_current_region(eng_regions, 3) %>%
  get_regional_lads()
myrmidon::save_it(london_lads)

london_msoas <- get_msoas(london_lads)
save_it(london_msoas)

# london_msoas <- readRDS(here::here("rds_data", "london_msoas.Rds"))




london_msoas_map <- tm_shape(london_msoas) +
  tm_borders("grey33", lwd = 1)
london_msoas_map
tmap_save(london_msoas_map, "london_msoas_map.png")

london_msoas_heatmap <- heatmap_data %>%
  dplyr::filter(rgn20nm == "London") %>%
  dplyr::select(c(msoa11cd, last_col())) %>%
  dplyr::rename(risk_decile = 2) %>%
  dplyr::mutate(across(2, as.factor)) %>%
  dplyr::left_join(london_msoas, .) %>%
  tmap::tm_shape() +
  tm_borders("grey66", lwd = 1) +
  tm_fill("risk_decile", palette = "viridis")

essex_bounds <- readRDS(here::here("rds_data", "essex_bounds.Rds"))

essex_msoas_heatmap <- heatmap_data %>%
  dplyr::filter(utla20nm == "Essex") %>%
  dplyr::select(c(msoa11cd, last_col())) %>%
  dplyr::rename(risk_decile = 2) %>%
  dplyr::mutate(across(2, as.factor)) %>%
  dplyr::left_join(essex_bounds, .) %>%
  tmap::tm_shape() +
  tm_borders("grey66", lwd = 1) +
  tm_fill("risk_decile", palette = "viridis")

essex_msoas_heatmap

london_msoas_heatmap
tmap_save(london_msoas_heatmap, "london_msoas_heatmap.png")


london_msoa_centroids <- get_current_region(eng_regions, 3) %>%
  get_msoa_centroids()
save_it(london_msoa_centroids)

