

get_current_region <- function(regions_list, n) {
  regions_list %>%
    slice(n) %>%
    pull(rgn20nm)
}


get_regional_lads <- function(region) {
  jogger::geo_get("lad", region, "rgn", return_style = "minimal", spatial_ref = 7405)
}


get_msoas <- function(lads_list) {
  lads_list %>%
    dplyr::pull(lad20nm) %>%
    purrr::map_df(~ geo_get("msoa", ., "lad", shape_fields = TRUE, spatial_ref = 7405)) %>%
    dplyr::relocate(c(shape_area, shape_length), .before = last_col())
}


get_msoa_centroids <- function(region) {
  jogger::geo_get("msoa", region, "rgn", return_centroids = TRUE, return_style = "minimal", spatial_ref = 7405)
}
