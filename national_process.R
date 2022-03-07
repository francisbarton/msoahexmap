
# Basic geographies download and setup ------------------------------------



eng_regions <- jogger::geo_get("region", "England", "ctry", spatial_ref = 7405)
myrmidon::save_it(eng_regions)

eng_lads <- region_names %>%
  purrr::map(get_regional_lads) %>%     # list of 9 dfs of LADs with boundaries
  purrr::set_names(region_names)
myrmidon::save_it(eng_lads)

eng_lad_centroids <- eng_lads %>%
  purrr::map(sf::st_centroid) %>%       # list of 9 dfs of LADs with centroids
  purrr::set_names(region_names)
myrmidon::save_it(eng_lad_centroids)

eng_msoas <- eng_lads %>%
  purrr::map(get_msoas) %>%             # list of 9 dfs of MSOAs with bounds
  purrr::set_names(region_names)
myrmidon::save_it(eng_msoas)

eng_msoa_centroids <- region_names %>%
  purrr::map(get_msoa_centroids) %>%    # list of 9 dfs of MSOAs with centroids
  purrr::set_names(region_names)
myrmidon::save_it(eng_msoa_centroids)



# Create sorted list of MSOAs by region -----------------------------------



msoas_sorted_by_proximity_orig <- list(
  eng_lads,
  eng_msoas,
  eng_msoa_centroids
) %>%
  purrr::pmap(
    ~ create_sorted_msoa_batches_by_lad(..1, ..2, ..3)
  ) %>%
  purrr::set_names(region_names)
myrmidon::save_it(msoas_sorted_by_proximity_orig)



# attempt all of England (except London) ----------------------------------

# omit London
regions_bounds <- eng_regions %>%
  dplyr::slice(-7)
london_bounds <- eng_regions %>%
  dplyr::slice(7)

# make a grid of the whole country, given `cell_size` as a variable.
# 43263 hexes
base_grid_full <- sf::st_make_grid(
  eng_regions,
  what = "polygons",
  square = FALSE,
  flat_topped = TRUE,
  cellsize = 3160
)

# crop full grids to actual area boundaries (any hexes that intersect)
# 15797 hexes
base_grid_cropped <- base_grid_full %>%
  sf::st_intersects(sf::st_union(eng_regions), sparse = FALSE) %>%
  which() %>%
  `[`(base_grid_full, .)

saveRDS(base_grid_cropped, here::here("rds_data", "base_grid_cropped.Rds"))



# only 231 hexes for London (983 MSOAs) at this size!
lon_int_index <- base_grid_cropped %>%
  sf::st_intersects(london_bounds, sparse = FALSE)

# 15566 hexes for 5808 non-London MSOAs (of 6791 total in England)
regions_grid_orig <- base_grid_cropped[!lon_int_index]

saveRDS(regions_grid_orig, here::here("rds_data", "regions_grid_orig.Rds"))



# just a quick look
tmap::tmap_mode("view")
tmap::tm_shape(regions_grid_orig) +
  tmap::tm_borders("olivedrab4", lwd = 1, alpha = 0.1) +
  tmap::tm_fill("olivedrab4", alpha = 0.2) +
  tmap::tm_shape(eng_regions) +
  tmap::tm_borders("grey50", lwd = 2, alpha = 0.3)







# Load all the above quickly when restarting ------------------------------



eng_regions <- readRDS(here::here("rds_data", "eng_regions.Rds"))

region_names <- eng_regions %>%
  dplyr::pull(rgn20nm)

eng_lads <- readRDS(here::here("rds_data", "eng_lads.Rds"))


eng_lad_centroids <- readRDS(here::here("rds_data", "eng_lad_centroids.Rds")) %>%
  purrr::map( ~ dplyr::mutate(., label = stringr::str_trunc(ltla21nm, 4, "right", "")))



eng_msoas <- readRDS(here::here("rds_data", "eng_msoas.Rds"))
eng_msoa_centroids <- readRDS(here::here("rds_data", "eng_msoa_centroids.Rds"))
msoas_sorted_by_proximity_orig <- readRDS(here::here("rds_data", "msoas_sorted_by_proximity_orig.Rds"))


base_grid_cropped <- readRDS(here::here("rds_data", "base_grid_cropped.Rds"))
regions_grid_orig <- readRDS(here::here("rds_data", "regions_grid_orig.Rds"))


my_pal <- ggsci::pal_d3("category20")







# Features to protect from base grid --------------------------------------

# Internal regional borders (ie not coastline) ?

# Can use `tmap` view mode to identify hexes to remove for certain features
# - the cell index comes up as a pop-up!
#
# The motivation for doing this is to help with recognition of particular
# features eg the Wirral, Isle of Wight, as distinct areas that otherwise would
# otherwise just be contiguous with neighbouring areas (Liverpool, Portsmouth)
# in the final map. In many places, particularly less dense areas, this is not
# an issue as there is a natural spacing in the map.

# Thames, west to east from edge of London
thames <- c(4206, 4244, 4207, 4245, 4208, 4282, 4316, 4350, 4317, 4351, 4318, 4384, 4417, 4319, 4385) # the last four can be removed if necessary

# the last 3 can be removed if necessary: just to sep Soton from Pompey
solent <- c(902, 949, 1139, 1304, 1429, 1493)

severn <- c(4521, 4593, 4629, 4669, 4712, 4798, 4843)

# northwest to southeast, erring towards the Wirral as north of the Mersey is
# even more dense
mersey <- c(11252, 11212, 11129, 11044, 11002, 10917, 10875, 10833, 10876)

humber <- c(11829, 11866, 11943, 12016, 12053, 12090, 12052, 12014, 11977, 11939, 11976, 12012, 12049, 12011)

# tyne <- c(15009, 14989, 14969)
poole_harbour <- c(819, 858, 898)
medway <- c(4212, 4173, 4131, 4085)

church_stoke <- c(7938, 7999)

peak_edge <- c(11103, 11018, 10933, 10891, 10849, 11622, 11584, 11505, 11466, 11426, 11347)
yh_nw_border <- c(12145, 12069, 11994, 11958, 11921, 11844)

# nuclear option to resist fragmentation in the North West region!
# random_nw <- NULL
random_nw <- c(11094, 12102)

# 15486 hexes of 15566 remaining!
regions_grid <- regions_grid_orig[-c(thames, solent, severn, mersey, humber, poole_harbour, medway, church_stoke, peak_edge, yh_nw_border, random_nw)]

# just another quick peep
tmap::tmap_mode("view")
tmap::tm_shape(regions_grid_orig) +
  tmap::tm_borders("olivedrab4", lwd = 1, alpha = 0.1) +
  tmap::tm_fill("olivedrab4", alpha = 0.2) +
  tmap::tm_shape(eng_regions) +
  tmap::tm_borders("grey50", lwd = 2, alpha = 0.3)




# even though the densest LAD may be Southampton (, Portsmouth, Watford...),
# the densest region overall (outside London, obv) is North West so that is
# what we want to allocate first
regional_densities <- eng_msoas[-7] %>%
  purrr::map(sf::st_drop_geometry) %>%
  purrr::map( ~ dplyr::summarise(., mean_msoa_area = mean(shape_area))) %>%
  purrr::map_dbl( ~ dplyr::pull(., mean_msoa_area)) %>%
  purrr::set_names(names(eng_msoas[-7])) %>%
  sort()

# split and re-order the regional boundaries so can be mapped/reduced
# in parallel with new msoas list
eng_rgns_sorted <- regions_bounds %>%
  split( ~ rgn20nm) %>%
  `[`(names(regional_densities))

# apply manual fixes to help eliminate fragmentation
msoas_density_sorted <- msoas_sorted_by_proximity_orig[names(regional_densities)] %>%
  regional_fixes()


# test North West (densest region) ----------------------------------------


nw_out <- create_hexgrid_national(
  data_inputs = list(
    hexes = NULL,
    lads = NULL,
    grid = regions_grid,
    n = 1
  ),
  area_bounds = eng_rgns_sorted[["North West"]],
  msoas_list = msoas_density_sorted[[1]]
)

saveRDS(nw_out, here::here("rds_data", "nw_out_3160_final.Rds"))


# region 2 test (South East) ----------------------------------------------

nw_out <- readRDS(here::here("rds_data", "nw_out_3160_final.Rds"))

se_out <- create_hexgrid_national(
  data_inputs = nw_out,
  area_bounds = eng_rgns_sorted[[2]],
  msoas_list = msoas_density_sorted[[2]]
)

saveRDS(se_out, here::here("rds_data", "se_out_3160_v2.Rds"))


# region 3 test (West Midlands) -------------------------------------------

se_out <- readRDS(here::here("rds_data", "se_out_3160_v2.Rds"))

wm_out <- create_hexgrid_national(
  data_inputs = se_out,
  area_bounds = eng_rgns_sorted[[3]],
  msoas_list = msoas_density_sorted[[3]]
)

saveRDS(wm_out, here::here("rds_data", "wm_out_3160_v1.Rds"))



# region 4 test (Yorkshire and The Humber) -------------------------------

wm_out <- readRDS(here::here("rds_data", "wm_out_3160_v1.Rds"))

yh_out <- create_hexgrid_national(
  data_inputs = wm_out,
  area_bounds = eng_rgns_sorted[[4]],
  msoas_list = msoas_density_sorted[[4]]
)

saveRDS(yh_out, here::here("rds_data", "yh_out_3160_v1.Rds"))


# region 5 test (North East) -------------------------------

yh_out <- readRDS(here::here("rds_data", "yh_out_3160_v1.Rds"))

ne_out <- create_hexgrid_national(
  data_inputs = yh_out,
  area_bounds = eng_rgns_sorted[[5]],
  msoas_list = msoas_density_sorted[[5]]
)

saveRDS(ne_out, here::here("rds_data", "ne_out_3160_v1.Rds"))


# region 6 test (East of England) -------------------------------

ne_out <- readRDS(here::here("rds_data", "ne_out_3160_v1.Rds"))

ee_out <- create_hexgrid_national(
  data_inputs = ne_out,
  area_bounds = eng_rgns_sorted[[6]],
  msoas_list = msoas_density_sorted[[6]]
)

saveRDS(ee_out, here::here("rds_data", "ee_out_3160_v1.Rds"))

# region 7 test (East Midlands) -------------------------------

ee_out <- readRDS(here::here("rds_data", "ee_out_3160_v1.Rds"))

em_out <- create_hexgrid_national(
  data_inputs = ee_out,
  area_bounds = eng_rgns_sorted[[7]],
  msoas_list = msoas_density_sorted[[7]]
)

saveRDS(em_out, here::here("rds_data", "em_out_3160_v1.Rds"))

# region 8 test (South West) -------------------------------

em_out <- readRDS(here::here("rds_data", "em_out_3160_v1.Rds"))

sw_out <- create_hexgrid_national(
  data_inputs = em_out,
  area_bounds = eng_rgns_sorted[[8]],
  msoas_list = msoas_density_sorted[[8]]
)

results <- sw_out

final_results <- results[1:2]
saveRDS(final_results, here::here("rds_data", "final_results.Rds"))

# https://research.mysociety.org/sites/imd2019/about/
imd_data <- readr::read_csv("imd2019_msoa_level_data.csv") %>%
  janitor::clean_names() %>%
  dplyr::select(
    msoa11cd = msoac,
    imd_decile = msoadecile
  ) %>%
  dplyr::mutate(across(imd_decile, ~ forcats::as_factor(.)))


# view
tmap::tmap_mode("view")
tmap::tm_shape(regions_grid) +
  tmap::tm_borders("olivedrab4", lwd = 1, alpha = 0.1) +
  tmap::tm_fill("olivedrab4", alpha = 0.2) +
  tmap::tm_shape(eng_regions) +
  tmap::tm_borders("grey50", lwd = 2, alpha = 0.3) +
  tmap::tm_shape(results[["hexes"]]) +
  tmap::tm_borders("grey75", lwd = 1, alpha = 0) +
  tmap::tm_fill("orange", alpha = 0.7) +
  tmap::tm_shape(results[["lads"]]) +
  tmap::tm_borders("darkorange1", lwd = 1) +
  # tmap::tm_text("ltla21nm", size = 1) +
  tmap::tm_layout(
    legend.show = FALSE
  )

# plot
tmap::ttm()
tmap::tm_shape(regions_grid_orig) +
  tmap::tm_borders("olivedrab4", lwd = 1, alpha = 0.1) +
  tmap::tm_fill("olivedrab4", alpha = 0.2) +
  tmap::tm_shape(eng_regions) +
  tmap::tm_borders("grey50", lwd = 2, alpha = 0.3) +
  tmap::tm_shape(final_results[["hexes"]]) +
  tmap::tm_borders("grey75", lwd = 1, alpha = 0) +
  tmap::tm_fill("orange", alpha = 0.7) +
  tmap::tm_shape(results[["lads"]]) +
  tmap::tm_borders("grey40", lwd = 1) +
  # tmap::tm_text("ltla21nm", size = 0.5) +
  # tmap::tm_shape(eng_lad_centroids[[2]]) +
  # tmap::tm_symbols("deeppink4", size = 0.1) +
  # tmap::tm_text("label", size = 0.1, case = "upper", col = "grey80") +
  tmap::tm_credits(
    paste0(
      "MSOAs: "
      , nrow(final_results[["hexes"]])
      , "\nAvailable cells: "
      , length(regions_grid)
      , "\nEfficiency: "
      , scales::percent(round(nrow(final_results[["hexes"]])/length(regions_grid), 1))
      , "\nCell height: 3160m"
    ),
    position = c("right", "top"),
    col = "grey15",
    align = "right") +
  tmap::tm_layout(
    title = "National: 3160m",
    scale = 0.8,
    legend.show = FALSE
  )

imd_hexes <- final_results[["hexes"]] %>%
  dplyr::left_join(imd_data)


tiff("imd_regional_hexmap.tiff", width = 12, height = 16, units = "in", pointsize = 14, res = 300)
tmap::tm_shape(eng_regions) +
  tmap::tm_borders("grey50", lwd = 2, alpha = 0.3) +
  tmap::tm_fill("grey60") +
  tmap::tm_shape(imd_hexes) +
  tmap::tm_borders("grey75", lwd = 1, alpha = 0) +
  tmap::tm_fill("imd_decile", palette = "viridis",  alpha = 0.7) +
  tmap::tm_shape(final_results[["lads"]]) +
  tmap::tm_borders("grey90", lwd = 1) +
  tmap::tm_layout(
    title = "IMD Regions Hexmap: hex height 3160m",
    scale = 1,
    legend.show = TRUE
  )
dev.off()

# take each region at a time, and allocate hexes from regions_grid
# names(eng_msoas[-7])
# [1] "North East"               "North West"
# [3] "Yorkshire and The Humber" "East Midlands"
# [5] "West Midlands"            "East of England"
# [7] "South East"               "South West"
all_regions_out <- purrr::reduce2(
  .x = eng_rgns_sorted,
  .y = msoas_density_sorted,
  .f = create_hexgrid_national,
  .init = list(
    hexes = NULL,
    lads = NULL,
    grid = regions_grid,
    n = 1
    )
)










# OLDER STUFF -------------------------------------------------------------








# experiments with London mapping at various cell sizes -------------------

# remove certain features from base grid for london -----------------------


richmond1 <- c(51.4524977042596, -0.27322697210996216)
richmond2 <- c(51.4466560718536, -0.2537146614910833)
richmond3 <- c(51.448205965537966, -0.29388706570642215)
richmond4 <- c(51.43081600128052, -0.2429528470800841)
richmond5 <- c(51.44212531174297, -0.2816440472788903)
richmond6 <- c(51.434612797968065, -0.2632795196375926)
richmond7 <- c(51.43604384823644, -0.2952261458469334)
richmond8 <- c(51.429007419424735, -0.27705291536856586)
richmond9 <- c(51.448484844736996, -0.2929212790996278)
richmond10 <- c(51.41876389282819, -0.27486144790767814)
richmond11 <- c(51.423926918800504, -0.26354636486476496)

hamps_heath1 <- c(51.56048941020072, -0.1594005502982289)
hamps_heath2 <- c(51.566472127628096, -0.1740731696332256)
hamps_heath3 <- c(51.57113124311811, -0.16026415830396332)

hyde_park <- c(51.50281670620354, -0.16202225578049814)

lea_valley0 <- c(51.67855689745908, -0.01074833042258532)
lea_valley1 <- c(51.661844754421196, -0.014907501744059738)
lea_valley2 <- c(51.65044992344299, -0.01741657839895673)
lea_valley3 <- c(51.64574149420728, -0.0191486527166432)
lea_valley4 <- c(51.63903551518596, -0.035827193155750656)
lea_valley5 <- c(51.62597979326484, -0.028214522470058512)
lea_valley6 <- c(51.610605459097016, -0.034997053501505525)
lea_valley7 <- c(51.6044148599233, -0.03458271606139785)
lea_valley8 <- c(51.59849073570559, -0.04527239028646409)
lea_valley9 <- c(51.58632696195899, -0.04916280483313935)
lea_valley10 <- c(51.57240546077568, -0.05678466227568408)
lea_valley11 <- c(51.562018900054724, -0.04579486507029182)
lea_valley12 <- c(51.556388655507554, -0.03682242460153038)

r_thames1 <- c(51.48571166623088, 0.21859945991273425)
r_thames2 <- c(51.484488350090864, 0.20224769839334003)
r_thames3 <- c(51.49299310734655, 0.18230016234327706)
r_thames4 <- c(51.48679962911424, 0.177115749099297)
r_thames5 <- c(51.50333897036761, 0.16795935075433005)
r_thames6 <- c(51.510033800760525, 0.1526232747732284)
r_thames7 <- c(51.51662575488767, 0.13682295662339733)
r_thames8 <- c(51.51892206229701, 0.12388864757011538)
r_thames9 <- c(51.511282440774664, 0.10647088749233394)
r_thames10 <- c(51.51123056423111, 0.09075800183972074)
r_thames11 <- c(51.50574281752859, 0.08200565098642301)
r_thames12 <- c(51.497024559414754, 0.053476275140938764)
r_thames13 <- c(51.49768250255558, 0.04238070375431431)
r_thames14 <- c(51.49714918619705, 0.02459021957158409)
r_thames15 <- c(51.50476650393496, 0.008690988365657926)
r_thames16 <- c(51.48951586177533, -0.00110777632702963)
r_thames17 <- c(51.49917519429207, -0.003965749362396844)
r_thames18 <- c(51.48544816204193, -0.014581077779475022)
r_thames19 <- c(51.49008471818716, -0.03657192722763131)

barking1 <- c(51.52416749767683, 0.09292469869991689)
barking2 <- c(51.529463674100946, 0.07771657424696718)
barking3 <- c(51.535475741406174, 0.07221857304297616)


lon_points <- list(
  # richmond1, # northernmost hex of original 7
  # richmond2, # "north-east" hex
  # richmond3, # "north-west" hex
  richmond4,
  richmond5,
  richmond6,
  richmond7,
  richmond8,
  richmond9,
  # richmond10,
  richmond11,
  hamps_heath1,
  hamps_heath2,
  hamps_heath3,
  # hyde_park,
  lea_valley0,
  lea_valley1,
  lea_valley2,
  lea_valley3, # 10
  lea_valley4,
  lea_valley5,
  lea_valley6,
  lea_valley7,
  lea_valley8,
  # lea_valley9,
  # lea_valley10,
  # lea_valley11,
  # lea_valley12,
  r_thames1,
  r_thames2,
  r_thames3,
  r_thames4, # 20
  r_thames5,
  r_thames6,
  r_thames7,
  r_thames8,
  r_thames9, # 25
  r_thames10,
  r_thames11,
  r_thames12,
  r_thames13,
  r_thames14, # 30
  r_thames15,
  r_thames16,
  r_thames17,
  r_thames18,
  r_thames19, # 35
  barking1,
  barking2,
  barking3
)

lon_points_sf <- lon_points %>%
  purrr::map(rev) %>%
  purrr::map(sf::st_point) %>%
  sf::st_sfc(crs = 4326) %>%
  sf::st_transform(crs = 7405)


lon_points_nearest_1264 <- lon_points_sf %>%
  sf::st_nearest_feature(., london_grid_cropped_1264)

# datapasta::vector_paste(sort(lon_points_nearest_1264))
lon_points_nearest_1264 <- c(302L, 321L, 322L, 341L, 342L, 362L, 382L, 553L, 560L, 575L, 576L, 582L, 583L, 601L, 622L, 623L, 624L, 628L, 646L, 648L, 651L, 672L, 674L, 697L, 698L, 721L, 722L, 745L, 795L, 865L, 891L, 918L, 1050L, 1076L, 1126L, 1167L, 1200L, 1212L, 1230L, 1244L, 1256L)

# test lon_points_nearest_1264
tmap::ttm()
tmap::tm_shape(sf::st_union(eng_msoas[[7]])) +
  tmap::tm_borders("grey70", lwd = 2) +
  tmap::tm_shape(london_grid_cropped_1264) +
  tmap::tm_borders("olivedrab4", lwd = 1) +
  tmap::tm_shape(london_grid_cropped_1264[lon_points_nearest_1264]) +
  tmap::tm_fill("lightseagreen")




# Final optimum cell size is 1264m
# Final thing will need to be expanded by x2.5
# across and x2.5 height (x6.25 area) ... a cell of 1383645m^2 area becomes
# 8647783m^2 area to match the rest of the country
# (3^0.5)*(1264^2)/2 = 1383645
# (3^0.5)*(3160^2)/2 = 8647783

lon_1264 <- create_hexgrid(
  area_bounds = eng_msoas[[7]],
  msoas_list = msoas_sorted_by_proximity[[7]],
  cell_size = 1264,
  compact = 2,
  omit_cells = lon_points_nearest_1264
)
saveRDS(lon_1264, here::here("rds_data", "lon_1264_final_maybe.Rds"))


results <- lon_1264
records <- dplyr::tribble(
  ~rgn20nm, ~cell_size, ~efficiency, ~median_accuracy, ~mean_accuracy, ~compact,
  eng_regions$rgn20nm[7], results[["cell_size"]], results[["efficiency"]], results[["median_accuracy"]], results[["mean_accuracy"]], 2
) %>%
  dplyr::mutate(score_median = round(1000*efficiency/median_accuracy, 2)) %>%
  dplyr::mutate(score_mean = round(1000*efficiency/mean_accuracy, 2)) %>%
  dplyr::bind_rows(records, .) %>%
  readr::write_csv("records.csv")



tmap::ttm()
tmap::tm_shape(results[["grid"]]) +
  tmap::tm_borders("olivedrab4", lwd = 1, alpha = 0.1) +
  tmap::tm_fill("olivedrab4", alpha = 0.2) +
  tmap::tm_shape(sf::st_union(results[["bounds"]])) +
  tmap::tm_borders("grey50", lwd = 2, alpha = 0.1) +
  tmap::tm_shape(results[["hexes"]]) +
  # tmap::tm_borders("grey75", lwd = 1) +
  # tmap::tm_fill("lad20nm", alpha = 0.7, palette = my_pal(20)) +
  tmap::tm_fill("orange", alpha = 0.7) +
  # tmap::tm_fill("utla21nm", palette = my_pal(length(unique(eng_msoa_centroids[[7]]$utla21nm)))) +
  tmap::tm_shape(results[["lads"]]) +
  tmap::tm_borders("grey40", lwd = 1) +
  tmap::tm_text("ltla21nm", size = 0.3) +
  tmap::tm_shape(eng_lad_centroids[[7]]) +
  tmap::tm_dots("deeppink4", size = 0.05) +
  tmap::tm_credits(
    paste0(
      "MSOAs: "
      , nrow(results[["bounds"]])
      , "\nAvailable cells: "
      , length(results[["grid"]])
      , "\nEfficiency: "
      , results[["efficiency"]]
      , "%\nCell height: "
      , results[["cell_size"]]
      , "m\nMedian accuracy: "
      , results[["median_accuracy"]]
      , "m\nMean accuracy: "
      , results[["mean_accuracy"]]
      , "m"
    ),
    position = c("right", "bottom"),
    col = "grey15",
    align = "right") +
  tmap::tm_layout(
    scale = 2,
    title = paste(eng_regions$rgn20nm[7], results[["cell_size"]]),
    title.color = "grey15",
    title.fontface = 2,
    legend.show = FALSE
  )




















# previous regional experiments ... to see what happens...! ---------------


# obsolete: try calculating ideal hex sizes -------------------------------



hex_sizes40 <- eng_msoas %>%
  purrr::map_df(~ calculate_hex_size(., efficiency = 40)) %>%
  dplyr::bind_cols(region = names(eng_msoas), .)
hex_sizes33 <- eng_msoas %>%
  purrr::map_df(~ calculate_hex_size(., efficiency = 33)) %>%
  dplyr::bind_cols(region = names(eng_msoas), .)

hex_sizes <- dplyr::bind_rows(hex_sizes40, hex_sizes33)



# experiments with regional mapping at various cell sizes -----------------



# North East
ne_out3000 <- create_hexgrid(
  area_bounds = eng_msoas[[1]],                # with polygons
  msoas_list = msoas_sorted_by_proximity[[1]], # with centroids
  cell_size = 3000,
  compact = FALSE
)
saveRDS(ne_out3000, here::here("rds_data", "ne_out3000_diffuse.Rds"))



# West Midlands
wm_out3000 <- create_hexgrid(
  area_bounds = eng_msoas[[5]],                # with polygons
  msoas_list = msoas_sorted_by_proximity[[5]], # with centroids
  cell_size = 3000
)


# East Midlands
em3000 <- create_hexgrid(
  area_bounds = eng_msoas[[4]],                # with polygons
  msoas_list = msoas_sorted_by_proximity[[4]], # with centroids
  cell_size = 3000
)
saveRDS(em3000, here::here("rds_data", "em3000_compact.Rds"))


# North West

nw_3000 <- create_hexgrid(
  area_bounds = eng_msoas[[2]],                # with polygons
  msoas_list = msoas_sorted_by_proximity[[2]], # with centroids
  cell_size = 3000
)
saveRDS(nw_3000, here::here("rds_data", "nw_3000_compact.Rds"))


# South West

sw_out3000 <- create_hexgrid(
  area_bounds = eng_msoas[[9]],                # with polygons
  msoas_list = msoas_sorted_by_proximity[[9]], # with centroids
  cell_size = 3000
)
saveRDS(sw_out3000, here::here("rds_data", "sw_out3000b.Rds"))



# South East

se_3000 <- create_hexgrid(
  area_bounds = eng_msoas[[8]],                # with polygons
  msoas_list = msoas_sorted_by_proximity[[8]], # with centroids
  cell_size = 3000
)
saveRDS(se_3000, here::here("rds_data", "se_3000.Rds"))



# Yorkshire and the Humber

yh_3000 <- create_hexgrid(
  area_bounds = eng_msoas[[3]],                # with polygons
  msoas_list = msoas_sorted_by_proximity[[3]], # with centroids
  cell_size = 3000
)
saveRDS(yh_3000, here::here("rds_data", "yh_3000_diffuse.Rds"))


# East of England

ee_3000 <- create_hexgrid(
  area_bounds = eng_msoas[[6]],                # with polygons
  msoas_list = msoas_sorted_by_proximity[[6]], # with centroids
  cell_size = 3000
)
saveRDS(ee_3000, here::here("rds_data", "ee_3000_compact.Rds"))


# West Midlands

wm_3000 <- create_hexgrid(
  area_bounds = eng_msoas[[5]],                # with polygons
  msoas_list = msoas_sorted_by_proximity[[5]], # with centroids
  cell_size = 3000
)
saveRDS(wm_3000, here::here("rds_data", "wm_3000_compact.Rds"))








########################################################################
# superseded: batch process, was a bit weird


# use the hex_array() function to create results for each LAD given a
# regional grid as a starting point...
# ... this works through each LAD's list of MSOAs iteratively and
# uses `reduce` to produce a neat set of results

# map in batches of 20 ----------------------------------------------------



current_lads <- remainder_lads_density_sorted %>%
  head(276) %>%
  tail(16)

results <- current_lads %>%
  purrr::reduce(
    hex_array,
    .init = list(NULL, remaining_grid)
  )

msoa_hexes <- results %>%
  purrr::pluck(1)

msoa_hexes_276 <- msoa_hexes
saveRDS(msoa_hexes_276, here::here("rds_data", "msoa_hexes_276.Rds"))


remaining_grid <- results %>%
  purrr::pluck(2)

hex_centroids <- msoa_hexes %>%
  sf::st_centroid() %>%
  split( ~ msoa11cd)

msoa_centroids <- current_lads %>%
  purrr::reduce(dplyr::bind_rows) %>%
  split( ~ msoa11cd)

distances <- purrr::map2_dbl(
  hex_centroids,
  msoa_centroids,
  ~ sf::st_distance(.x, .y)
)

median_accuracy <- distances %>%
  stats::median() %>%
  round()

mean_accuracy <- distances %>%
  mean() %>%
  round()

current_msoas <- england_bounds %>%
  dplyr::filter(ltla21nm %in% names(current_lads))
current_lads_grid <- base_grid_cropped %>%
  sf::st_intersects(sf::st_union(current_msoas), sparse = FALSE) %>%
  which()

efficiency <- (100 * length(hex_centroids)/length(current_lads_grid)) %>%
  round(1)


records <- dplyr::tribble(
  ~batch, ~msoas, ~median_accuracy, ~mean_accuracy, ~efficiency,
  "261:276", nrow(current_msoas), median_accuracy, mean_accuracy, efficiency
) %>%
  dplyr::bind_rows(records, .) %>%
  readr::write_csv("england_batch_records2.csv")


lad_hex_groups <- list(msoa_hexes_20, msoa_hexes_40, msoa_hexes_60, msoa_hexes_80, msoa_hexes_100, msoa_hexes_120, msoa_hexes_140, msoa_hexes_160, msoa_hexes_180, msoa_hexes_200, msoa_hexes_220, msoa_hexes_240, msoa_hexes_260, msoa_hexes_276) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  dplyr::group_by(ltla21cd, ltla21nm) %>%
  dplyr::summarise(.groups = "drop")


tmap::tm_shape(regions_grid) +
  tmap::tm_borders("olivedrab4", lwd = 1, alpha = 0.1) +
  tmap::tm_fill("olivedrab4", alpha = 0.2) +
  tmap::tm_shape(eng_regions) +
  tmap::tm_borders("grey50", lwd = 2, alpha = 0.3) +
  tmap::tm_shape(msoa_hexes_20) +
  tmap::tm_fill("grey80") +
  tmap::tm_shape(msoa_hexes_40) +
  tmap::tm_fill("grey80") +
  tmap::tm_shape(msoa_hexes_60) +
  tmap::tm_fill("grey80") +
  tmap::tm_shape(msoa_hexes_80) +
  tmap::tm_fill("grey80") +
  tmap::tm_shape(msoa_hexes_100) +
  tmap::tm_fill("grey80") +
  tmap::tm_shape(msoa_hexes_120) +
  tmap::tm_fill("grey80") +
  tmap::tm_shape(msoa_hexes_140) +
  tmap::tm_fill("grey80") +
  tmap::tm_shape(msoa_hexes_160) +
  tmap::tm_fill("grey80") +
  tmap::tm_shape(msoa_hexes_180) +
  tmap::tm_fill("grey80") +
  tmap::tm_shape(msoa_hexes_200) +
  tmap::tm_fill("grey80") +
  tmap::tm_shape(msoa_hexes_220) +
  tmap::tm_fill("grey80") +
  tmap::tm_shape(msoa_hexes_240) +
  tmap::tm_fill("grey80") +
  tmap::tm_shape(msoa_hexes_260) +
  tmap::tm_fill("grey80") +
  tmap::tm_shape(msoa_hexes_276) +
  tmap::tm_fill("ltla21nm", palette = my_pal(20)) +
  tmap::tm_shape(sf::st_union(london_grid_cropped)) +
  tmap::tm_borders("grey40", lwd = 2) +
  tmap::tm_shape(lad_hex_groups) +
  tmap::tm_borders("grey40", lwd = 1) +
  # tmap::tm_text("ltla21nm", size = 0.5) +
  # tmap::tm_shape(eng_lad_centroids[[7]]) +
  # tmap::tm_dots("deeppink4", size = 0.15) +
  tmap::tm_credits(
    paste0(
      "This batch of 16 LADs\n",
      "MSOAs: "
      , nrow(msoa_hexes)
      , "\nAvailable cells: "
      , length(remaining_grid)
      , "\nLADs area cells: "
      , length(current_lads_grid)
      , "\nEfficiency: "
      , efficiency
      , "%\nCell height: 3000m"
      , "\nMedian accuracy: "
      , median_accuracy
      , "m\nMean accuracy: "
      , mean_accuracy
      , "m"
    ),
    position = c("right", "top"),
    col = "grey15",
    align = "right") +
  tmap::tm_layout(
    scale = 0.67,
    title = "England MSOA hexmap\nAll 276 LADs",
    title.color = "grey15",
    title.fontface = 2,
    legend.show = FALSE
  )


# end results of batch process:

remainder_msoa_hexes <- list(msoa_hexes_20, msoa_hexes_40, msoa_hexes_60, msoa_hexes_80, msoa_hexes_100, msoa_hexes_120, msoa_hexes_140, msoa_hexes_160, msoa_hexes_180, msoa_hexes_200, msoa_hexes_220, msoa_hexes_240, msoa_hexes_260, msoa_hexes_276) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  dplyr::left_join(remainder_lads_density) %>%
  dplyr::relocate(geometry, .after = last_col())

remainder_lad_hex_groups <- list(msoa_hexes_20, msoa_hexes_40, msoa_hexes_60, msoa_hexes_80, msoa_hexes_100, msoa_hexes_120, msoa_hexes_140, msoa_hexes_160, msoa_hexes_180, msoa_hexes_200, msoa_hexes_220, msoa_hexes_240, msoa_hexes_260, msoa_hexes_276) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  dplyr::group_by(ltla21cd, ltla21nm) %>%
  dplyr::summarise(.groups = "drop")
