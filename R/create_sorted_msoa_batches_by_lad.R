#' Create sorted MSOA batches
#'
#' Uses helper functions sort_by_init_proximity() and
#' sort_lads_list_by_density() to create the grouped and sorted MSOA lists
#' ready for the hex calculation process
#'
#' @param lads_sf sf tibble of LADs (with boundary geometries)
#' @param msoas_sf sf tibble of MSOAs (with boundary geometries)
#' @param msoa_centroids sf tibble of MSOA centroids
create_sorted_msoa_batches_by_lad <- function(lads_sf, msoas_sf, msoa_centroids) {

  # Create a list of MSOAs (with centroid geometry) for each LAD
  msoa_centroids_split <- msoa_centroids %>%
    split( ~ ltla21nm)

  lad_centroids <- lads_sf %>%
    sf::st_centroid() %>%
    split( ~ ltla21nm)

  # Arrange the list of MSOAs in each LAD according to how close their centroid
  # is to the LAD's centroid. The intention here is to try to centre to some
  # extent each cluster of MSOAs more faithfully over the extent of its LAD
  msoa_centroids_split %>%

    # create a list of distances from the LAD's overall centroid...
    purrr::map2(., lad_centroids, ~ sf::st_distance(.x, .y)) %>%

    # convert the outputs from st_distance to vectors...
    purrr::map(as.vector) %>%

    # join these vectors back to the source dfs...
    purrr::map2(msoa_centroids_split, ~ dplyr::bind_cols(.y, lad_ctr_proximity = .x)) %>%

    # and then order each df by proximity of the MSOAs
    purrr::map(~ dplyr::arrange(., lad_ctr_proximity)) %>%

    # sort each df by proxim. to initial MSOA
    purrr::map(sort_by_init_proximity) %>%

    # sort list by density and return
    sort_lads_list_by_density(msoas_sf)
}


#' Sort MSOAs by proximity to the closest ("initial") MSOA to the LAD centroid
#'
#' @param dtf sf tibble of MSOA centroids in a LAD
#'
#' @return
sort_by_init_proximity <- function(dtf) {

  top <- dplyr::slice(dtf, 1)

  dtf %>%
    dplyr::mutate(proximity_to_top = sf::st_distance(., top)) %>%
    dplyr::arrange(proximity_to_top)
}

#' Sort LADs list by density
#'
#' @param msoas_sf sf tibble of all MSOAs in area
#' @param msoas_list_by_proximity List of MSOAs already split by LAD name
#'
#' @return

# Use the mean area of MSOAs in each LAD as the way to order the hexing process
sort_lads_list_by_density <- function(msoas_list_by_proximity, msoas_sf) {
  msoas_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(ltla21nm) %>%
    dplyr::summarise(mean_area = mean(shape_area), .groups = "drop") %>%
    dplyr::arrange(mean_area) %>%
    dplyr::pull(ltla21nm) %>%
    # re-order list by name, according to density (smallest mean area)
    `[`(msoas_list_by_proximity, .)
}


london_fixes <- function(lst) {

  london <- lst[["London"]]

  # bring City of London and Westminster higher up the list (above Islington)
  col_pos <- which(names(london) == "City of London")
  wes_pos <- which(names(london) == "Westminster")
  new_names <- c(names(london)[1:2], "City of London", "Westminster", names(london)[-c(1:2, col_pos, wes_pos)])
  london <- london[new_names]

  # Lambeth to come before Hammersmith
  ham_pos <- which(names(london) == "Hammersmith and Fulham")
  lam_pos <- which(names(london) == "Lambeth")
  # wan_pos <- which(names(london) == "Wandsworth")
  new_names <- c(names(london)[1:ham_pos - 1], "Lambeth", names(london)[-c(1:ham_pos - 1, lam_pos)])
  london <- london[new_names]


  # Newham needs to come before Hackney
  hac_pos <- which(names(london) == "Hackney")
  new_pos <- which(names(london) == "Newham")
  new_names <- c(names(london)[1:hac_pos - 1], "Newham", names(london)[-c(1:hac_pos - 1, new_pos)])
  london <- london[new_names]

  # Hounslow needs to come before Ealing
  # Ealing needs to come before Brent
  # Enfield needs to come before Barnet
  # Barnet needs to come before Harrow
  # Barking before Waltham Forest (before Redbridge, which it is anyway)
  wal_pos <- which(names(london) == "Waltham Forest")

  movers <- c("Hounslow", "Ealing", "Enfield", "Barnet", "Barking and Dagenham")
  movers_pos <- which(names(london) %in% movers)

  new_names <- c(names(london)[1:wal_pos - 1], movers, names(london)[-c(1:wal_pos - 1, movers_pos)])
  london <- london[new_names]


  lst <- lst %>%
    purrr::assign_in("London", london)

}


regional_fixes <- function(lst) {

  nw <- lst[["North West"]]

  # datapasta::vector_paste_vertical(names(msoas_density_sorted[[1]]))
  # then edited manually (more times than I want to admit)
  # with surgical delicacy
  new_names <- c(
    "Blackpool",
    "Wirral",
    "Liverpool",

    "Knowsley",
    "West Lancashire",
    "Sefton",

    "Manchester",
    "Salford",
    "Wigan",
    "Tameside",
    "Oldham",

    "St. Helens",
    "Bolton",
    "Bury",

    "Fylde",
    "Chorley",
    "South Ribble",

    "Blackburn with Darwen",
    "Rossendale",
    "Rochdale",
    "Hyndburn",

    "Wyre",
    "Preston",
    "Lancaster",

    "Burnley",
    "Pendle",
    "Ribble Valley",

    "Stockport",
    "Trafford",
    "Warrington",
    "Cheshire East",
    "Halton",
    "Cheshire West and Chester",

    "South Lakeland",
    "Barrow-in-Furness",
    "Copeland",
    "Allerdale",
    "Eden",
    "Carlisle"
    )

  nw <- nw[new_names]

  # return:
  lst <- lst %>%
    purrr::assign_in("North West", nw)


  se <- lst[["South East"]]

  # datapasta::vector_paste_vertical(names(msoas_density_sorted[[2]]))

  new_names <- c(
    "Southampton",
    "Portsmouth",

    "Gravesham",

    "Reading",
    "Slough",
    "Windsor and Maidenhead",

    "Runnymede",
    "Surrey Heath",

    "Worthing",
    "Brighton and Hove",
    "Oxford",
    "Gosport",
    "Hastings",
    "Rushmoor",
    "Havant",
    "Eastbourne",
    "Crawley",
    "Epsom and Ewell",
    "Spelthorne",
    "Medway",
    "Adur",
    "Woking",
    "Fareham",
    "Eastleigh",
    "Elmbridge",
    "Dartford",
    "Thanet",
    "Reigate and Banstead",
    "Bracknell Forest",
    "Wokingham",
    "Milton Keynes",
    "Arun",
    "Guildford",
    "Canterbury",
    "Tonbridge and Malling",
    "Hart",
    "Mid Sussex",
    "Mole Valley",
    "Waverley",
    "Maidstone",
    "Isle of Wight",
    "Swale",
    "Lewes",
    "Dover",
    "Tandridge",
    "Buckinghamshire",
    "Tunbridge Wells",
    "Sevenoaks",
    "Folkestone and Hythe",
    "Basingstoke and Deane",
    "Cherwell",
    "West Berkshire",
    "New Forest",
    "Horsham",
    "South Oxfordshire",
    "East Hampshire",
    "Wealden",
    "Vale of White Horse",
    "Ashford",
    "Test Valley",
    "Rother",
    "Winchester",
    "West Oxfordshire",
    "Chichester")
  se <- se[new_names]

  lst <- lst %>%
    purrr::assign_in("South East", se)


  wm <- lst[["West Midlands"]]

  # datapasta::vector_paste_vertical(names(msoas_density_sorted[[3]]))

  new_names <- c(
    "Birmingham",
    "Wolverhampton",
    "Sandwell",
    "Dudley",
    "Coventry",
    "Worcester",
    "Stoke-on-Trent",

    "Nuneaton and Bedworth",
    "North Warwickshire",
    "Walsall",
    "Tamworth",
    "Lichfield",
    "East Staffordshire",
    "Cannock Chase",

    "Redditch",
    "Solihull",
    "Telford and Wrekin",
    "Newcastle-under-Lyme",
    "Wyre Forest",
    "Bromsgrove",
    "Warwick",
    "South Staffordshire",
    "Rugby",
    "Wychavon",
    "Stafford",
    "Staffordshire Moorlands",
    "Malvern Hills",
    "Stratford-on-Avon",
    "Shropshire",
    "Herefordshire, County of")
  wm <- wm[new_names]

  lst <- lst %>%
    purrr::assign_in("West Midlands", wm)

  yh <- lst[["Yorkshire and The Humber"]]

  # datapasta::vector_paste_vertical(names(msoas_density_sorted[[4]]))


  new_names <- c(
    "Kingston upon Hull, City of", # densest

    "Sheffield", # up against southern boundary
    "Barnsley",
    "Rotherham",


    "Calderdale", # up against western boundary
    "Wakefield",
    "Harrogate",
    "Leeds", # major city
    "Bradford", # major city
    "Kirklees",

    "Doncaster",
    "North East Lincolnshire",
    "York",
    "North Lincolnshire",
    "East Riding of Yorkshire",

    "Scarborough",
    "Selby",
    "Hambleton",
    "Craven",
    "Richmondshire",
    "Ryedale")
  yh <- yh[new_names]

  lst <- lst %>%
    purrr::assign_in("Yorkshire and The Humber", yh)


  ne <- lst[["North East"]]

  # datapasta::vector_paste_vertical(names(msoas_density_sorted[[5]]))

  new_names <- c(
    "South Tyneside",
    "North Tyneside",

    "Middlesbrough",
    "Sunderland",
    "Newcastle upon Tyne",
    "Gateshead",
    "Hartlepool",
    "Stockton-on-Tees",
    "Redcar and Cleveland",
    "Darlington",
    "County Durham",
    "Northumberland")

  ne <- ne[new_names]

  lst <- lst %>%
    purrr::assign_in("North East", ne)

  # return:
  lst
}
