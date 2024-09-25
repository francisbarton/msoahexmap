get_region_bounds <- function() {
  out <- boundr::bounds("rgn", opts = opts(crs = 27700, resolution = "BUC"))
  assertthat::assert_that(
    nrow(out) == 9L,
    identical(colnames(out), c("rgn23cd", "rgn23nm", "geometry")),
    "sf" %in% class(out),
    "tbl_df" %in% class(out)
  )
  out
}


get_lad_bounds <- function(lad_lookup, itl2_lookup) {
  out <- lad_lookup |>
    dplyr::select(!"lad23nm") |> # matching on lad23nm is perilous
    dplyr::left_join(itl2_lookup, "lad23cd") |>
    dplyr::relocate(c("lad23nm", starts_with("itl")), .after = "lad23cd") |>
    boundr::add_geometry(opts(crs = 27700, resolution = "BUC"))
  assertthat::assert_that(
    nrow(out) == 296L,
    identical(
      colnames(out),
      c(
        "lad23cd", "lad23nm", "itl221cd", "itl221nm", "rgn23cd",
        "rgn23nm", "geometry"
      )
    ),
    "tbl_df" %in% class(out),
    "sf" %in% class(out)
  )
  out
}

get_itl2_bounds <- function(itl2_lookup) {
  out <- itl2_lookup |>
    dplyr::select(starts_with("itl2")) |>
    dplyr::distinct() |>
    dplyr::mutate(rgn23cd = dplyr::case_when(
      grepl("^TLC", itl221cd) ~ "E12000001",
      grepl("^TLD", itl221cd) ~ "E12000002",
      grepl("^TLE", itl221cd) ~ "E12000003",
      grepl("^TLF", itl221cd) ~ "E12000004",
      grepl("^TLG", itl221cd) ~ "E12000005",
      grepl("^TLH", itl221cd) ~ "E12000006",
      grepl("^TLI", itl221cd) ~ "E12000007",
      grepl("^TLJ", itl221cd) ~ "E12000008",
      grepl("^TLK", itl221cd) ~ "E12000009"
    )) |>
    dplyr::mutate(rgn23nm = dplyr::case_when(
      grepl("^TLC", itl221cd) ~ "North East",
      grepl("^TLD", itl221cd) ~ "North West",
      grepl("^TLE", itl221cd) ~ "Yorkshire and The Humber",
      grepl("^TLF", itl221cd) ~ "East Midlands",
      grepl("^TLG", itl221cd) ~ "West Midlands",
      grepl("^TLH", itl221cd) ~ "East of England",
      grepl("^TLI", itl221cd) ~ "London",
      grepl("^TLJ", itl221cd) ~ "South East",
      grepl("^TLK", itl221cd) ~ "South West"
    )) |>
    boundr::add_geometry_to_table(opts(crs = 27700, resolution = "BUC"))
  assertthat::assert_that(
    nrow(out) == 33L,
    identical(
      colnames(out),
      c("itl221cd", "itl221nm", "rgn23cd", "rgn23nm", "geometry")
    ),
    "tbl_df" %in% class(out),
    "sf" %in% class(out)
  )
  out
}


get_itl2_lookup <- function() {
  out <- boundr::lookup("lad", "itl2", lookup_year = 2023) |>
    dplyr::filter(if_any("itl221cd", \(x) grepl("TL[C-K]", x)))
  assertthat::assert_that(
    nrow(out) == 296L,
    identical(colnames(out), c("lad23cd", "lad23nm", "itl221cd", "itl221nm")),
    "tbl_df" %in% class(out),
    all(purrr::map_lgl(out, is.character))
  )
  out
}

get_lad_lookup <- function() {
  out <- "lad" |>
    boundr::lookup("rgn", lookup_year = 2023) |>
    dplyr::filter(if_any("lad23cd", \(x) grepl("^E", x)))
  assertthat::assert_that(
    nrow(out) == 296L,
    identical(colnames(out), c("lad23cd", "lad23nm", "rgn23cd", "rgn23nm")),
    "tbl_df" %in% class(out),
    all(purrr::map_lgl(out, is.character))
  )
  out
}


get_msoa_lookup <- function() {
  out <- boundr::lookup("msoa", "lad", within_year = 2023, opts = opts(query_option = 1)) |>
    dplyr::filter(if_any("msoa21cd", \(x) grepl("^E", x))) |>
    dplyr::select(!ends_with("nmw"))
  assertthat::assert_that(
    nrow(out) == 6856L,
    identical(
      colnames(out),
      c("msoa21cd", "msoa21nm", "msoa21hclnm", "lad23cd", "lad23nm")
    ),
    "tbl_df" %in% class(out),
    all(purrr::map_lgl(out, is.character))
  )
  out
}


get_msoa_bounds <- function(lad_lookup, itl2_lookup, msoa_lookup) {
  lad_lookup |>
    dplyr::select(!"lad23nm") |> # matching on lad23nm is unreliable
    dplyr::left_join(itl2_lookup, "lad23cd") |>
    dplyr::select(!"lad23nm") |> # matching on lad23nm is fraught with danger
    dplyr::left_join(msoa_lookup, "lad23cd") |>
    dplyr::relocate(starts_with("msoa")) |>
    dplyr::relocate(c("lad23nm", starts_with("itl")), .after = "lad23cd") |>
    boundr::add_geometry(
      # return_width = "full" gives us the shape_area column
      boundr_opts(crs = 27700, resolution = "BSC", return_width = "full")
    ) |>
    # but it also adds this column back in, which now wants moving left
    # (if we include Wales again in lad_lookup then this will already be here)
    dplyr::relocate("msoa21nmw", .after = "msoa21nm")
}

get_msoa_centroids <- function(lad_lookup, itl2_lookup, msoa_lookup) {
  lad_lookup |>
    dplyr::select(!"lad23nm") |> # matching on lad23nm is a grave mistake
    dplyr::left_join(itl2_lookup, "lad23cd") |>
    dplyr::select(!"lad23nm") |> # matching on lad23nm will only cause pain
    dplyr::left_join(msoa_lookup, "lad23cd") |>
    dplyr::relocate(starts_with("msoa")) |>
    dplyr::relocate(c("lad23nm", starts_with("itl")), .after = "lad23cd") |>
    boundr::add_geometry(opts(crs = 27700), geometry = "centroids")
}



# make a grid of the whole country, given `cell_size` as a variable.
create_base_grid <- function(eng_regions, cell_size) {
  sf::st_make_grid(
    # 15826 hexes at size 3160 (reduces to 15592 when London area excluded)
    sf::st_union(eng_regions),
    what = "polygons",
    square = FALSE,
    flat_topped = TRUE,
    cellsize = cell_size
  )
}


fettle_edges <- function(base_grid) {
  estuaries_etc <- list(
    solent = c(915, 962, 1106, 1154, 1207, 1262, 1059, 1060),
    thames = c(
      4445, 4488, 4444, 4487, 4443, 4353, 4305, 4352, 4304, 4351, 4396
    ),
    severn = c(4758, 4805, 4898, 4945, 4992),
    mersey = c(11538, 11457, 11373, 11331, 11245, 11203),
    humber = c(12077, 12115, 12190, 12228, 12303, 12341, 12302, 12264),
    borders = c(
      15804, 15770, 15722, 15620, 15584, 15534, 15282, 15243, 10945, 10903,
      10820, 10572, 10413, 10115, 10016, 9687, 9447, 9267, 9030, 8182, 8304,
      7995, 7871, 7507, 7448, 6858, 6271, 6157, 5824, 5716, 5662, 5458, 4712)
  ) |>
    purrr::list_c() # length 71

  base_grid[-estuaries_etc]
}
