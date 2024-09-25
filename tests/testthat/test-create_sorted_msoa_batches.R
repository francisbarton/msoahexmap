test_that("create_sorted_msoa_batches works", {

  # get data inputs
  msoa_centroids_by_region <- readRDS("_targets/objects/msoa_centroids_by_region")
  eng_lad_centroids <- readRDS("_targets/objects/eng_lad_centroids")
  msoas_by_region <- readRDS("_targets/objects/msoas_by_region")

  # some assertion tests on data inputs
  expect_length(msoa_centroids_by_region, 9)
  expect_named(
    msoa_centroids_by_region[[1]],
    c("msoa21cd", "msoa21nm", "msoa21hclnm", "lad23cd", "lad23nm", "geometry")
  )
  
  expect_length(eng_lad_centroids, 9)
  expect_named(
    eng_lad_centroids[[1]],
    c("lad23cd", "lad23nm", "rgn23cd", "rgn23nm", "geometry")
  )
  # expect_equal(sf::st_geometry_type(eng_lad_centroids[[1]][1,]), "POINT")
  
  expect_length(msoas_by_region, 9)
  # expect_true(
  #   all(sf::st_geometry_type(msoas_by_region[[1]]) %in% c("POLYGON", "MULTIPOLYGON"))
  # )
  expect_true(
    all(c("msoa21cd", "msoa21nm", "lad23cd", "lad23nm", "shape_area") %in% names(msoas_by_region[[1]]))
  )

  # test out the function

  # This is what we have in the workflow currently:
  sorted_list <- list(
    msoa_centroids_by_region,
    eng_lad_centroids,
    msoas_by_region
  ) |>
    purrr::pmap(create_sorted_msoa_batches) |>
    expect_error() # we want to be able to switch this to expect_no_error
  
  # So each of the 3 data inputs (all have length 9) will be passed element-wise
  # to "create_sorted_msoa_batches" by pmap.
  # Let's see if there's an error when we pass the first elements in:
  out <- create_sorted_msoa_batches(
    msoa_centroids_by_region[[1]],
    eng_lad_centroids[[1]],
    msoas_by_region[[1]]
  ) |>
    expect_error() # we want to be able to switch this to expect_no_error


  msoa_centrs1 <- msoa_centroids_by_region[[1]]
  lad_centrs1 <- eng_lad_centroids[[1]]
  regn_msoas1 <- msoas_by_region[[1]]
  
  msoa_centroids_list <- msoa_centrs1 |>
    dplyr::nest_by(lad23cd, .keep = TRUE) |>
    tibble::deframe() |>
    expect_no_error()
  
  # 1 list item per LAD in the region
  expect_length(msoa_centroids_list, length(unique(msoa_centrs1[["lad23cd"]])))

  lad_centroids_list <- lad_centrs1 |>
    dplyr::nest_by(lad23cd, .keep = TRUE) |>
    tibble::deframe() |>
    expect_no_error()

  # 1 list item per LAD in the region
  expect_length(lad_centroids_list, length(unique(lad_centrs1[["lad23cd"]])))

  # Each list item should be for the same LAD across both lists
  expect_equal(length(msoa_centroids_list), length(lad_centroids_list))
  expect_equal(names(msoa_centroids_list), names(lad_centroids_list))
  
  # List items are tibbles
  expect_s3_class(msoa_centroids_list[[1]], "tbl_df")
  expect_s3_class(lad_centroids_list[[1]], "tbl_df")

  lad_msoa_centrs <- msoa_centroids_list[[1]]
  lad_centr <- lad_centroids_list[[1]]

  expect_equal(nrow(lad_msoa_centrs), 11)
  expect_equal(nrow(lad_centr), 1)

  lad_ctr_prox <- \(x, y) {
    dplyr::mutate(x, lad_ctr_prox = as.vector(sf::st_distance(x, y)))
  }
  out1a <- lad_ctr_prox(lad_msoa_centrs, lad_centr) |>
    expect_no_error()
  expect_named(out1a, c(names(lad_msoa_centrs), "lad_ctr_prox"))
  expect_true(all(is.numeric(out1a[["lad_ctr_prox"]])))

  out1b <- msoa_centroids_list |>
    purrr::map2(lad_centroids_list, lad_ctr_prox)

  expect_identical(out1a, out1b[[1]])

  out2a <- sort_by_init_proximity(out1a)
  expect_named(out2a, names(lad_msoa_centrs))

  inputs_as_list <- msoa_centroids_list |>
    # Add column of distances for each MSOA centr from the LAD's overall centr
    purrr::map2(lad_centroids_list, lad_ctr_prox) |>
    # Use `sort_by_init_proximity()` to sort each df by proxim. to initial MSOA
    purrr::map(sort_by_init_proximity)

  expect_length(inputs_as_list, 12) # 12 LADs in region
  expect_equal(nrow(regn_msoas1), 342)

  ordered_lad_codes <- regn_msoas1 |>
    sf::st_drop_geometry() |>
    dplyr::summarise(across("shape_area", mean), .by = "lad23cd") |>
    dplyr::arrange(pick("shape_area")) |>
    dplyr::pull("lad23cd")
  expect_length(ordered_lad_codes, 12)

  expect_named(inputs_as_list, ordered_lad_codes, TRUE)

  out <- inputs_as_list[ordered_lad_codes] |>
    expect_no_error()

})
