# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c(
    "boundr",
    "dplyr",
    "janitor",
    "purrr",
    "sf",
    "tibble",
    "tidyr"
    ),
  format = "rds"
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

list(
  eng_regions |>
    tar_target(
      boundr::bounds("rgn", "ctry", "England",
                     lookup_year = "2022", crs = 27700)
  ),

  lads_by_region |>
    tar_target(
      get_all_region_lads(eng_regions)
      ),

  eng_lad_centroids |>
    tar_target(
      get_all_lad_centroids(lads_by_region)
      ),

  msoas_by_region |>
    tar_target(
      get_msoas_by_region(lads_by_region, eng_regions)
      ),

  msoa_centroids_by_region |>
    tar_target(
      get_msoa_centroids_by_rgn(lads_by_region, eng_regions)
      ),

  msoas_sorted_orig |>
    tar_target(
      sort_msoas_by_proximity(
        msoa_centroids_by_region,
        eng_lad_centroids,
        msoas_by_region
      )
  ),

  base_grid_full |>
    tar_target(
      create_base_grid(eng_regions, cell_size = 3160)
      ),

  london_index |>
    tar_target(
      extract_london_index(base_grid_full, eng_regions)
      ),

  london_orig |>
    tar_target(
      base_grid_full[london_index]
      ),

  london_swole |>
    tar_target(
      extract_london(base_grid_full, london_index)
    ),

  base_grid_orig |>
    tar_target(
      base_grid_full[eng_regions] |>
        fettle_edges() |>
        sf::st_difference(sf::st_union(london_orig))
      ),

  london_msoas |>
    tar_target(
      msoas_sorted_orig[["London"]]
    ),

  regional_msoas |>
    tar_target(
      msoas_sorted_orig |>
        purrr::discard_at("London")
    )
)
