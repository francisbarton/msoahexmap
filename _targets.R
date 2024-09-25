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
  region_bounds |>
    tar_target(get_region_bounds()),

  lad_lookup |>
    tar_target(get_lad_lookup()),

  itl2_lookup |>
    tar_target(get_itl2_lookup()),
  
  msoa_lookup |>
    tar_target(get_msoa_lookup()),
  
  itl2_bounds |>
    tar_target(get_itl2_bounds(itl2_lookup)),

  lad_bounds |>
    tar_target(get_lad_bounds(lad_lookup, itl2_lookup)),

  msoa_bounds |>
    tar_target(get_msoa_bounds(lad_lookup, itl2_lookup, msoa_lookup)),

  msoa_centroids |>
    tar_target(get_msoa_centroids(lad_lookup, itl2_lookup, msoa_lookup)),

  sorted_msoa_data |>
    tar_target(
      create_sorted_msoa_data(msoa_centroids, msoa_bounds, itl2_bounds)
    ),

  base_grid_full |>
    tar_target(create_base_grid(region_bounds, cell_size = 3160)),

  london_index |>
    tar_target(extract_london_index(base_grid_full, region_bounds)),

  london_orig |>
    tar_target(base_grid_full[london_index]),

  london_swole |>
    tar_target(extract_london(base_grid_full, london_index)),

  base_grid_orig |>
    tar_target(
      base_grid_full[region_bounds] |>
        # fettle_edges() |>
        sf::st_difference(sf::st_union(london_orig))
    )
)
