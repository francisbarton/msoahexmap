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
    ), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  tar_target(
    name = eng_regions,
    command = boundr::bounds(
      "rgn", "ctry", "England", lookup_year = "2022", crs = 27700)
  ),
  tar_target(
    name = lads_by_region,
    command = get_all_region_lads(eng_regions)  
  ),
  tar_target(
    name = eng_lad_centroids,
    command = get_all_lad_centroids(lads_by_region)
  ),
  tar_target(
    name = msoas_by_region,
    command = get_msoas_by_region(lads_by_region, eng_regions)
  ),
  tar_target(
    name = msoa_centroids_by_region,
    command = get_msoa_centroids_by_rgn(lads_by_region, eng_regions)
  ),
  tar_target(
    name = msoas_sorted_orig,
    command = sort_msoas_by_proximity(
      msoa_centroids_by_region,
      eng_lad_centroids,
      msoas_by_region
    )
  ),
  tar_target(
    name = base_grid_orig,
    command = create_base_grid(eng_regions, cell_size = 3160)
  )
)
