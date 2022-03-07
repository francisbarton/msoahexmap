## code to prepare `itl2_boundaries` dataset goes here


# https://geoportal.statistics.gov.uk/datasets/international-territorial-level-2-january-2021-uk-bgc-v2/
#
# Source: Office for National Statistics licensed under the
# Open Government Licence v.3.0
#
# Contains OS data © Crown copyright and database right [2021]

itl2_boundaries <- sf::st_read("https://opendata.arcgis.com/datasets/0488a2ea0c4b450683510cac7ad4a216_0.geojson") |>
  janitor::clean_names()

usethis::use_data(itl2_boundaries, internal = TRUE, overwrite = TRUE)
