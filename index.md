---
title: Allocating MSOAs to a hexmap grid
date:  January 17, 2021
output: 
  html_document:
    keep_md: TRUE
---






### LADs, MSOAs and Centroids

The intention is to create a [hexmap](hexmap) of MSOAs in the wider county of Sussex.
This area comprises three modern-day upper-tier local authorities:

* Brighton and Hove
* West Sussex
* East Sussex.

First, we obtain the geographical data we will need to use:



```r
# Local Authority District names and boundaries within 3 upper-tier authorities
sussex_lads <- c("Brighton and Hove", "East Sussex", "West Sussex") %>%
  purrr::map_df(
    ~ jogger::geo_get("lad"
                      , .
                      , "utla"
                      , return_style = "simple"
                      , spatial_ref = 27700
                      )
    ) %>% 
  dplyr::rename(lad20nm = ltla20nm)


# MSOA names and boundaries within each LAD
sussex_bounds <- sussex_lads %>%
  dplyr::pull(lad20nm) %>%
  purrr::map_df(
    ~ jogger::geo_get("msoa"
                      , .
                      , "lad"
                      , spatial_ref = 27700
                      , shape_fields = TRUE
                      )
    ) %>% 
  dplyr::relocate(c(shape_area, shape_length, geometry), .after = last_col())
  # # not needed now - was previous way of sorting areas by smallest MSOA area
  # %>% order_along(lad20cd, shape_area)


# Population-weighted centroids of the MSOAs
sussex_centroids <- sussex_bounds %>%
  sf::st_drop_geometry() %>% 
  dplyr::select(starts_with(c("msoa", "lad"))) %>% 
  dplyr::left_join(
    pull(., msoa11cd) %>% 
    jogger::geo_get_bounds(
      "msoa11cd"
      , .
      , spatial_ref = 27700
      , return_centroids = TRUE
    )
  ) %>% 
  sf::st_as_sf()
```








West Sussex and East Sussex then comprise, between them, 12 lower-tier authorities (LADs), making 13 in total (Brighton and Hove is a unitary.)

The area contains 202 Middle Layer Super Output Areas (MSOAs).
These are Census geographies (divisions) that are designed to have populations of roughly 8,000 people each, though this varies quite a bit in reality
(and due to change since their establishment in 2011).

MSOAs are each located within a certain LAD - they do not cross LAD boundaries.

The [ONS](ons-opengeo) provides population-weighted centroids (central points) for each MSOA.


Now we have data for the boundaries of the LADs:


```r
head(sussex_lads, 3)
```

```
## Simple feature collection with 3 features and 4 fields
## geometry type:  GEOMETRY
## dimension:      XY
## bbox:           xmin: 523598.1 ymin: 95149.32 xmax: 587158.6 ymax: 113659.4
## projected CRS:  OSGB 1936 / British National Grid
##     lad20cd           lad20nm  utla20cd          utla20nm
## 1 E06000043 Brighton and Hove E06000043 Brighton and Hove
## 2 E07000061        Eastbourne E10000011       East Sussex
## 3 E07000062          Hastings E10000011       East Sussex
##                         geometry
## 1 POLYGON ((529091.9 111919.1...
## 2 MULTIPOLYGON (((561983.3 10...
## 3 MULTIPOLYGON (((578843.4 11...
```

and we can plot these along with their (geometric) centroids:


```r
sussex_lad_centroids <- sussex_lads %>% 
  sf::st_centroid()
```

```
## Warning in st_centroid.sf(.): st_centroid assumes attributes are constant over
## geometries of x
```

```r
tm_shape(sussex_lads) +
  tm_borders("grey45", lwd = 2) +
  tm_fill("lad20nm", alpha = 0.2, palette = my_pal(13)) +
  tm_layout(legend.outside = TRUE) +
  tm_shape(sussex_lad_centroids) +
  tm_dots(col = "orange", size = 0.1)
```

![](index_files/figure-html/lads-list2-1.png)<!-- -->


We also have the boundaries of the MSOAs:


```r
head(sussex_bounds, 3)
```

```
## Simple feature collection with 3 features and 7 fields
## geometry type:  POLYGON
## dimension:      XY
## bbox:           xmin: 527461.1 ymin: 107261.5 xmax: 535584.2 ymax: 111919.1
## projected CRS:  OSGB 1936 / British National Grid
##    msoa11cd              msoa11nm                 msoa11hclnm   lad20cd
## 1 E02003491 Brighton and Hove 001                Patcham East E06000043
## 2 E02003492 Brighton and Hove 002 Coldean & Moulsecoomb North E06000043
## 3 E02003493 Brighton and Hove 003     Patcham West & Westdene E06000043
##             lad20nm shape_area shape_length                       geometry
## 1 Brighton and Hove    3182609     13261.52 POLYGON ((532232.2 110000.1...
## 2 Brighton and Hove    6021309     13229.30 POLYGON ((534131 110292.5, ...
## 3 Brighton and Hove   10279974     19527.72 POLYGON ((529091.9 111919.1...
```


which we can plot as well as their centroids:


```r
tm_shape(sussex_bounds) + 
  tm_borders("grey75", lwd = 1) + 
  tm_shape(sussex_centroids) + 
  tm_dots(col = "white", size = 0.2) + 
  tm_shape(sussex_centroids) + 
  tm_dots(col = "tomato2", size = 0.1) 
```

![](index_files/figure-html/cent-list-1.png)<!-- -->


### Using `sf` to make a hex grid for our area









[hexmap]: https://
[ons-opengeo]: https://
