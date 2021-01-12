order_along <- function(df, order_along, order_by) {

  df2 <- df %>%
    sf::st_drop_geometry()

  cols <- colnames(df2)

  df2 <- df2 %>%
    dplyr::arrange({{ order_by }})

  df2 %>%
    dplyr::select({{ order_along }}) %>%
    dplyr::distinct() %>%
    dplyr::full_join(df2) %>%
    dplyr::select(dplyr::all_of(cols)) %>%
    dplyr::left_join(df) %>%
    sf::st_sf()

}
