#' Create data for one harmony pair
#'
#' @param .data a tsibble
#' @param harmony_tbl_row a row from the harmony table
#' @param response univariate response variable
#'
#' @return a tibble with the harmony pair and response variable
#'
#' @examples
#' library(dplyr)
#' sm <- smart_meter10 %>%
#'   filter(customer_id %in% c("10017936"))
#' harmonies <- sm %>%
#'   harmony(
#'     ugran = "month",
#'     filter_in = "wknd_wday",
#'     filter_out = c("hhour", "fortnight")
#'   )
#' # harmonies <- harmonies %>% mutate(facet_variable = NA)
#' panel_data <- create_harmony_data(
#'   sm,
#'   harmonies[3, ], general_supply_kwh
#' )
#' @export
create_harmony_data <- function(.data, harmony_tbl_row, response) {
  if (!is.na(harmony_tbl_row$facet_variable)) {
    .data <- .data %>%
      gravitas::create_gran(harmony_tbl_row$facet_variable) %>%
      gravitas::create_gran(harmony_tbl_row$x_variable)


    data <- .data %>%
      tibble::as_tibble() %>%
      dplyr::select(
        id_facet = harmony_tbl_row$facet_variable,
        id_x = harmony_tbl_row$x_variable,
        sim_data = {{ response }}
      )
  } else {
    .data <- .data %>%
      gravitas::create_gran(harmony_tbl_row$x_variable)


    data <- .data %>%
      tibble::as_tibble() %>%
      dplyr::select(
        # id_facet = NA,
        id_x = harmony_tbl_row$x_variable,
        sim_data = {{ response }}
      ) %>%
      dplyr::mutate(id_facet = 1)
  }

  data %>%
    dplyr::mutate(
      x_variable = harmony_tbl_row$x_variable,
      facet_variable = harmony_tbl_row$facet_variable
    ) %>%
    dplyr::select(facet_variable, x_variable, id_facet, id_x, sim_data)
}
