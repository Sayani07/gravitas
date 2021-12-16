#' Title computing all within and between facet distances between quantile categories given a data
#'
#' @param .data data for which mmpd needs to be calculated
#' @param gran_x granularities mapped across x levels
#' @param gran_facet granularities mapped across facets
#' @param response univarite response variable
#' @param quantile_prob probabilities
#' @param dist_ordered if categories are ordered
#' @param lambda value of tuning parameter for computing weighted pairwise distances
#' @return the raw weighted pairwise  within-facet and between-facet distances
#'
#' @examples
#' library(dplyr)
#' library(parallel)
#' sm <- smart_meter10 %>%
#'   filter(customer_id %in% c("10017936"))
#' gran_x <- "month_year"
#' gran_facet <- "wknd_wday"
#' v <- compute_pairwise_dist(sm, gran_x, gran_facet,
#'   response = general_supply_kwh
#' )
#' # month of the year not working in this setup
#' @export compute_pairwise_dist
compute_pairwise_dist <- function(.data,
                                  gran_x = NULL,
                                  gran_facet = NA,
                                  response = NULL,
                                  quantile_prob =
                                    seq(0.01, 0.99, 0.01),
                                  dist_ordered = TRUE,
                                  lambda = 0.67) {
  if (!is.na(gran_facet)) {
    lambda_t <- lambda
    if (!((gran_x %in% names(.data) &
      (gran_facet %in% names(.data))))) {
      .data <- .data %>%
        gravitas::create_gran(gran_x) %>%
        gravitas::create_gran(gran_facet) %>%
        dplyr::rename("id_facet" = !!gran_facet) %>%
        dplyr::rename("id_x" = !!gran_x)
    } else {
      .data <- .data %>%
        dplyr::rename("id_facet" = !!gran_facet) %>%
        dplyr::rename("id_x" = !!gran_x)
    }
  } else {
    lambda_t <- 1
    if (!((gran_x %in% names(.data)))) {
      .data <- .data %>%
        gravitas::create_gran(gran_x) %>%
        dplyr::rename("id_x" = !!gran_x) %>%
        dplyr::mutate(id_facet = 1)
    } else {
      .data <- .data %>%
        dplyr::rename("id_facet" = !!gran_facet) %>%
        dplyr::rename("id_x" = !!gran_x)
    }
  }

  all_dist_data <- suppressMessages(
    .data %>%
      tibble::as_tibble() %>%
      dplyr::select(id_x, id_facet, {{ response }}) %>%
      dplyr::rename("sim_data" = {{ response }}) %>%
      # mutate(sim_data = scale(sim_data)) %>%
      compute_quantiles(
        quantile_prob =
          quantile_prob
      ) %>%
      distance_all_pairwise(
        quantile_prob =
          quantile_prob,
        dist_ordered = dist_ordered,
        lambda = lambda_t
      )
  )

  all_dist_data
}
