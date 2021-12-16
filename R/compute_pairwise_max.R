#' Title Compute raw pairwise distances
#'
#' @param .data data for which mmpd needs to be calculated
#' @param gran_x granularities mapped across x levels
#' @param gran_facet granularities mapped across facetss
#' @param response univarite response variable
#' @param quantile_prob probabilities
#' @param dist_ordered if categories are ordered
#' @param lambda value of tuning parameter for computing weighted pairwise distances
#' @return the raw weighted pairwise distance
#'
#' @examples
#' library(dplyr)
#' library(parallel)
#' sm <- smart_meter10 %>%
#'   filter(customer_id %in% c("10017936"))
#' gran_x <- "month_year"
#' gran_facet <- "hour_day"
#' v <- compute_pairwise_max(sm, gran_x, gran_facet,
#'   response = general_supply_kwh, lambda = 0.95
#' )
#' # month of the year not working in this setup
#' @export compute_pairwise_max
compute_pairwise_max <- function(.data,
                                 gran_x = NULL,
                                 gran_facet = NULL,
                                 response = NULL,
                                 quantile_prob =
                                   seq(0.01, 0.99, 0.01),
                                 dist_ordered = TRUE,
                                 lambda = 0.67) {
  dist_data <- compute_pairwise_dist(
    .data,
    gran_x,
    gran_facet,
    {{ response }},
    quantile_prob,
    dist_ordered,
    lambda
  )

  max(dist_data$trans_value, na.rm = TRUE) %>% round(digits = 3)
}
