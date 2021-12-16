#' Title Compute scalar normalised pairwise distances
#'
#' @param .data data for which mmpd needs to be calculated
#' @param gran_x granularities mapped across x levels
#' @param gran_facet granularities mapped across facetss
#' @param response univarite response variable
#' @param quantile_prob probabilities
#' @param dist_ordered if categories are ordered
#' @param lambda value of tuning parameter for computing weighted pairwise distances
#' @return the weighted pairwise distance normalised through modeling raw distances as a function of total number of categories
#'
#' @examples
#' library(dplyr)
#' library(parallel)
#' sm <- smart_meter10 %>%
#'   dplyr::filter(customer_id %in% c("10017936"))
#' gran_x <- "day_week"
#' gran_facet <- "month_year"
#' v <- compute_pairwise_norm_scalar(sm, gran_x, gran_facet,
#'   response = general_supply_kwh, lambda = 0.67
#' )
#' # month of the year not working in this setup
#' @export compute_pairwise_norm_scalar
compute_pairwise_norm_scalar <- function(.data,
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

  raw <- max(dist_data$trans_value, na.rm = TRUE)

  # fitting a log-linear model and normalising for the number of distances
  # (raw - 0.0027 * log(nrow(dist_data))) %>% round(digits = 3)

  # for two granularities
  if (!is.na(gran_facet)) {
    (raw - 1 / (23.4 - 0.96 * log(nrow(dist_data)))) * 320 %>%
      round(digits = 3)
  }

  # for one granularity

  else {
    (raw - 1 / (26.09 - 1.87 * log(nrow(dist_data)))) * 260 %>%
      round(digits = 3)
  }
}
