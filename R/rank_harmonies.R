#' rank harmonies with significant patterns
#' @param .data a tsibble or data with already computed categories
#' @param harmony_tbl A tibble containing one or more hamronies with facet_variable, x_variable, facet_levels and x_levels
#' @param dist_ordered if categories are ordered
#' @param quantile_prob numeric vector of probabilities with value #'in [0,1]  whose sample quantiles are wanted. Default is set to #' "decile" plot
#' @param lambda value of tuning parameter for computing weighted pairwise distances
#' @param nperm number of permutations for normalization
#' @param response the response variable
#' @param use_perm should permutation approach for normalization be used
#' @examples
#' library(gravitas)
#' library(parallel)
#' library(dplyr)
#' library(tidyr)
#' sm <- smart_meter10 %>%
#'   filter(customer_id %in% c("10017936"))
#' harmonies <- sm %>%
#'   harmony(
#'     ugran = "month",
#'     filter_in = "wknd_wday",
#'     filter_out = c("hhour", "fortnight")
#'   )
#' all_harmony <- rank_harmonies(sm,
#'   harmony_tbl = harmonies,
#'   response = general_supply_kwh
#' )
#'@export
rank_harmonies <- function(.data,
                             harmony_tbl = NULL,
                             response = NULL,
                             quantile_prob = seq(0.01, 0.99, 0.01),
                             dist_ordered = TRUE,
                             lambda = 0.67,
                             nperm = 20,
                             use_perm = TRUE) {

  wpd_obs <- wpd(.data,
                 harmony_tbl,
                 {{response}},
                 quantile_prob = seq(0.01, 0.99, 0.01),
                 dist_ordered,
                 lambda,
                 nperm,
                 use_perm) %>%
    unlist() %>%
    tibble::as_tibble()


  harmony_tbl %>%
    dplyr::bind_cols(wpd_obs) %>%
    dplyr::rename(wpd = value) %>%
    dplyr::arrange(-wpd)
  #                            gt_maxpd = max_pd > right_quantile_maxpd)

}
