#' Selecting threshold with rcompanion package
#'
#' @param .data a tsibble.
#' @param response response variable.
#' @param prob numeric vector of probabilities with values in [0,1].
#' @param harmony_tbl A tibble of harmonies and their levels obtained from the function().
#' @param hierarchy_tbl A hierarchy table specifying the hierarchy of units

# threshold_harmonies <- function(.data = NULL,
#                                 harmony_tbl = NULL,
#                                 response = NULL,
#                                 prob = seq(0.01,   0.99, 0.01),
#                                 hierarchy_tbl = NULL)
# {
#
#   (1:nrow(harmony_tbl)) %>% purrr::map(function(rowi){
#     cyc_grans <- harmony_tbl%>% magrittr::extract(rowi,)
#     facet_var <- cyc_grans$facet_variable
#     x_var <- cyc_grans$x_variable
#     z <- p_harmonies(.data, gran1 = facet_var, gran2 = x_var, response,  prob, hierarchy_tbl, test, tau, r)
#   })
# }

# p_harmonies <- function(.data = NULL,
#                                gran1 = NULL,
#                                gran2 = NULL,
#                                response = NULL,
#                                prob = seq(0.01, 0.99, 0.01),
#                                hierarchy_tbl = NULL,  test = "median", tau = 0.95, r = 500,...)
# {
#   new_distvector <- hist_distance(.data,
#                                   gran1,
#                                   gran2,
#                                   response,
#                                   prob = seq(0.01, 0.99, 0.01),
#                                   hierarchy_tbl,...)
#
#   PT <- rcompanion::pairwisePercentileTest(freqx ~ newds,
#                     data = new_distvector,
#                     test = test,
#                     tau  = tau,
#                     r    = r)
#
#   pairwise_diff = try(rcompanion::cldList(p.adjust ~ Comparison, data = PT))
#
#   if("try-error" %in% class(pairwise_diff)){
#     return_diff = 1
#   }
#   return_diff = dplyr::if_else(length(unique(pairwise_diff)) <=1, "reject", "choose")
#   return_diff
# }
