#' Selecting harmonies with significant difference in distributions for two cyclic granularities
#'
#' @param .data a tsibble.
#' @param response response variable.
#' @param harmony_tbl A tibble of harmonies and their levels obtained from the function().
#' @param prob numeric vector of probabilities with values in [0,1].
#' @param hierarchy_tbl A hierarchy table specifying the hierarchy of units
#' @param create_gran_data if data corresponding to a pair of cyclic granularity needs to be created
#' @param nsamp sample size of permutation test to compute threshold
#' @param dist_ordered if levels of the time granularity is ordered.
#' @param ... Other arguments passed on to individual methods.
#' @examples
#' \dontrun{
#' library(tsibbledata)
#' library(ggplot2)
#' library(tsibble)
#' library(lvplot)
#' library(dplyr)
#' library(gravitas)
#' library(purrr)
#' library(magrittr)
#' sm <- smart_meter10 %>%
#' filter(customer_id %in% c("10017936"))
#' .data = sm
#' harmonies <- sm %>%
#' harmony(ugran = "month",
#'        filter_in = "wknd_wday",
#'        filter_out = c("hhour", "fortnight"))
#' gran1 = "wknd_wday"
#' gran2 = "hour_day"
#' response  = "general_supply_kwh"
#' global_harmony <-  sm %>%
#' global_threshold(harmony_tbl = harmonies,
#' response = "general_supply_kwh", nsamp = 2)
#' }
#' @export

global_threshold <- function(.data = NULL,
                                harmony_tbl = NULL,
                                response = NULL,
                                prob = seq(0.01,0.99, 0.01),
                                hierarchy_tbl = NULL,
                                create_gran_data = TRUE,
                                dist_ordered = TRUE,
                                nsamp = 20,...)
{
  MMPD_obs <-  .data %>%
    rank_harmony(harmony_tbl = harmonies,
                 response = response,
                 create_gran_data = create_gran_data,
                 dist_ordered = dist_ordered,...)


MMPD_sample_lst <- (1:nsamp) %>%
    purrr::map(function(i){

      if(create_gran_data)
      {
        response_sample <-  sample(.data[[response]], size = nrow(.data))

      data_sample <- .data %>%
  dplyr::mutate(response = response_sample)%>%
  dplyr::select(-!!response) %>%
        dplyr::mutate(
          !!response := response) %>%
        dplyr::select(-response)
      }

      else{

        .data <- (1:length(.data)) %>%
          purrr::map(function(lengthi){
            .data %>% magrittr::extract2(lengthi) %>%  dplyr::mutate(id = lengthi)
          })

        data <- dplyr::bind_rows(.data)%>% dplyr::ungroup()
        response_sample <-  sample(data[[response]], size = nrow(data))

        data_sample <- data %>%
          dplyr::mutate(response = response_sample)%>%
          dplyr::select(-!!response) %>%
          dplyr::mutate(
            !!response := response) %>%
          dplyr::select(id, dplyr::everything(), - response)

        data_sample <- split(data_sample, data_sample$id)
        data_sample <- purrr::map(data_sample, ~ (.x %>% select(-1)))
      }

  data_sample %>%
    rank_harmony(harmony_tbl = harmonies,
                 response = response,
                 create_gran_data = create_gran_data,
                 dist_ordered = dist_ordered,...) %>%
    dplyr::select(MMPD)
    })

MMPD_sample <- (1:nsamp) %>%
  purrr::map(function(i){
    MMPD_sample_lst %>% magrittr::extract2(i) %>%  dplyr::select(MMPD)
  })

# maxpd_sample <- (1:nsamp) %>%
#   purrr::map(function(i){
#     MMPD_sample_lst %>% magrittr::extract2(i) %>%  dplyr::select(max_pd)
#   })

  right_quantile_MMPD <- stats::quantile(unlist(MMPD_sample), probs = 0.9)
  #right_quantile_maxpd <- stats::quantile(unlist(maxpd_sample), probs = 0.9)
  MMPD_tbl <- MMPD_obs %>%
    dplyr::mutate(select_harmony = MMPD > right_quantile_MMPD,
                             gt_MMPD = right_quantile_MMPD)
                      #gt_maxpd = max_pd > right_quantile_maxpd)
                      #
  MMPD_sample <- unlist(MMPD_sample)

  return(list(MMPD_tbl, MMPD_sample))
}

# not relevant now

#   # do it for every harmony pair in the harmony table
#   return_val <- (1:nrow(harmony_tbl)) %>% purrr::map(function(rowi){
#     cyc_grans <- harmony_tbl%>% magrittr::extract(rowi,)
#     facet_var <- cyc_grans$facet_variable
#     x_var <- cyc_grans$x_variable
#
#     # MMPD sample values for each harmony pair
#     z <- pvalue_harmony_pair(.data, gran1 = facet_var, gran2 = x_var, response)
#
#     # obs value of MMPD for every harmony pair
#     data_pair <- create_gran_pair(.data,
#                                   gran1 = facet_var,
#                                   gran2 = x_var,
#                                   hierarchy_tbl) %>%
#       tibble::as_tibble()
#
#     obs <- data_pair %>%
#       dplyr::select(facet_var, x_var, !!response) %>%
#       dplyr::mutate(
#         response = .data[[response]]
#       ) %>%
#       dplyr::select(-!!response) %>%
#       tidyr::pivot_wider(names_from = facet_var,
#                          values_from = response,
#                          values_fn = list(response = list)) %>%
#       dist_harmony_pair()
#
#     MMPD_obs <- obs$val
#
#     # get MMPD samples for all pairs
#     right_quantile <- stats::quantile(unlist(z), probs = 0.95)
#     #MMPD_obs > right_quantile
#     right_quantile
#   })
#
#   return_val_un <- unlist(return_val)
#   #return_val_obs <- unlist(MMPD_obs)
#   harmony_tbl %>%
#     dplyr::mutate(threshold = return_val_un)
# }

# pvalue_harmony_pair <- function(.data = NULL,
#                                 gran1 = NULL,
#                                 gran2 = NULL,
#                                 response = NULL,
#                                 size =NULL,
#                                 hierarchy_tbl = NULL,  test = "median", tau = 0.95, r = 500, probs = 0.95,...)
# {
#   if(is.null(size)){
#     size = length(.data)
#   }
#   data_pair <- create_gran_pair(.data, gran1, gran2, hierarchy_tbl) %>% tibble::as_tibble()
#
#
#   MMPD_sample_lst <- (1:5) %>%
#     purrr::map(function(i){
#
#       # get the sample
#
#       response_sample <- sample(data_pair[[response]], nrow(data_pair))
#       MMPD_sample <- data_pair %>%
#         dplyr::select(!!gran1, !!gran2, !!response) %>%
#         # get data in required format for each sample
#         dplyr::mutate(
#           response = response_sample
#         ) %>%
#         dplyr::select(-!!response) %>%
#         tidyr::pivot_wider(names_from = !!gran1,
#                            values_from = response,
#                            values_fn = list(response = list)) %>%
#         # compute MMPD for each of these random sample
#         dist_harmony_pair()
#
#       MMPD_sample$val
#     })
#
#   MMPD_sample_lst
#   #right_quantile <- stats::quantile(unlist(MMPD_sample_lst), probs)
#   #MMPD_obs > right_quantile
# }
#
