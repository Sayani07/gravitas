#' Selecting harmonies with significant difference in distributions for two cyclic granularities
#'
#' @param .data a tsibble.
#' @param response response variable.
#' @param harmony_tbl A tibble of harmonies and their levels obtained from the function().
#' @param prob numeric vector of probabilities with values in [0,1].
#' @param hierarchy_tbl A hierarchy table specifying the hierarchy of units
#
#' @examples
#' library(tsibbledata)
#' library(ggplot2)
#' library(tsibble)
#' library(lvplot)
#' library(dplyr)
#' library(gravitas)
#' library(purrr)
#' library(magrittr)
#' sm <- smart_meter10 %>%
#' filter(customer_id %in% c(10017936))
#' .data = sm
#' gran1 = "wknd_wday"
#' gran2 = "hour_day"
#' response  = "general_supply_kwh"
# .data %>% histogram_distance(gran1 = "wknd_wday", gran2 = "hour_day",
# response = "general_supply_kwh")
# .data %>% histogram_distance(gran1 = "day_week", gran2 = "hour_day",
# response = "general_supply_kwh")

threshold_harmonies <- function(.data = NULL,
                                harmony_tbl = NULL,
                                response = NULL,
                                prob = seq(0.01,0.99, 0.01),
                                hierarchy_tbl = NULL)
{
     # do it for every harmony pair in the harmony table
  return_val <- (1:nrow(harmony_tbl)) %>% purrr::map(function(rowi){
    cyc_grans <- harmony_tbl%>% magrittr::extract(rowi,)
    facet_var <- cyc_grans$facet_variable
    x_var <- cyc_grans$x_variable

    # MMPD sample values for each harmony pair
    z <- pvalue_harmony_pair(.data, gran1 = facet_var, gran2 = x_var, response)

    # obs value of MMPD for every harmony pair
    data_pair <- create_gran_pair(.data,
                                  gran1 = facet_var,
                                  gran2 = x_var,
                                  hierarchy_tbl) %>%
      tibble::as_tibble()

    obs <- data_pair %>%
      dplyr::select(facet_var, x_var, !!response) %>%
      dplyr::mutate(
        response = .data[[response]]
      ) %>%
      dplyr::select(-!!response) %>%
      tidyr::pivot_wider(names_from = facet_var,
                         values_from = response,
                         values_fn = list(response = list)) %>%
      dist_harmony_pair()

    MMPD_obs <- obs$val

    # get MMPD samples for all pairs
    right_quantile <- stats::quantile(unlist(z), probs = 0.95)
   MMPD_obs > right_quantile
  })

  return_val_un <- unlist(return_val)
  #return_val_obs <- unlist(MMPD_obs)
  harmony_tbl %>%
    dplyr::mutate(threshold = return_val_un)
}

pvalue_harmony_pair <- function(.data = NULL,
                                gran1 = NULL,
                                gran2 = NULL,
                                response = NULL,
                                size =NULL,
                                hierarchy_tbl = NULL,  test = "median", tau = 0.95, r = 500, probs = 0.95,...)
{
  if(is.null(size)){
    size = length(.data)
  }

  data_pair <- create_gran_pair(.data, gran1, gran2, hierarchy_tbl) %>% tibble::as_tibble()


  MMPD_sample_lst <- (1:5) %>%
    purrr::map(function(i){

      # get the sample

      response_sample <- sample(data_pair[[response]], nrow(data_pair))
      MMPD_sample <- data_pair %>%
        dplyr::select(!!gran1, !!gran2, !!response) %>%
        # get data in required format for each sample
        dplyr::mutate(
          response = response_sample
        ) %>%
        dplyr::select(-!!response) %>%
        tidyr::pivot_wider(names_from = !!gran1,
                           values_from = response,
                           values_fn = list(response = list)) %>%
        # compute MMPD for each of these random sample
        dist_harmony_pair()

      MMPD_sample$val
    })

  MMPD_sample_lst
  #right_quantile <- stats::quantile(unlist(MMPD_sample_lst), probs)
  #MMPD_obs > right_quantile
}

