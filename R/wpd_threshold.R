#' chooses threshold for a harmony table
#' @param .data a tsibble or data with already computed categories
#' @param harmony_tbl A tibble containing one or more hamronies with facet_variable, x_variable, facet_levels and x_levels
#' @param dist_ordered if categories are ordered
#' @param quantile_prob numeric vector of probabilities with value #'in [0,1]  whose sample quantiles are wanted. Default is set to #' "decile" plot
#' @param lambda value of tuning parameter for computing weighted
#' @param nperm number of permutations for normalization
#' @param response the response variable
#' @param use_perm should permutation approach for normalization be used
#' @param probs threshold probability
#' @param nsamp number of samples considered to compute threshold
#' @param create_harmony_data a logical value indicating if data corresponding to harmonies to be created or not
#'
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
#' all_harmony <- wpd_threshold(sm,
#'   harmony_tbl = harmonies,
#'   response = general_supply_kwh, nsamp = 3
#' )
#' @export
wpd_threshold <- function(.data,
                          harmony_tbl = NULL,
                          response = NULL,
                          quantile_prob = seq(0.01, 0.99, 0.01),
                          dist_ordered = TRUE,
                          lambda = 0.67,
                          nperm = 20,
                          use_perm = TRUE,
                          probs = c(0.9, 0.95, 0.99),
                          nsamp = 100,
                          create_harmony_data = TRUE) {

  wpd_observed <- wpd(.data,
    harmony_tbl,
    response = {{ response }},
    quantile_prob,
    dist_ordered,
    lambda,
    nperm,
    use_perm,
    create_harmony_data
  )

  wpd_sample <- parallel::mclapply((1:nsamp), function(x) {

    if(!create_harmony_data){
      .data = .data %>% bind_rows()
    }


    response_sample <- .data %>%
      tibble::as_tibble() %>%
      dplyr::ungroup() %>%
      dplyr::select({{ response }}) %>%
      dplyr::sample_frac(size = 1)


    data_sample <- .data %>%
      dplyr::select(-{{ response }}) %>%
      dplyr::bind_cols(response = response_sample)

    if(!create_harmony_data){
      data_sample <-  data_sample %>% group_split(nfacet, nx)
    }


    wpd(data_sample,
      harmony_tbl,
      {{ response }},
      quantile_prob,
      dist_ordered,
      lambda,
      nperm,
      use_perm,
      create_harmony_data
    )
  })

  threshold_01 <- stats::quantile(unlist(wpd_sample), probs = 0.99, na.rm = TRUE)

  threshold_02 <- stats::quantile(unlist(wpd_sample), probs = 0.95, na.rm = TRUE)

  threshold_03 <- stats::quantile(unlist(wpd_sample), probs = 0.90, na.rm = TRUE)


  harmony_tbl %>%
    dplyr::bind_cols(value = unlist(wpd_observed)) %>%
    dplyr::rename(wpd = value) %>%
    dplyr::mutate(wpd = round(wpd, 3)) %>%
    dplyr::mutate(
      select_harmony =
        if_else(wpd_observed > threshold_01,
          paste(wpd, "***", sep = " "),
          if_else(wpd_observed > threshold_02, paste(wpd, "**", sep = " "),
            if_else(wpd_observed > threshold_03, paste(wpd, "*", sep = " "), as.character(wpd))
          )
        )
    ) %>%
    dplyr::arrange(-wpd)
}
