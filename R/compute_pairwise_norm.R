#' Title Compute normalised mmpd
#'
#' @param .data data for which mmpd needs to be calculated
#' @param gran_x granularities mapped across x levels
#' @param gran_facet granularities mapped across facets
#' @param response univarite response variable
#' @param quantile_prob probabilities
#' @param dist_ordered if categories are ordered
#' @param lambda tuning parameter
#' @param nperm number of permutations for normalization
#' @param seed seed considered
#' @return weighted pairwise distances normalized using permutation approach
#'
#' @examples
#' library(tidyverse)
#' library(gravitas)
#' library(parallel)
#' sm <- smart_meter10 %>%
#'   dplyr::filter(customer_id %in% c("10017936"))
#' gran_x <- "week_month"
#' gran_facet <- "wknd_wday"
#' v <- compute_pairwise_norm(sm, gran_x, gran_facet,
#'   response = general_supply_kwh, nperm = 20, lambda = 0.9
#' )
#' # month of the year not working in this setup
#' @export
compute_pairwise_norm <- function(.data,
                                  gran_x = NULL,
                                  gran_facet = NULL,
                                  response = NULL,
                                  quantile_prob =
                                    seq(0.01, 0.99, 0.01),
                                  dist_ordered = TRUE,
                                  lambda = 0.67,
                                  nperm = 100,
                                  seed = 9000) {

  sd <- sd1 <- NULL

  mmpd_raw <- compute_pairwise_max(
    .data, gran_x, gran_facet,
    {{ response }}, quantile_prob,
    dist_ordered,lambda
  )
  .data <- .data %>% dplyr::ungroup()

  shuffle_data <- parallel::mclapply(
    seq_len(nperm),
    function(x) {
      set.seed(seed + x)
      rows <- sample(nrow(.data))
      new_response <- dplyr::select(tibble::as_tibble(.data), {{ response }})[rows, ] %>% dplyr::pull({{ response }})

      names_response <- names(tidyselect::eval_select(dplyr::enquo(response), .data))

      # shuffled data made on the fly
      new_data <- .data %>%
        dplyr::select(-{{ response }}) %>%
        dplyr::mutate(new_response = new_response) %>%
        dplyr::mutate(!!names_response := new_response) %>%
        dplyr::select(-new_response)
      # compute raw on this shuffled data

      shuffle_raw <- compute_pairwise_max(
        new_data, gran_x, gran_facet,
        {{ response }},
        quantile_prob,
        dist_ordered,
        lambda
      ) %>% rlang::set_names("mmpd_raw")
      shuffle_raw
    }
  ) %>% dplyr::bind_rows()

  sd1 <- if_else(sd(shuffle_data$mmpd_raw, na.rm = TRUE)==0,
                 1,
                 sd(shuffle_data$mmpd_raw, na.rm = TRUE))

  val <- (mmpd_raw - mean(shuffle_data$mmpd_raw, na.rm = TRUE)) / sd1
  val
}
