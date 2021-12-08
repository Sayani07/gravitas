#' Title Compute normalised maximum pairwise distances
#'
#' @param .data data for which mmpd needs to be calculated
#' @param gran_x granularities mapped across x levels
#' @param gran_facet granularities mapped across facetss
#' @param response univarite response variable
#' @param quantile_prob probabilities
#' @param dist_ordered if categories are ordered
#' @param nperm number of permutations required for normalisation
#' @param lambda value of the tuning parameter for computing weighted pairwise distances
#' @return the weighted pairwise distance normalised using permutation
#'
#' @examples
#' library(tidyverse)
#' library(gravitas)
#' library(parallel)
#' sm <- smart_meter10 %>%
#'   filter(customer_id %in% c("10017936"))
#' gran_x <- "week_month"
#' gran_facet <- "week_fortnight"
#' v <- compute_norm_pairwise_max(sm, gran_x, gran_facet,
#'   response = general_supply_kwh, nperm = 10
#' )
#' # month of the year not working in this setup
#' @export compute_norm_pairwise_max
compute_norm_pairwise_max <- function(.data,
                                      gran_x = NULL,
                                      gran_facet = NULL,
                                      response = NULL,
                                      quantile_prob =
                                        seq(0.01, 0.99, 0.01),
                                      dist_ordered = TRUE,
                                      nperm = 100,
                                      lambda = 0.67) {

  sd <- NULL

  mmpd_raw <- compute_pairwise_max(
    .data, gran_x, gran_facet,
    {{ response }}, quantile_prob,
    dist_ordered, lambda
  )

  if (!((gran_x %in% names(.data) &
    (gran_facet %in% names(.data))))) {
    .data <- .data %>%
      gravitas::create_gran(gran_x) %>%
      gravitas::create_gran(gran_facet)
  }

  shuffle_data <- parallel::mclapply(
    seq_len(nperm),
    function(x) {
      set.seed(2020 + x)
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
        dist_ordered, lambda
      ) %>% rlang::set_names("mmpd_raw")
      shuffle_raw
    }
  ) %>% dplyr::bind_rows()

  val <- (mmpd_raw - mean(shuffle_data$mmpd_raw, na.rm = TRUE)) / sd(shuffle_data$mmpd_raw, na.rm = TRUE)

  val
}
