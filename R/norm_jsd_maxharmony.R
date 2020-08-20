#' Normalise max pairwise distance for one variable
#'
#' @param harmony_datai a tibble with one row whose column consists of lists of numeric response variable
#' @param quantile_prob numeric vector of probabilities with values in [0,1].
#' @param dist_distribution Underlying distribution of distances.
#' @param dist_ordered if levels of the time granularity is ordered.
#' @return normalised max JS distance
#' @export norm_jsd_maxharmony

norm_jsd_maxharmony <- function(harmony_datai, quantile_prob = seq(0.01, 0.99, by = 0.01), dist_distribution = 'normal', dist_ordered = TRUE){

  # iterate for all facet_levels
  iter <- nrow(harmony_datai)

  (1:iter) %>%
    purrr::map(function(i){
      harmony_datai %>%
        dplyr::slice(i) %>%
        dplyr::select(-1) %>%
        norm_jsd_maxpair(quantile_prob, dist_distribution, dist_ordered)
    })
}
