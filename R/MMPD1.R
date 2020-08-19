# compute MMPD for one harmony pair
#' Normalise max pairwise distance for one variable
#' @param harmony_datai a tibble with many rows whose column consists of lists of numeric response variable
#' @param quantile_prob numeric vector of probabilities with values in [0,1].
#' @param dist_distribution Underlying distribution of distances.
#' @param dist_ordered if levels of the time granularity is ordered.
#' @param base base of logarithm while normalizing
#' @return  MMPD for one harmony pair
#' @param ... Other arguments passed on to individual methods.
#' @export MMPD1

MMPD1 <- function(harmony_datai, quantile_prob = seq(0.01, 0.99, by = 0.01), dist_distribution = 'normal', dist_ordered = TRUE, base = exp(1),...){
  harmony_datai %>%
    norm_jsd_maxharmony(quantile_prob,
                         dist_distribution,
                         dist_ordered) %>%
    median_by_log()
}

