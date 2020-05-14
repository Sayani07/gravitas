#' Plotting probability distributions across granularities
#'
#' Plot probability distribution of univariate series across bivariate temporal granularities.
#'
#' @param .data a tsibble.
#' @param response response variable.
#' @param gran1 the granularity which is to be placed across facets. Can be column names if required granularity already exists in the tsibble. For example, a column with public holidays which needs to be treated as granularity, can be included here.
#' @param gran2 the granularity to be placed across x-axis. Can be column names if required granularity already exists in the tsibble.
#' @param prob numeric vector of probabilities with values in [0,1].
#' @param hierarchy_tbl A hierarchy table specifying the hierarchy of units
#' @param dist_distribution Underlying distribution of distances. Look at hist_distance()
#' @return  A tibble of harmonies and their levels ranked ion descending order of average maximum pairwise distance of the harmony pairs.
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
#' response  = "general_supply_kwh"
#' .data %>% hist_distance(gran1 = "hour_day", gran2 = "wknd_wday", response = "general_supply_kwh")
#' @export hist_distance
# rank harmony table
hist_distance <- function(.data = NULL,
                         gran1 = NULL,
                         gran2 = NULL,
                         response = NULL,
                         prob = seq(0.01, 0.99, 0.01),
                         hierarchy_tbl = NULL,
                         dist_distribution = "normal")
{

data_gran <- create_gran_pair(.data,
                              gran1,
                              gran2,
                              hierarchy_tbl) %>%
      tibble::as_tibble() %>%
      dplyr::select(!!gran1, !!gran2, !!response)%>%
      tidyr::pivot_wider(names_from = !!gran2,
                         values_from = !!response,
                         values_fn = list(response = list))

   z <- dist_harmony_pair(data_gran, dist_distribution)
   distvector <- z$distvec

new_distvector <-
   distvector %>%
   tibble::as_tibble(.name_repair = "unique") %>%
   dplyr::mutate(newds = dplyr::row_number()) %>%
   tidyr::pivot_longer(-newds, names_to = "histx", values_to = "freqx")


new_distvector %>% ggplot2::ggplot(ggplot2::aes(freqx)) + ggplot2::geom_histogram() + ggplot2::facet_grid(~newds) +
   ggplot2::xlab("pairwise distances") + ggplot2::ylab("relative frequencies")
}
