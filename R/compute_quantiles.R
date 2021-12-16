#' @title compute quantiles of data across categories
#'
#' @param sim_panel_data data with categories and response variable
#' @param quantile_prob quantiles of reponse variable needed
#'
#' @return data with quantiles of response variable corresponding to categories
#' @author Sayani07
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(distributional)
#' library(tidyr)
#' sim_panel_data <- sim_panel(
#'   nx = 3,
#'   nfacet = 2,
#'   ntimes = 100,
#'   sim_dist = distributional
#'   ::dist_normal(5, 10)
#' ) %>%
#'   unnest(c(data))
#' sim_panel_data %>% ggplot() +
#'   geom_boxplot(aes(x = as.factor(id_x), y = sim_data)) +
#'   facet_wrap(~id_facet)
#' compute_quantiles(sim_panel_data) %>%
#'   unnest(sim_data_quantile) %>%
#'   ggplot() +
#'   geom_boxplot(aes(x = as.factor(id_x), y = unlist(sim_data_quantile))) +
#'   facet_wrap(~id_facet)
#'
#' sim_varx_normal <- function(nx, nfacet, mean, sd, w) {
#'   rep(dist_normal((mean + seq(0, nx - 1, by = 1) * w), sd), nfacet)
#' }
#' data <- sim_panel(
#'   nx = 3,
#'   nfacet = 2,
#'   ntimes = 100,
#'   sim_dist = sim_varx_normal(nx = 3, nfacet = 2, mean = 0, sd = 1, w = 100)
#' )
#' sim_panel_data %>% ggplot() +
#'   geom_boxplot(aes(x = as.factor(id_x), y = sim_data)) +
#'   facet_wrap(~id_facet)
#' compute_quantiles(sim_panel_data) %>%
#'   unnest(sim_data_quantile) %>%
#'   ggplot() +
#'   geom_boxplot(aes(x = as.factor(id_x), y = unlist(sim_data_quantile))) +
#'   facet_wrap(~id_facet)
#' @export

compute_quantiles <- function(sim_panel_data,
                              quantile_prob = seq(0.01, 0.99, 0.01)) {
  sim_data <- id_facet <- id_x <- list_data <- sim_data_quantile <- NULL
  # preprocess the data(quantile_transform)
  sim_panel_data <- sim_panel_data %>%
    dplyr::ungroup()

  sim_panel_data <- sim_panel_data %>%
    dplyr::mutate(sim_data = stats::qqnorm(sim_data, plot.it = FALSE)$x)

  facet <- unique(sim_panel_data$id_facet)
  nfacet <- length(facet)

  # quantile_prob <- seq(0.01, 0.05, 0.01)

  sim_facet_data <- sim_panel_data %>%
    # for each group find quantiles
    dplyr::group_by(id_facet, id_x) %>%
    dplyr::summarize(
      list_data = list(sim_data),
      sim_data_quantile = stats::quantile(unlist(list_data),
        quantile_prob,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-list_data) %>%
    tidyr::nest(sim_data_quantile = sim_data_quantile)
  sim_facet_data
  # put each x on the columns so that pairwise distance could be computed
  # pivot_wider(id_cols = c(1,2,4),
  #             names_from = id_x,
  #             values_from = sim_data_quantile,
  #             values_fn = list(sim_data_quantile = list)) %>%
}
