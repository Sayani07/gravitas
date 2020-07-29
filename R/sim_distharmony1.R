#' Selecting harmonies with significant difference in distributions for two cyclic granularities
#'
#' @param .data a tsibble.
#' @param ntimes the size of the sample
#' @param sim_dist the distribution to be generated using package distributional
#' @param hierarchy_tbl A hierarchy table specifying the hierarchy of units
#
#' @examples
#' library(ggplot2)
#' library(gravitas)
#' library(purrr)
#' library(distributional)
#' library(magrittr)
#' library(tidyr)
#' harmonies <- tibble::tibble(facet_variable = c("A", "B"),x_variable  = c("B","A"), facet_levels = c(2, 3),x_levels = c(3, 2))
#'.data = harmonies[1,]
#'data <- sim_distharmony1(.data, sim_dist =  c(rep(distributional::dist_normal(mu = 2, sigma = 5),3), rep(distributional::dist_normal(mu = 5, sigma = 10), 3)))
#'data %>% select(-dist) %>% unnest(sim_dist) %>%
#'ggplot(aes(x = Var2, y = sim_dist)) +
#'facet_wrap(~Var1) + geom_boxplot()
#'data %>% select(-dist) %>% unnest(sim_dist) %>%
#' ggplot(aes(x = Var1, y = sim_dist)) +
#' facet_wrap'(~Var2) + geom_boxplot()
#'  dat1 <- data %>% select(-dist) %>%
#'  unnest(sim_dist) %>%
#'  pivot_wider(names_from = Var1, values_from  = sim_dist)
#'  dat2 <- data %>% select(-dist) %>%
#'   unnest(sim_dist) %>%
#'   pivot_wider(names_from = Var2, values_from  = sim_dist)
#' @export sim_distharmony1

sim_distharmony1 <- function(.data,
                             ntimes = 500,
                             sim_dist = distributional::dist_normal(mu = 2, sigma = 5)){

  nfacet <- .data$facet_levels
  nx <- .data$x_levels

  len_sim_dist <- sim_dist %>% lengths %>% length()

  # if only one distribution is given, repeat it for all categories

  if(len_sim_dist==1)
  {
    sim_dist = rep(sim_dist, nfacet*nx)
  }

  lev_facet <- paste0(.data$facet_variable, 1:nfacet)
  lev_x <- paste0(.data$x_variable, 1:nx)

  data_1 <- expand.grid(lev_facet, lev_x) %>%
    dplyr::mutate(dist = sim_dist)
  #sim_dist <- dist_normal(mu = 1:6, sigma = 3)

  data_1 %>% dplyr::group_by(Var1, Var2)  %>%
    dplyr::mutate(sim_dist = distributional::generate(dist, ntimes))
}

