#' @title computing all within and between facet distances between quantile categories given quantile data
#'
#' @param sim_panel_quantiles quantile data
#' @param dist_ordered if categories are ordered
#' @param quantile_prob numeric vector of probabilities with value #'in [0,1]  whose sample quantiles are wanted. Default is set to #' "decile" plot
#' @param lambda value of tuning parameter for computing weighted pairwise distances
#' @return within and between facet distances
#' @author Sayani07
#' @export distance_all_pairwise
#' @examples
#' library(dplyr)
#' library(parallel)
#' library(ggplot2)
#' library(distributional)
#' library(tidyr)
#' sim_panel_data <- sim_panel(
#'   nx = 2,
#'   nfacet = 3,
#'   ntimes = 5,
#'   sim_dist = distributional
#'   ::dist_normal(5, 10)
#' ) %>%
#'   unnest(c(data))
#' sim_panel_quantiles <-
#'   compute_quantiles(sim_panel_data)
#'
#' distance_all_pairwise(sim_panel_quantiles, lambda = 0.5)
#' dist_data <- distance_all_pairwise(sim_panel_quantiles, lambda = 0.7)
#' # Plot raw distances
#' ggplot(dist_data, aes(x = 1:9, y = value, colour = dist_type)) +
#'   geom_line() +
#'   geom_point()
#' # Plot transformed distances
#' ggplot(dist_data, aes(
#'   x = 1:9, y = trans_value, colour =
#'     dist_type
#' )) +
#'   geom_line() +
#'   geom_point()
distance_all_pairwise <- function(sim_panel_quantiles,
                                  quantile_prob = seq(0.01, 0.99, 0.01),
                                  dist_ordered = TRUE,
                                  lambda = 0.67)
                                  # dist_rel = function(x){1-x}
                                  # relative distance
                                  # additive inverse
# weights = function(x){1/x} multiplicative inverse)
{
  row_number <- id_facet.x <- id_facet.y <- id_x.x <- remove_row <- id_facet_1 <- id_x_1 <- id_facet_2 <- id_x_2 <- value <- id_x.y <- NULL

  dist_type <- NULL
  # ncoly <- sim_panel_quantiles %>%
  #   distinct(id_facet) %>%
  #   nrow()
  # nrowy <- sim_panel_quantiles %>%
  #   distinct(id_x) %>%
  #   nrow()

  # range of i, j and k are defined in this way since some cyclic granularities start from 0 and others from 1 -  it was creating a problem while filtering in m1 and m2, where m2 was leading to a tibble of 0 rows and JS function was failing
  # if (any((class(sim_panel_quantiles$id_x) %in% c("character", "integer")))) {
  #   sim_panel_quantiles$id_x <- as.numeric(sim_panel_quantiles$id_x) %>% factor()
  # }
  # if (any((class(sim_panel_quantiles$id_facet) %in% c("character", "integer")))) {
  #   sim_panel_quantiles$id_facet <- as.numeric(sim_panel_quantiles$id_facet) %>% factor()
  # }


  id_facet_ref <- sim_panel_quantiles$id_facet %>%
    unique() %>%
    tibble::as_tibble() %>%
    rlang::set_names("id_facet") %>%
    dplyr::mutate(id_facet_ref = row_number())

  id_x_ref <- sim_panel_quantiles$id_x %>%
    unique() %>%
    tibble::as_tibble() %>%
    rlang::set_names("id_x") %>%
    dplyr::mutate(id_x_ref = row_number())

  vm <- sim_panel_quantiles %>%
    dplyr::left_join(id_facet_ref, by = "id_facet") %>%
    dplyr::left_join(id_x_ref, by = "id_x") %>%
    dplyr::mutate(row_number = row_number())

  # differences of all combination of row taking two a time need to be computed
  allcomb <- utils::combn(vm$row_number, 2) %>%
    t() %>%
    tibble::as_tibble()

  # define within-facet and between-facet distances
  all_data <- allcomb %>%
    dplyr::left_join(vm, by = c("V1" = "row_number")) %>%
    dplyr::left_join(vm, by = c("V2" = "row_number")) %>%
    dplyr::mutate(dist_type = dplyr::if_else(id_facet_ref.x == id_facet_ref.y,
      "within-facet",
      dplyr::if_else(id_x_ref.x == id_x_ref.y, "between-facet", "uncategorised")
    )) %>%
    dplyr::filter(dist_type != "uncategorised")

  # remove un-ordered within-facet distances if categories are ordered

  if (dist_ordered) {
    all_data <- all_data %>%
      dplyr::mutate(
        remove_row =
          dplyr::if_else((dist_type == "within-facet" &
            (as.numeric(id_x_ref.y) - as.numeric(id_x_ref.x)) != 1), 1, 0)
      ) %>%
      dplyr::filter(remove_row == 0)
  }


  all_dist <- lapply(
    seq_len(nrow(all_data)),
    function(x) {
      distance <- JS(
        prob = quantile_prob,
        unlist(all_data[x, ]$sim_data_quantile.x),
        unlist(all_data[x, ]$sim_data_quantile.y)
      )
    }
  ) %>%
    unlist() %>%
    tibble::as_tibble()


  return_data <- all_data %>%
    dplyr::rename(
      "id_facet_1" = "id_facet.x",
      "id_facet_2" = "id_facet.y",
      "id_x_1" = "id_x.x",
      "id_x_2" = "id_x.y"
    ) %>%
    dplyr::select(
      id_facet_1,
      id_x_1,
      id_facet_2,
      id_x_2,
      dist_type
    ) %>%
    dplyr::bind_cols(all_dist) %>%
    dplyr::mutate(trans_value = dplyr::if_else(dist_type == "within-facet",
      lambda * value,
      (1 - lambda) * value
    ))

  return_data
}

JS <- function(prob, q, p) {
  # Compute approximate densities
  x <- seq(min(q, p), max(q, p), l = 201)
  qpmf <- pmf(x, prob, q)
  ppmf <- pmf(x, prob, p)
  m <- 0.5 * (ppmf + qpmf)
  JS <- suppressWarnings(0.5 * (sum(stats::na.omit(ppmf * log(ppmf / m, base = 2))) +
    sum(stats::na.omit(qpmf * log(qpmf / m, base = 2)))))
  return(JS)
}

# Compute approximate discretized density (like a probability mass function)
# at each x (equally spaced) given quantiles q with probabilities p
pmf <- function(x, p, q) {
  qcdf <- stats::approx(q, p, xout = x, yleft = 0, yright = 1, ties = max, na.rm = TRUE)$y
  qpmf <- c(0, diff(qcdf) / (x[2] - x[1]))
  return(qpmf / sum(qpmf))
}
