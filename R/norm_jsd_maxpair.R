#' Normalise max pairwise distance for one variable
#'
#' @param .data a tibble with one row whose column consists of lists of numeric response variable
#' @param quantile_prob numeric vector of probabilities with values in [0,1].
#' @param dist_distribution Underlying distribution of distances.
#' @param dist_ordered if levels of the time granularity is ordered.
#' @return normalised max JS distance
#' @export norm_jsd_maxpair
norm_jsd_maxpair <- function(.data, quantile_prob =
                               seq(0.01, 0.99, by = 0.01),
                             dist_distribution = "normal",
                             dist_ordered = TRUE) {
  .data %>%
    quantile_extractx_n(quantile_prob) %>%
    JSdist_pair_matrix(dist_ordered) %>%
    JSdist_norm(dist_distribution)
}

# dist_matrix_x(harmony_datai)

# distance matrix between categories across x-axis

# x: vector/matrix of multiple observations across categories
# x = step1_data[[16]][-1][1,]
quantile_extractx_n <- function(x = NULL,
                                quantile_prob = seq(0.01, 0.99, by = 0.01)) {
  names(x) <- paste0("L", names(x))
  lencol <- ncol(x)
  lenrow <- nrow(x)

  (1:lencol) %>%
    purrr::map_dfr(function(coli) {
      x %>%
        magrittr::extract2(coli) %>%
        unlist() %>%
        quantile_extractx(quantile_prob)
    })
}

# y = quantile_extractx_n(x)
JSdist_pair_matrix <- function(y,
                               dist_ordered = TRUE,
                               ...) {
  nrowy <- nrow(y)

  dist <- (1:(nrowy - 1)) %>%
    purrr::map(function(i) {
      ((i + 1):nrowy) %>%
        purrr::map(function(j) {
          m1 <- y[i, ]
          m2 <- y[j, ]
          z <- JS(prob = quantile_prob, m1, m2)
          if (dist_ordered) {
            if (j != i + 1) {
              z <- NA
            }
          }
          return(z)
        })
    })
}

# algorithm for normalisation
# z = JSdist_pair_matrix(y)

JSdist_norm <- function(z,
                        dist_distribution = "normal", dist_ordered = TRUE, ...) {
  dist <- unlist(z)
  max_dist <- max(dist, na.rm = TRUE)

  len_uniq_dist <- length(which(!is.na(dist)))
  p <- (1 - 1 / len_uniq_dist)

  mu <- mean(dist, na.rm = TRUE)
  sigma <- stats::sd(dist, na.rm = TRUE)

  if (dist_distribution == "normal") {
    b <- stats::qnorm(p = p, mean = mu, sd = sigma)
    a <- 1 / (len_uniq_dist * stats::dnorm(b, mean = mu, sd = sigma))
    value <- dplyr::if_else(len_uniq_dist == 1, mu, (max_dist - b) / a)
  }
  return(value)
}
