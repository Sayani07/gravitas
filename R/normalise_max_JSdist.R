# distance matrix between categories across x-axis

# x: vector/matrix of multiple observations across categories
# x = step1_data[[16]][-1][1,]
quantile_extractx_n <-function(x,
                               prob = seq(0.01, 0.99, by = 0.01)){
  names(x) <- paste0("L", names(x))
  lencol <- ncol(x)
  lenrow <- nrow(x)

 (1:lencol) %>%
    purrr::map_dfr(function(coli){
       x %>%
        magrittr::extract2(coli) %>%
        unlist() %>%
        quantile_extractx(., prob)
    })
}

# y = quantile_extractx_n(x)
JSdist_pair_matrix <- function(y,
                               dist_ordered = TRUE,
                               ...){
  nrowy <- nrow(y)

dist <- (1:(nrowy - 1)) %>%
  purrr::map(function(i){
    ((i + 1):nrowy) %>%
      purrr::map(function(j){
        m1 <- y[i,]
        m2 <- y[j,]
        z = JS(prob, m1, m2)
        if (dist_ordered) {
          if (j != i + 1)
         z = NA
        }
 return(z)
  })
  })
}

# algorithm for normalisation
# z = JSdist_pair_matrix(y)
JSdist_normalise_matrix <- function(z,
                                    dist_distribution = 'normal')
{
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

normalise_max_JSdist <- function(harmony_datai){

# iterate for all facet_levels
iter <- nrow(harmony_datai)

(1:iter) %>%
    purrr::map(function(i){
      harmony_datai %>%
        dplyr::slice(i) %>%
        dplyr::select(-1) %>%
        quantile_extractx_n() %>%
        JSdist_pair_matrix() %>%
        JSdist_normalise_matrix()
    })
}

#dist_matrix_x(harmony_datai)
