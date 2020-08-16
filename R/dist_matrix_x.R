# distance matrix between categories across x-axis

# x: vector/matrix of multiple observations across categories
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

JSdist_pair_matrix <- function(x,
                               dist_distribution = "normal",
                               dist_ordered = TRUE)





  step2 <- NULL
  for (i in 1:lencol) {
    step2[[i]] <- lapply(x[[colNms[i]]], quantile_extractx)
  }

  step3 <- rep(list(diag(lenrow)), lencol)

  step4 <- p <- a <- b <- mu <- sigma <- array(NA, dim = lencol)

  dist_vector <- vector()
  ## Logic
  # __ find the stepped sum difference of density vector elements
    dist <- matrix(NA,
                   nrow = lenrow,
                   ncol = lenrow
    ) ## Matrix
    row_of_col_max <- NULL
    for (i in 1:(lenrow - 1))
    {
      for (j in (i + 1):lenrow)
      {
        m1 <- step2[[k]][[i]]
        m2 <- step2[[k]][[j]]
        dist[i, j] <- JS(prob, m1, m2)
        dist[dist == 0] <- NA
        if (dist_ordered) {
          if (j != i + 1) dist[i, j] <- NA
        }
      }
    }
}

# algorithm for normalisation
JSdist_matrix_normalise <-

    max_dist <- max(dist, na.rm = TRUE)

    dist[lower.tri(dist)] <- NA
    len_uniq_dist <- lenrow^2 - length(which(is.na(dist)))
    p[k] <- (1 - 1 / len_uniq_dist)

    mu[k] <- mean(dist, na.rm = TRUE)
    sigma[k] <- stats::sd(dist, na.rm = TRUE)

    if (dist_distribution == "general") {
      a[k] <- stats::quantile(as.vector(dist), prob = 1 - p[k], type = 8, na.rm = TRUE)
      step4[k] <- max_dist / a[k]
    }

    if (dist_distribution == "normal") {
      b[k] <- stats::qnorm(p = p[k], mean = mu[k], sd = sigma[k])
      a[k] <- 1 / (len_uniq_dist * stats::dnorm(b[k], mean = mu[k], sd = sigma[k]))
      step4[k] <- dplyr::if_else(len_uniq_dist == 1, mu[k], (max_dist - b[k]) / a[k])
    }

    d <- as.vector(dist)
    d <- d[!is.na(d)]
    dist_vector <- rbind(dist_vector, d)
  }

  row.names(dist_vector)

  value <- list(val = nmax_nmax, distvec = dist_vector, max_distance = max_max)
  value
}
