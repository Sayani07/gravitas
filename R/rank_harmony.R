#' Plotting probability distributions across granularities
#'
#' Plot probability distribution of univariate series across bivariate temporal granularities.
#'
#' @param .data a tsibble, if cyclic granularity needs to be constructed or a list consisting of tibbles for each pair of cyclic granularity in the harmony table
#' @param harmony_tbl A tibble of harmonies and their levels obtained from the function().
#' @param response response variable.
#' @param prob numeric vector of probabilities with values in [0,1].
#' @param hierarchy_tbl A hierarchy table specifying the hierarchy of units
#' @param dist_distribution Underlying distribution of distances. Look at hist_distance()
#' @param dist_ordered if levels of the time granularity is ordered.
#' @param alpha significance level
#' @param create_gran_data if data corresponding to a pair of cyclic granularity needs to be created
#' @return  A tibble of harmonies and their levels ranked in descending order of average maximum pairwise distance of the harmony pairs.
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
#'   filter(customer_id %in% c("10017936"))
#' harmonies <- sm %>%
#'   harmony(
#'     ugran = "month",
#'     filter_in = "wknd_wday",
#'     filter_out = c("hhour", "fortnight")
#'   )
#' .data <- sm
#' response <- "general_supply_kwh"
#' harmony_tbl <- harmonies
#' smart_harmony <- .data %>% rank_harmony(
#'   harmony_tbl = harmonies,
#'   response = "general_supply_kwh", dist_ordered = TRUE
#' )
#' harmony_tbl <- PBS %>% harmony(ugran = "year")
#' rank_harmony(PBS, harmony_tbl = harmony_tbl, response = "Cost")
#' @export rank_harmony

rank_harmony <- function(.data = NULL,
                         harmony_tbl = NULL,
                         response = NULL,
                         prob = seq(0.01, 0.99, 0.01),
                         dist_distribution = "normal",
                         hierarchy_tbl = NULL,
                         dist_ordered = TRUE,
                         alpha = 0.05,
                         create_gran_data = TRUE) {
  # <- _data <- <- <- step1(.data, harmony_tbl, response)

  dist_harmony_data <- dist_harmony_tbl(.data, harmony_tbl, response, prob, dist_distribution, hierarchy_tbl, dist_ordered, create_gran_data)

  comp_dist <- dist_harmony_data %>%
    unlist() %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    tibble::as_tibble(.name_repair = "unique")

  # all for n = 100
  # taken from Tests for the Exponential, Weibull and Gumbel Distributions Based on the Stabilized Probability Plot
  if (alpha == 0.05) {
    galpa <- 0.073
  }
  else if (alpha == 0.1) {
    galpa <- 0.066
  }
  else if (alpha == 0.01) {
    galpa <- 0.089
  }

  mean_max <- comp_dist$...1
  max_distance <- comp_dist$...2

  harmony_sort <- harmony_tbl %>%
    dplyr::mutate(MMPD = round(mean_max, 2)) %>%
    dplyr::arrange(dplyr::desc(MMPD)) %>%
    # dplyr::mutate(r = rank(-max_pd)) %>%
    # dplyr::filter(max_norm_s>=galpa) %>%
    # dplyr::select(-max_norm_s) %>%
    dplyr::filter(!is.na(MMPD)) %>%
    rename(
      "facet" = "facet_variable",
      "x" = "x_variable",
      "facet_l" = "facet_levels",
      "x_l" = "x_levels"
    )


  harmony_sort
}


# loop through all harmony pairs in the harmony table
# uses dist_harmony_pair used for calculatin%>% max pairiwise
# distance for one harmony pair

dist_harmony_tbl <- function(.data, harmony_tbl, response, prob,
                             dist_distribution = NULL, hierarchy_tbl = NULL, dist_ordered = NULL, create_gran_data = NULL, ...) {
  step1_data <- step1(.data, harmony_tbl, response, hierarchy_tbl, create_gran_data, ...)
  (1:length(step1_data)) %>%
    purrr::map(function(rowi) {
      step_datai <- step1_data %>%
        magrittr::extract2(rowi)
      z <- dist_harmony_pair(step_datai, prob, dist_distribution, dist_ordered, create_gran_data, ...)
      c(z$val, z$max_distance)
    })
}

# average of max pairwise distance for one harmony pair
dist_harmony_pair <- function(step1_datai,
                              prob = seq(0.01, 0.99, 0.01),
                              dist_distribution = "normal",
                              dist_ordered,
                              create_gran_data, ...) {
  colnames(step1_datai) <- paste0("L", colnames(step1_datai))
  colNms <- colnames(step1_datai)[2:ncol(step1_datai)]
  lencol <- length(colNms)
  lenrow <- nrow(step1_datai)

  step2 <- NULL
  for (i in 1:lencol) {
    step2[[i]] <- lapply(step1_datai[[colNms[i]]], quantile_extractx)
  }

  step3 <- rep(list(diag(lenrow)), lencol)

  step4 <- p <- a <- b <- mu <- sigma <- array(NA, dim = lencol)

  dist_vector <- vector()
  ## Logic
  # __ find the stepped sum difference of density vector elements
  for (k in 1:lencol) {
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

  # normalised max of normalised max

  nmax_nmax <- stats::median(step4, na.rm = TRUE) / log(lencol)

  # unnormalised max of normalised max

  max_nmax <- max(step4)

  # normalised max of unormalised max

  nmax_max <- stats::median(max_dist, na.rm = TRUE) / log(lencol)

  # unnormalised max of unormalised max

  max_max <- max(max_dist, na.rm = TRUE)

  value <- list(val = nmax_nmax, distvec = dist_vector, max_distance = max_max)
  value
}

# create two granularities at once
create_gran_pair <- function(.data, gran1, gran2, hierarchy_tbl = NULL) {
  .data %>%
    create_gran(gran1, hierarchy_tbl) %>%
    create_gran(gran2, hierarchy_tbl)
}


# harmony_data <-create_harmony_data(.data, harmony_tbl, response)

# <- for each element of the list formed

step1 <- function(.data, harmony_tbl,
                  response = NULL, hierarchy_tbl = NULL,
                  create_gran_data = NULL, ...) {
  harmony_data <- create_harmony_data(.data, harmony_tbl, response, hierarchy_tbl, create_gran_data)

  (1:length(harmony_data)) %>%
    purrr::map(function(rowi) {
      harmony_datai <- harmony_data %>% magrittr::extract2(rowi)
      namesi <- names(harmony_datai)

      # responsei <- create_harmony_datai[[response]]

      harmony_datai %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          response = harmony_datai[[response]]
        ) %>%
        dplyr::select(-!!response) %>%
        tidyr::pivot_wider(
          names_from = namesi[1],
          values_from = response,
          values_fn = list(response = list)
        )
    })
}

# create data for each row of harmony table
# a list created with a tsibble in each element corresponding to each row of the harmony table
# create_harmony_data(smart_meter10, harmony_tbl, "general_supply_kwh")
create_harmony_data <- function(.data = NULL, harmony_tbl = NULL, response = NULL, hierarchy_tbl = NULL,
                                create_gran_data = TRUE, ...) {
  if (create_gran_data) {
    (1:nrow(harmony_tbl)) %>% purrr::map(function(rowi) {
      .data %>%
        create_gran_pair(
          harmony_tbl$facet_variable[rowi],
          harmony_tbl$x_variable[rowi], hierarchy_tbl
        ) %>%
        tibble::as_tibble() %>%
        dplyr::select(
          harmony_tbl$facet_variable[rowi],
          harmony_tbl$x_variable[rowi],
          .data[[tidyselect::all_of(response)]]
        )
    })
  }
  else {
    return(.data)
  }
}
# already put
# step1_data <- step1(.data, harmony_tbl, response)

# already put
# dist_harmony_data <- dist_harmony_tbl(step1_data)

# compute_JSD <- function(x, y, message = FALSE)
# {
#   mat <- rbind(x, y)
#   return(philentropy::JSD(mat))
# }

# density_extractx <- function(x)
# {
#   stats::density(x)$y
# }



