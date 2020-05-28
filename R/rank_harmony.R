#' Plotting probability distributions across granularities
#'
#' Plot probability distribution of univariate series across bivariate temporal granularities.
#'
#' @param .data a tsibble.
#' @param harmony_tbl A tibble of harmonies and their levels obtained from the function().
#' @param response response variable.
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
#' harmonies <- sm %>%
#' harmony(ugran = "month",
#'        filter_in = "wknd_wday",
#'        filter_out = c("hhour", "fortnight"))
#' .data = sm
#' response  = "general_supply_kwh"
#' harmony_tbl =  harmonies
#' smart_harmony <- .data %>% rank_harmony(harmony_tbl = harmonies,
#' response = "general_supply_kwh")
#' harmony_tbl <- PBS %>% harmony(ugran = "year")
#'rank_harmony(PBS, harmony_tbl = harmony_tbl, response = "Cost")
#' @export rank_harmony
# rank harmony table
rank_harmony <- function(.data = NULL,
                         harmony_tbl = NULL,
                         response = NULL,
                         prob = seq(0.01, 0.99, 0.01),
                         dist_distribution = "chisq",
                         hierarchy_tbl = NULL,
                         alpha = 0.05)
{
  # <- _data <- <- <- step1(.data, harmony_tbl, response)

  dist_harmony_data <- dist_harmony_tbl(.data, harmony_tbl, response, prob, dist_distribution, hierarchy_tbl)

  comp_dist <- dist_harmony_data %>%
    unlist %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    as_tibble(.name_repair = "unique")

# all for n = 100
# taken from Tests for the Exponential, Weibull and Gumbel Distributions Based on the Stabilized Probability Plot
    if(alpha == 0.05){
      galpa <- 0.073
    }
    else if (alpha == 0.1){
      galpa <- 0.066
    }
  else if (alpha == 0.01){
    galpa <- 0.089
    }

  mean_max <- comp_dist$...1
  max_norm_stat <- comp_dist$...2
  harmony_sort <- harmony_tbl %>%
    dplyr::mutate(mean_max_variation = round(mean_max,5),
                  max_norm_s = max_norm_stat) %>%
    dplyr::arrange(dplyr::desc(mean_max_variation)) %>%
    dplyr::filter(!is.na(mean_max_variation),
                  max_norm_s>=galpa) %>%
    dplyr::select(-max_norm_s)

  harmony_sort
}


# loop through all harmony pairs in the harmony table
# uses dist_harmony_pair used for calculating max pairiwise
# distance for one harmony pair

dist_harmony_tbl <- function(.data, harmony_tbl, response, prob,
                             dist_distribution = NULL, hierarchy_tbl = NULL){
  step1_data <- step1(.data, harmony_tbl, response, hierarchy_tbl)
  (1: length(step1_data)) %>%
    purrr::map(function(rowi){
      step_datai <- step1_data %>%
        magrittr::extract2(rowi)
      z <- dist_harmony_pair(step_datai, prob, dist_distribution)
      c(z$val, z$max_norm_stat)
    })
}

# average of max pairwise distance for one harmony pair
dist_harmony_pair <-function(step1_datai,
                             prob = seq(0.01, 0.99, 0.01),
                             dist_distribution = "normal")
{
  colnames(step1_datai) <- paste0("L",colnames(step1_datai))
  colNms <- colnames(step1_datai)[2:ncol(step1_datai)]

  step2 <- NULL
  for (i in 1:length(colNms)) {
    step2[[i]] <- lapply(step1_datai[[colNms[i]]], quantile_extractx)
  }

  #rowTibb <- step1_datai
  step3 <- rep(list(diag(nrow(step1_datai))), length(colNms))
  #step4 <- matrix(NA, ncol = nrow(rowTibb), nrow = length(colNms))
  step4 <- array(NA, dim = length(colNms))
  prob <- array(NA, dim = length(colNms))
  a <- array(NA, dim = length(colNms))
  new_a <- array(NA, dim = length(colNms))
  b <- array(NA, dim = length(colNms))
  mu <- array(NA, dim = length(colNms))
  sigma <- array(NA, dim = length(colNms))
  alpha <- array(NA, dim = length(colNms))
  beta <- array(NA, dim = length(colNms))
  lambda <- array(NA, dim = length(colNms))
  K <- array(NA, dim = length(colNms))
  cd <- array(NA, dim = length(colNms))
  pd <- array(NA, dim = length(colNms))
  dist_vector <- vector()
  ## Logic
  #__ find the stepped sum difference of density vector elements
  for (k in 1:length(colNms)){

    dist <- matrix(NA,
                   nrow = nrow(step1_datai),
                   ncol = nrow(step1_datai)) ## Matrix
    row_of_col_max <- NULL
    for(i in 1:nrow(step1_datai))
    {
      for (j in 1:nrow(step1_datai))
      {
        m1 <- step2[[k]][[i]]
        m2 <- step2[[k]][[j]]
        #message(paste0("K:",k," I:",i," J:", j))
        dist[i, j] <- compute_JSD(m1, m2)
        dist[dist == 0] <- NA
        # row_of_col_max[j] <- max(dist[, j])
        # maximum of the entire matrix
      }
    }

    max_dist <- max(dist, na.rm = TRUE)
    min_dist <- min(dist, na.rm = TRUE)
    # row_of_col_max <- dplyr::if_else(max_dist - min_dist==0, 0, max_dist/(nrow(step1_datai) *(max_dist - min_dist)))


    # Fisher–Tippett–Gnedenko theorem
    # dist[lower.tri(dist)] <- NA
    # len_uniq_dist <- nrow(step1_datai)^2 - length(which(is.na(dist)))
    # prob[k] <- (1- 1/len_uniq_dist)
    # a[k] <- stats::quantile(as.vector(dist), prob = prob[k], type = 8, na.rm = TRUE)
    # step4[k] <- max_dist/a[k]

    #earlier version
    # row_of_col_max <- dplyr::if_else(max_dist - min_dist==0, 0, max_dist/(nrow(step1_datai) *(max_dist - min_dist)))
    # step4[k] <- row_of_col_max


   dist[lower.tri(dist)] <- NA
   len_uniq_dist <- nrow(step1_datai)^2 - length(which(is.na(dist)))
   #dist_vector <- matrix(NA, nrow = length(colNms), ncol = len_uniq_dist)
   prob[k] <- (1- 1/len_uniq_dist)

   mu[k] <- mean(dist, na.rm = TRUE)
   sigma[k] <- stats::sd(dist, na.rm = TRUE)

   if(dist_distribution == "general")
   {
     a[k] <- stats::quantile(as.vector(dist), prob = 1-prob[k], type = 8, na.rm = TRUE)
     step4[k] <- max_dist/a[k]
   }

   if(dist_distribution == "normal")
   {
     a[k] <- stats::quantile(as.vector(dist), prob = prob[k], type = 8, na.rm = TRUE)
   new_a[k] <- mu[k] + sigma[k]*a[k]
   b[k] <- sigma[k]/a[k]
   step4[k] <- dplyr::if_else(len_uniq_dist==1, mu[k], (max_dist - new_a[k])/(b[k]))
   }

   if(dist_distribution == "gamma")
   {
    # fit_dist <- MASS::fitdistr(dist, densfun = "gamma")
    # if(method == "MME")
    # {
   alpha[k] <- (mu[k]/sigma[k])^2
   beta[k] <- sigma[k]^2/mu[k]
   # }
   #  if(method == "MLE")
   #  {
   #    alpha[k] = fit_dist$estimate[[1]]
   #    beta[k] = 1/fit_dist$estimate[[2]]
   #  }
    b[k] <- stats::quantile(as.vector(dist), prob = 1-prob[k], type = 8, na.rm = TRUE)
    a[k] <- 1/beta[k]

   #b[k] <- a[k]*(log(len_uniq_dist) + (alpha[k]-1)*(log(log(len_uniq_dist))) - log(gamma(alpha[k])))
   step4[k] <- dplyr::if_else(len_uniq_dist==1, mu[k], (max_dist -  b[k])/a[k])
   }

   if(dist_distribution == "chisq_98")
   {
     #MASS::fitdistr(dist, densfun = "chi-squared")
     alpha[k] <- length(prob) - 1
     beta[k] <- 1/2
     b[k] <- stats::quantile(as.vector(dist), prob = 1-prob[k], type = 8, na.rm = TRUE)
     a[k] <- 1/beta[k]
     #b[k] <- a[k]*(log(len_uniq_dist) + (alpha[k]-1)*(log(log(len_uniq_dist))) - log(gamma(alpha[k])))
     step4[k] <- dplyr::if_else(len_uniq_dist==1, mu[k], (max_dist -  b[k])/a[k])
   }

   if(dist_distribution == "chisq_1")
   {
     #MASS::fitdistr(dist, densfun = "chi-squared")
     alpha[k] <-  1
     beta[k] <- 1/2
     b[k] <- stats::quantile(as.vector(dist), prob = 1-prob[k], type = 8, na.rm = TRUE)
     a[k] <- 1/beta[k]
     #b[k] <- a[k]*(log(len_uniq_dist) + (alpha[k]-1)*(log(log(len_uniq_dist))) - log(gamma(alpha[k])))
     step4[k] <- dplyr::if_else(len_uniq_dist==1, mu[k], (max_dist -  b[k])/a[k])
   }

   if(dist_distribution == "weibull")
   {
   K[k] <-  (sigma[k]/ mu[k])^(-1.086) #can change later
   lambda[k] <- mu[k]/gamma(1+1/K[k]) #can change later
   b[k] <- stats::quantile(as.vector(dist), prob = 1 - prob[k], type = 8, na.rm = TRUE)
   #cd[k] <- exp(-b[k]/lambda[k])^K[k]
   #pd[k] <- (K[k]/lambda[k])*(b[k]/lambda[k])^(K[k]-1)*exp(-b[k]/lambda[k])^(K[k])
   #a[k] <- cd[k]/ pd[k]
   a[k] <- (((1/lambda[k])^K[k])*K[k]*(b[k]^(K[k]-1)))^(-1)
   step4[k] <- dplyr::if_else(len_uniq_dist==1, mu[k], (max_dist -  b[k])/a[k])
   }
   d<- as.vector(dist)
   d <- d[!is.na(d)]
   dist_vector <- rbind(dist_vector,d)
  }
  row.names(dist_vector)
  normalised_value <- stats::median(step4, na.rm = TRUE)/log(length(colNms))
  max_norm <- max(step4)
  value <- list(val = normalised_value, distvec = dist_vector, max_norm_stat = max_norm)
  value
  #max(step4, na.rm = TRUE)
  #return(step5)
}


# lengthy code


# dist_harmony_pair_gamma <-function(step1_datai, prob)
# {
#   colnames(step1_datai) <- paste0("L",colnames(step1_datai))
#   colNms <- colnames(step1_datai)[2:ncol(step1_datai)]
#
#   step2 <- NULL
#   for (i in 1:length(colNms)) {
#     step2[[i]] <- lapply(step1_datai[[colNms[i]]], quantile_extractx)
#   }
#
#   #rowTibb <- step1_datai
#   step3 <- rep(list(diag(nrow(step1_datai))), length(colNms))
#   #step4 <- matrix(NA, ncol = nrow(rowTibb), nrow = length(colNms))
#   step4 <- array(NA, dim = length(colNms))
#   prob <- array(NA, dim = length(colNms))
#   a <- array(NA, dim = length(colNms))
#   new_a <- array(NA, dim = length(colNms))
#   b <- array(NA, dim = length(colNms))
#   alpha <- array(NA, dim = length(colNms))
#   beta <- array(NA, dim = length(colNms))
#   mu <- array(NA, dim = length(colNms))
#   sigma <- array(NA, dim = length(colNms))
#   K <- array(NA, dim = length(colNms))
#   lambda <- array(NA, dim = length(colNms))
#   cd <- array(NA, dim = length(colNms))
#   pd <- array(NA, dim = length(colNms))
#   ## Logic
#   # for each of the list 7 DOW
#   #__ find the stepped sum difference of density vector elements
#   for (k in 1:length(colNms)){
#
#     dist <- matrix(NA,
#                    nrow = nrow(step1_datai),
#                    ncol = nrow(step1_datai)) ## Matrix
#     row_of_col_max <- NULL
#     for(i in 1:nrow(step1_datai))
#     {
#       for (j in 1:nrow(step1_datai))
#       {
#         m1 <- step2[[k]][[i]]
#         m2 <- step2[[k]][[j]]
#         #message(paste0("K:",k," I:",i," J:", j))
#         dist[i, j] <- compute_JSD(m1, m2)
#         dist[dist == 0] <- NA
#         # row_of_col_max[j] <- max(dist[, j])
#         # maximum of the entire matrix
#       }
#     }
#
#     max_dist <- max(dist, na.rm = TRUE)
#     min_dist <- min(dist, na.rm = TRUE)
#
#     dist[lower.tri(dist)] <- NA
#     len_uniq_dist <- nrow(step1_datai)^2 - length(which(is.na(dist)))
#     prob[k] <- (1- 1/len_uniq_dist)
#     mu[k] <- mean(dist, na.rm = TRUE)
#     sigma[k] <- stats::sd(dist, na.rm = TRUE)
#     alpha[k] <- (mu[k]/sigma[k])^2
#     beta[k] <- sigma[k]^2/mu[k]
#     a[k] <- 1/beta[k]
#     b[k] <- a[k]*(log(len_uniq_dist) + (alpha[k]-1)*(log(log(len_uniq_dist))) - log(gamma(alpha[k])))
#     step4[k] <- dplyr::if_else(len_uniq_dist==1, mu[k], (max_dist -  b[k])/a[k])
#   }
#   stats::median(step4, na.rm = TRUE)/log(length(colNms))
#   #max(step4, na.rm = TRUE)
#   #return(step5)
# }
#
#
#
# dist_harmony_pair_weibull <-function(step1_datai, prob)
# {
#   colnames(step1_datai) <- paste0("L",colnames(step1_datai))
#   colNms <- colnames(step1_datai)[2:ncol(step1_datai)]
#
#   step2 <- NULL
#   for (i in 1:length(colNms)) {
#     step2[[i]] <- lapply(step1_datai[[colNms[i]]], quantile_extractx)
#   }
#
#   #rowTibb <- step1_datai
#   step3 <- rep(list(diag(nrow(step1_datai))), length(colNms))
#   #step4 <- matrix(NA, ncol = nrow(rowTibb), nrow = length(colNms))
#   step4 <- array(NA, dim = length(colNms))
#   prob <- array(NA, dim = length(colNms))
#   a <- array(NA, dim = length(colNms))
#   new_a <- array(NA, dim = length(colNms))
#   b <- array(NA, dim = length(colNms))
#   alpha <- array(NA, dim = length(colNms))
#   beta <- array(NA, dim = length(colNms))
#   mu <- array(NA, dim = length(colNms))
#   sigma <- array(NA, dim = length(colNms))
#   K <- array(NA, dim = length(colNms))
#   lambda <- array(NA, dim = length(colNms))
#   cd <- array(NA, dim = length(colNms))
#   pd <- array(NA, dim = length(colNms))
#
#
#   for (k in 1:length(colNms)){
#
#     dist <- matrix(NA,
#                    nrow = nrow(step1_datai),
#                    ncol = nrow(step1_datai)) ## Matrix
#     row_of_col_max <- NULL
#     for(i in 1:nrow(step1_datai))
#     {
#       for (j in 1:nrow(step1_datai))
#       {
#         m1 <- step2[[k]][[i]]
#         m2 <- step2[[k]][[j]]
#         #message(paste0("K:",k," I:",i," J:", j))
#         dist[i, j] <- compute_JSD(m1, m2)
#         dist[dist == 0] <- NA
#         # row_of_col_max[j] <- max(dist[, j])
#         # maximum of the entire matrix
#       }
#     }
#
#     max_dist <- max(dist, na.rm = TRUE)
#     min_dist <- min(dist, na.rm = TRUE)
#
#     dist[lower.tri(dist)] <- NA
#     len_uniq_dist <- nrow(step1_datai)^2 - length(which(is.na(dist)))
#     prob[k] <- (1- 1/len_uniq_dist)
#     mu[k] <- mean(dist, na.rm = TRUE)
#     sigma[k] <- stats::sd(dist, na.rm = TRUE)
#     K[k] <-  (sigma[k]/ mu[k])^(-1.086)
#     lambda[k] <- mu[k]/gamma(1+1/K[k])
#     b[k] <- stats::quantile(as.vector(dist), prob = prob[k], type = 8, na.rm = TRUE)
#     cd[k] <- exp(-b[k]/lambda[k])^K[k]
#     pd[k] <- (K[k]/lambda[k])*(b[k]/lambda[k])^(K[k]-1)*exp(-b[k]/lambda[k])^(K[k])
#     a[k] <- cd[k]/ pd[k]
#     step4[k] <- dplyr::if_else(len_uniq_dist==1, mu[k], (max_dist -  b[k])/a[k])
#   }
#   stats::median(step4, na.rm = TRUE)/log(length(colNms))
#   #max(step4, na.rm = TRUE)
#   #return(step5)
# }

# create two granularities at once
create_gran_pair <-  function(.data, gran1, gran2, hierarchy_tbl = NULL)
{
  .data %>%
    create_gran(gran1, hierarchy_tbl) %>%
    create_gran(gran2, hierarchy_tbl)
}


#harmony_data <-create_harmony_data(.data, harmony_tbl, response)

# <- for each element of the list formed

step1 <- function(.data, harmony_tbl, response = NULL, hierarchy_tbl = NULL){

  harmony_data <-create_harmony_data(.data, harmony_tbl, response,hierarchy_tbl)

  (1: length(harmony_data)) %>%
  purrr::map(function(rowi){
    harmony_datai <- harmony_data %>% magrittr::extract2(rowi)
    namesi <- names(harmony_datai)

 #responsei <- create_harmony_datai[[response]]

    harmony_datai %>%
      dplyr::mutate(
        response = harmony_datai[[response]]
      ) %>%
      dplyr::select(-!!response) %>%
      tidyr::pivot_wider(names_from = namesi[1],
               values_from = response,
               values_fn = list(response = list))
  })
}

# create data for each row of harmony table
# a list created with a tsibble in each element corresponding to each row of the harmony table
# create_harmony_data(smart_meter10, harmony_tbl, "general_supply_kwh")
create_harmony_data <- function(.data = NULL, harmony_tbl = NULL, response = NULL, hierarchy_tbl = NULL)
{
  (1:nrow(harmony_tbl)) %>% purrr::map(function(rowi){
    .data %>% create_gran_pair(harmony_tbl$facet_variable[rowi],
                               harmony_tbl$x_variable[rowi], hierarchy_tbl) %>%
      tibble::as_tibble() %>%
      dplyr::select(harmony_tbl$facet_variable[rowi],
                    harmony_tbl$x_variable[rowi],
                    .data[[tidyselect::all_of(response)]])
  })
}
# already put
#step1_data <- step1(.data, harmony_tbl, response)

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

#  Rob's code for computing JSD using quantiles
#
# Compute Jensen-Shannon distance
# based on quantiles q and p at probabilities prob
JS <- function(prob,q,p)
{
  # Compute approximate densities
  x <- seq(min(q,p),max(q,p), l=201)
  qpmf <- pmf(x,prob,q)
  ppmf <- pmf(x,prob,p)
  m <- 0.5 * (ppmf + qpmf)
  JS <- suppressWarnings(0.5*(sum(stats::na.omit(ppmf*log(ppmf/m))) +
                                sum(stats::na.omit(qpmf*log(qpmf/m)))))
  return(JS)
}

# Compute approximate discretized density (like a probability mass function)
# at each x (equally spaced) given quantiles q with probabilities p
pmf <- function(x, p, q)
{
  qcdf <- stats::approx(q,p,xout=x,yleft=0,yright=1, ties = mean)$y
  qpmf <- c(0,diff(qcdf)/ (x[2]-x[1]))
  return(qpmf / sum(qpmf))
}

compute_JSD <- function(x, y, prob = seq(0.01, 0.99, 0.01))
{
   JSD <- JS(prob, x, y)
  return(JSD)
}

quantile_extractx <- function(x =  NULL, prob = seq(0.01, 0.99, by = 0.01))
{
  stats::quantile(x, prob, type=8, na.rm = TRUE)
}
