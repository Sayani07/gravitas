#' Plotting probability distributions across granularities
#'
#' Plot probability distribution of univariate series across bivariate temporal granularities.
#'
#' @param .data a tsibble.
#' @param harmony_tbl A tibble of harmonies and their levels obtained from the function().
#' @param response response variable.
#' @param prob numeric vector of probabilities with values in [0,1].
#' @param ... other arguments to be passed for customising the obtained ggplot object.
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
#' library(philentropy)
#' harmonies <- smart_meter10 %>% harmony(
#'  ugran = "week",
#'  filter_out = c("hhour"))
#'rank_harmony(smart_meter10, harmony_tbl = harmonies,response =  "general_supply_kwh")
#'
#' harmony_tbl <- PBS %>% harmony(ugran = "semester")
#'rank_harmony(PBS, harmony_tbl = harmony_tbl, response = "Cost")
#' @export rank_harmony
# rank harmony table
rank_harmony <- function(.data = NULL,
                         harmony_tbl = NULL,
                         response = NULL,
                         prob = seq(0.01, 0.99, 0.01))
{
  #step1_data <- step1(.data, harmony_tbl, response)

  dist_harmony_data <- dist_harmony_tbl(.data, harmony_tbl, response, prob)

  mean_max <- unlist(dist_harmony_data)
  harmony_sort <- harmony_tbl %>%
    dplyr::mutate(mean_max_variation = round(mean_max,2)) %>%
    dplyr::arrange(dplyr::desc(mean_max_variation))


  harmony_sort
}


# loop through all harmony pairs in the harmony table
# uses dist_harmony_pair used for calculating max pairiwise
# distance for one harmony pair

dist_harmony_tbl <- function(.data, harmony_tbl, response, prob){
  step1_data <- step1(.data, harmony_tbl, response)
  (1: length(step1_data)) %>%
    purrr::map(function(rowi){
      step_datai <- step1_data %>%
        magrittr::extract2(rowi)
      dist_harmony_pair(step_datai, prob)
    })
}

# average of max pairwise distance for one harmony pair
dist_harmony_pair <-function(step1_datai, prob)
{
  colNms <- colnames(step1_datai)[2:ncol(step1_datai)]

  step2 <- NULL

  for (i in 1:length(colNms)) {
    step2[[i]] <- lapply(step1_datai[[colNms[i]]], quantile_extractx)
  }

  #rowTibb <- step1_datai
  step3 <- rep(list(diag(nrow(step1_datai))), length(colNms))
  #step4 <- matrix(NA, ncol = nrow(rowTibb), nrow = length(colNms))
  step4 <- array(NA, dim = length(colNms))
  ## Logic
  # for each of the list 7 DOW
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
        # row_of_col_max[j] <- max(dist[, j])
        row_of_col_max <- max(dist)
        # maximum of the entire matrix
      }
    }
    #step3[[k]] <- dist
    step4[k] <- row_of_col_max
    #step5 <- mean(step4)
  }
  mean(step4)
  #return(step5)
}

# create two granularities at once
create_gran_pair <-  function(.data, gran1, gran2)
{
  .data %>%
    create_gran(gran1) %>%
    create_gran(gran2)
}


#harmony_data <-create_harmony_data(.data, harmony_tbl, response)

# step1 for each element of the list formed
step1 <- function(.data, harmony_tbl, response){

  harmony_data <-create_harmony_data(.data, harmony_tbl, response)

  (1: length(harmony_data)) %>%
  purrr::map(function(rowi){
    harmony_datai <- harmony_data %>% magrittr::extract2(rowi)
    namesi <- names(harmony_datai)

 #responsei <- create_harmony_datai[[response]]

    harmony_datai %>%
       tidyr::pivot_wider(names_from = namesi[2],
               values_from = response)
               #values_fn = list( responsei = list))
  })
}

# create data for each row of harmony table
# a list created with a tsibble in each element corresponding to each row of the harmony table
# create_harmony_data(smart_meter10, harmony_tbl, "general_supply_kwh")
create_harmony_data <- function(.data = NULL, harmony_tbl = NULL, response = NULL)
{
  (1:nrow(harmony_tbl)) %>% purrr::map(function(rowi){
    .data %>% create_gran_pair(harmony_tbl$facet_variable[rowi],
                               harmony_tbl$x_variable[rowi]) %>%
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
