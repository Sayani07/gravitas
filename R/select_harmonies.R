#' Plotting probability distributions across granularities
#'
#' Plot probability distribution of univariate series across bivariate temporal granularities.
#'
#' @param .data a tsibble
#' @param gran1 the granularity which is to be placed across facets. Can be column names if required granularity already exists in the tsibble. For example, a column with public holidays which needs to be treated as granularity, can be included here.
#' @param gran2 the granularity to be placed across x-axis. Can be column names if required granularity already exists in the tsibble.
#' @param hierarchy_tbl A hierarchy table specifying the hierarchy of units and their relationships.
#' @param plot_type type of distribution plot. Options include "boxplot", "lv" (letter-value), "quantile", "ridge" or "violin".
#' @param response response variable to be plotted.
#' @param facet_h levels of facet variable for which facetting is allowed while plotting bivariate temporal granularities.
#' @param quantile_prob numeric vector of probabilities with value in [0,1]  whose sample quantiles are wanted. Default is set to "decile" plot.
#' @param symmetric If TRUE, symmetic quantile area plot is drawn. If FALSE, only quantile lines are drawn instead of area. If TRUE, length of quantile_prob should be odd and ideally the quantile_prob should be a symmetric vector with median at the middle position.
#' @param alpha level of transperancy for the quantile area
#' @param ... other arguments to be passed for customising the obtained ggplot object.
#' @return a ggplot object which can be customised as usual.
#
#' @examples
#' library(tsibbledata)
#' library(ggplot2)
#' library(tsibble)
#' library(lvplot)
#' library(dplyr)
#'library(gravitas)
#'library(tidyverse)
#'library(purrr)
#'library(magrittr)
#'library(philentropy)
#'harmony_tbl <- smart_meter10 %>% harmony(
#'  ugran = "week",
#'  filter_out = c("hhour"))
#'rank_harmony(smart_meter10, harmony_tbl = harmony_tbl, "general_supply_kwh")
#'
#'harmony_tbl <- PBS %>% harmony(
#'  ugran = "year")
#'rank_harmony(PBS, harmony_tbl = harmony_tbl, "Cost")

# rank harmony table
rank_harmony <- function(.data, harmony_tbl, response)
{
  #step1_data <- step1(.data, harmony_tbl, response)

  dist_harmony_data <- dist_harmony_tbl(.data, harmony_tbl, response)

  mean_max <- unlist(dist_harmony_data)
  harmony_sort <- harmony_tbl %>%
    mutate(dist = mean_max) %>%
    arrange(desc(dist))
  harmony_sort
}


# loop through all harmony pairs in the harmony table
# uses dist_harmony_pair used for calculating max pairiwise
# distance for one harmony pair

dist_harmony_tbl <- function(.data, harmony_tbl, response){
  step1_data <- step1(.data, harmony_tbl, response)
  (1: length(step1_data)) %>%
    purrr::map(function(rowi){
      step_datai <- step1_data %>%
        extract2(rowi)
      dist_harmony_pair(step_datai)
    })
}

# average of max pairwise distance for one harmony pair
dist_harmony_pair <-function(step1_datai)
{
  colNms <- colnames(step1_datai)[2:ncol(step1_datai)]

  step2 <- NULL

  for (i in 1:length(colNms)) {
    step2[[i]] <- lapply(step1_datai[[colNms[i]]], density_extractx)
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
    harmony_datai <- harmony_data %>% extract2(rowi)
    namesi <- names(harmony_datai)

 #responsei <- create_harmony_datai[[response]]

    harmony_datai %>%
       pivot_wider(names_from = namesi[2],
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
                    .data[[response]])
  })
}
# already put
#step1_data <- step1(.data, harmony_tbl, response)

# already put
# dist_harmony_data <- dist_harmony_tbl(step1_data)

compute_JSD <- function(x, y, message = FALSE)
{
  mat <- rbind(x, y)
  return(JSD(mat))
}

density_extractx <- function(x)
{
  density(x)$y
}
