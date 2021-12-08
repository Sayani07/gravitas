#' computes wpd for one harmony or harmony table
#' chooses compute_pairwise_norm for smaller levels (<=5)
#' chooses compute_pairwise_norm_scalar for higher levels (>5)
#'
#' @param .data a tsibble or data with already computed categories
#' @param harmony_tbl A tibble containing one or more hamronies with facet_variable, x_variable, facet_levels and x_levels
#' @param dist_ordered if categories are ordered
#' @param quantile_prob numeric vector of probabilities with value #'in [0,1]  whose sample quantiles are wanted. Default is set to #' "decile" plot
#' @param lambda value of tuning parameter for computing weighted
#' @param nperm number of permutations for normalization
#' @param response the response variable
#' @param use_perm should permutation approach for normalization be used
#' @param create_harmony_data a logical value indicating if data corresponding to harmonies to be created or not
#'
#' @examples
#' library(gravitas)
#' library(parallel)
#' library(dplyr)

#' library(tidyr)
#' sm <- smart_meter10 %>%
#'   filter(customer_id %in% c("10006414"))
#' harmonies <- sm %>%
#'   harmony(
#'     ugran = "year",
#'     filter_in = "wknd_wday",
#'     filter_out = c("hhour", "fortnight", "quarter", "semester")
#'   )
#' all_harmony <- wpd(sm,
#'   harmony_tbl = harmonies,
#'   response = general_supply_kwh
#' )
#' harmonies1 <- harmonies %>% mutate(facet_variable = NA)
#'
#'  h = harmonies1 %>% select(-facet_levels) %>% distinct() %>% mutate(facet_levels = NA)
#'  all_harmony <- wpd(sm,
#'   harmony_tbl = h,
#'   response = general_supply_kwh, nperm = 200, use_perm = TRUE
#' )
#'
#' @export
wpd <- function(.data,
                harmony_tbl = NULL,
                response = NULL,
                quantile_prob = seq(0.01, 0.99, 0.01),
                dist_ordered = TRUE,
                lambda = 0.67,
                nperm = 20,
                use_perm = TRUE,
                create_harmony_data = TRUE) {


  facet_levels <- x_levels <- sim_data <- NULL

  # one row or all harmonies of the harmony table

  harmony_data <- create_harmony_tbl_data(.data,
                                          harmony_tbl = harmony_tbl,
                                          response = {{ response }}
  )



  harmony_tbl <- harmony_tbl %>%
    dplyr::group_by(
      facet_variable,
      x_variable
    ) %>%
    dplyr::group_keys() %>%
    left_join(harmony_tbl, by = c("facet_variable", "x_variable"))

  if(all(is.na(harmony_tbl$facet_levels))){
    harmony_tbl_lev <- harmony_tbl %>% dplyr::mutate(lev = dplyr::if_else(x_levels <= 5, "low", "high"))
  } else {
    harmony_tbl_lev <- harmony_tbl %>%
      dplyr::mutate(lev = dplyr::if_else(facet_levels <= 5 & x_levels <= 5, "low", "high"))
  }


  if (!use_perm) {
    lapply(
      harmony_data,
      function(x) {
d = compute_pairwise_norm_scalar(
  x,
  gran_x = "id_x",
  gran_facet = "id_facet",
  response = sim_data,
  quantile_prob,
  dist_ordered,
  lambda
)
x %>% distinct(facet_variable, x_variable) %>% bind_cols(wpd=d)
      }
) %>% dplyr::bind_rows()

  } else {
    parallel::mclapply(
      seq_len(nrow(harmony_tbl_lev)),
      function(x) {
        if (harmony_tbl_lev[x, ]$lev == "high") {
          d = compute_pairwise_norm_scalar(
            harmony_data %>% magrittr::extract2(x),
            gran_x = "id_x",
            gran_facet = "id_facet",
            response = sim_data,
            quantile_prob = quantile_prob,
            dist_ordered = dist_ordered,
            lambda =lambda
          )
        }
        else {
          d = compute_pairwise_norm(
            harmony_data %>% magrittr::extract2(x),
            gran_x = "id_x",
            gran_facet = "id_facet",
            response = sim_data,
            quantile_prob = quantile_prob,
            dist_ordered = dist_ordered,
            lambda = lambda,
            nperm = nperm
          )
        }
        wpd_row <- bind_cols(harmony_data %>% magrittr::extract2(x) %>% distinct(x_variable, facet_variable), wpd =  d)
      }
    ) %>% dplyr::bind_rows()
  }
}
  # wpd <- unlist(value) %>%
  #   tibble::as_tibble()
  #
  # harmony_tbl %>%
  #   dplyr::mutate(wpd = wpd)

