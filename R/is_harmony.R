#' Check if two temporal granularities are harmonies
#'
#' Interaction of pair of granularities, categorised as harmony and clash
#' @param .data A tsibble object.
#' @param gran1 One of the temporal granularities to check for harmonies.
#' @param gran2 The second temporal granularity in the pair.
#' @param hierarchy_tbl A hierarchy table specifying the hierarchy of units and their relationships.
#' @param facet_h highest level of facet variable that can be considered in harmony pair.
#' @param x_h highest level of x-axis variable that can be considered in harmony pair.
#' @return TRUE if two granularties are harmonies.
#' @examples
#' library(tsibbledata)
#' vic_elec %>% is_harmony("hour_day", "day_week")
#' @export is_harmony

is_harmony <- function(.data,
                       gran1,
                       gran2,
                       hierarchy_tbl = NULL,
                       facet_h = NULL,
                       x_h = NULL) {
  # data must be tsibble
  if (!tsibble::is_tsibble(.data)) {
    stop("must use tsibble")
  }

  if (gran1 == gran2) {
    warning("the two granularities should be distinct")
  }

  harmony_object <- gran_tbl(.data, gran1, gran2, hierarchy_tbl)
  names <- names(harmony_object)
  # All possible combination that are missing
  cmbmiss <- any(harmony_object$nobs == 0)
  facet_nlevel <- harmony_object[, 1] %>% dplyr::distinct()
  x_nlevel <- harmony_object[, 2] %>% dplyr::distinct()

  if (is.null(facet_h)) {
    facet_h <- 31
  }
  if (is.null(x_h)) {
    x_h <- 48
  }

  if (cmbmiss == "TRUE" | nrow(facet_nlevel) > facet_h | nrow(x_nlevel) > x_h) {
    return_output <- "FALSE"
  } else {
    return_output <- "TRUE"
  }
  if (gran1 == gran2) {
    return_output <- "FALSE"
  }

  return(return_output)
}


clash_reason <- function(.data,
                         gran1,
                         gran2,
                         hierarchy_tbl,
                         response = NULL,
                         ...) {
  gran_full <- gran_tbl(.data, gran1, gran2, hierarchy_tbl)
  if (any(gran_full$nobs == 0)) {
    clash_combination <- gran_full %>%
      dplyr::filter(nobs == 0) %>%
      dplyr::select(gran1, gran2)

    distinct_gran1 <- gran_full %>%
      dplyr::distinct(gran_full[[gran1]]) %>%
      nrow()
    distinct_gran2 <- gran_full %>%
      dplyr::distinct(gran_full[[gran2]]) %>%
      nrow()

    data_count <- gran_tbl(.data, gran1, gran2, hierarchy_tbl)

    return(list(paste(gran1, "has", distinct_gran1, "distinct levels and", gran2, "has", distinct_gran2, "distinct levels", "with the following structurally empty combinations. They are structurally empty as the structure of calendar does not allow these combinations to appear together."), clash_combination))
  } else {
    return(paste("Good Work! You have chosen harmonies. Go ahead and save the plot using your choice of distribution plot"))
  }
}
