#' Get compatibility tables for two granularities
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param data a tsibble object
#' @param gran1 the first granularity function to use
#' @param gran2 the first granularity function to use
#' @param response variable for which summary is desired per combination
#' @return compatibility table providing if the two granularities are harmonies or clashes. It also provides information on the range of the number of observations per combination and variation across number of combinations and other summery statistics.
compatibility.tbl_ts <- function(.data, gran1, gran2, response = NULL, ...) {
  #
  #   #   exprs <- enexprs(..., .named = TRUE)
  #   # if (is_empty(exprs)) {
  #   #   attr(.data, "index2") <- index(.data)
  #   #   return(.data)
  #   # }
  #   # if (is_false(has_length(exprs, 1))) {
  #   #   abort("`index_by()` only accepts one expression.")
  #   # }
  #   # expr_name <- names(exprs)[1]
  #   #
  #   # idx <- index(.data)
  #   # idx_chr <- as_string(idx)
  #   #
  #   # if (identical(idx_chr, expr_name)) {
  #   #   abort(sprintf("Column `%s` (index) can't be overwritten.", idx_chr))
  #   # }
  #   #
  #   # idx2 <- sym(expr_name)
  #   #
  #   # expr_name


  if (!tsibble::is_tsibble(.data)) {
    stop("must use tsibble")
  }

  ind <- .data[[rlang::as_string(tsibble::index(.data))]]

  # All possible combinations that are possible
  Allcomb <- .data %>% tidyr::expand(gran1, gran2)
  # All possible combinations that  exist
  combexist <- .data %>% tidyr::expand(tidyr::nesting(gran1, gran2))
  # All possible combination that are missing
  cmbmiss <- Allcomb %>% dplyr::anti_join(combexist)

  # Output <- list()

  # Data <- .data %>% mutate(L1 = .data[[level1]], L2 = .data[[level2]])


  # Output$Type <- Type <- dplyr::if_else(nrow(cmbmiss) != 0, "Clashes", "Harmonies")


  Obs_per_possible_combn <- .data %>% tibble::as_tibble() %>% dplyr::group_by(.data[[gran1]], .data[[gran2]]) %>% dplyr::summarise(
    count = n(), min = stats::fivenum(.data[[response]])[1],
    q1 = stats::fivenum(.data[[response]])[2],
    median = stats::fivenum(.data[[response]])[3],
    q3 = stats::fivenum(.data[[response]])[4],
    max = stats::fivenum(.data[[response]])[5]
  )

  # Output$Missing_comb <- cmbmiss

  combn_table <- Obs_per_possible_combn

  # Output$Summary <- summary(Obs_per_possible_combn$n)

  combn_table
}




