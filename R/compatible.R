#' Get compatibility tables for two granularities
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param data a tsibble object
#' @param fn1 the first granularity function to use
#' @param fn2 the first granularity function to use
#' @param argfn1 granularity to be fed in fn1
#' @param argfn2 granularity to be fed in fn2
#'
#' @return compatibility table providing if the two granularities are harmonies or clashes. It also provides information on the range of the number of observations per combination and variation across number of combinations and other summery statistics.
#'
#' #' @examples
#' #' tsibbledata::aus_elec %>% dplyr::mutate(hour_day = ghour(Time, "day"), day_week = gday(Time, "week")) %>% compatibility("hour_day", "day_week")
#'


compatibility <- function(.data, ...) {
  UseMethod("compatibility")
}

compatibility.tbl_ts <- function(.data, level1, level2, ...)

{

#   exprs <- enexprs(..., .named = TRUE)
# if (is_empty(exprs)) {
#   attr(.data, "index2") <- index(.data)
#   return(.data)
# }
# if (is_false(has_length(exprs, 1))) {
#   abort("`index_by()` only accepts one expression.")
# }
# expr_name <- names(exprs)[1]
#
# idx <- index(.data)
# idx_chr <- as_string(idx)
#
# if (identical(idx_chr, expr_name)) {
#   abort(sprintf("Column `%s` (index) can't be overwritten.", idx_chr))
# }
#
# idx2 <- sym(expr_name)
#
# expr_name


  if (!tsibble::is_tsibble(.data)) {
    stop("must use tsibble")
  }

  ind <- .data[[rlang::as_string(tsibble::index(.data))]]

  # All possible combinations that are possible
  Allcomb <- .data %>% tidyr::expand(level1, level2)
  # All possible combinations that  exist
  combexist <- .data %>% tidyr::expand(tidyr::nesting(level1, level2))
  # All possible combination that are missing
  cmbmiss <- Allcomb %>% dplyr::anti_join(combexist)

  Output = list()

  Output$data <- .data %>% mutate(L1 = .data[[level1]], L2 = .data[[level2]])


  Output$Type <- Type <- dplyr::if_else(nrow(cmbmiss) != 0, "Clashes", "Harmonies")

  Output$Missing_comb <- cmbmiss

  Obs_per_possible_combn <- .data %>% dplyr::group_by(L1 = .data[[level1]], L2 = .data[[level2]]) %>% dplyr::tally()

  Output$Obs_per_possible_combn <- Obs_per_possible_combn

  Output$Summary <- summary(Obs_per_possible_combn$n)

  Output
}
