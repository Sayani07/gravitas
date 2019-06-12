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
#'
#' @examples
#' \dotrun{
#' library(dplyr)
#' library(tsibbledata)
#' aus_elec %>% dplyr::mutate(hour_day = ghour(Time, "day"), day_week = gday(Time, "week")) %>% compatibility.tbl_ts("hour_day", "day_week", "Demand")
#' }
#' @export compatibility
compatibility <- function(.data, gran1, gran2, response = NULL, ...) {

  if (!tsibble::is_tsibble(.data)) {
    stop("must use tsibble")
  }

  ind <- .data[[rlang::as_string(tsibble::index(.data))]]

  gran1_split <- str_split(gran1, "_", 2) %>% unlist()
  gran2_split <- str_split(gran2, "_", 2) %>% unlist()


  .data %>% mutate(hhour_week = nest(gran1_split, ind), hhour_week = nest(gran2_split, ind))

  }

