#' Get combination of seconds component of a date time
#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#' @param x a date-time object
#' @param granularity the granularity to be paired up with seconds
#' @param ... other arguments to be passed for appropriate labels
#' @return combination of the seconds component of x as a number
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' tsibbledata::nyc_bikes %>% mutate(sec_hour = gsecond(start_time, "hour")) %>% tail()
#' gsecond(lubridate::now(), "day")
#' @export gsecond
gsecond <- function(x, granularity = "hour", ...) {

  # if (!tsibble::is_tsibble(data)) {
  #   stop("must use tsibble")
  # }
  #
  # # Pick up the time index varible form the tsibble
  # x <- data[[rlang::as_string(tsibble::index(data))]]

  lookup_l1 <- lookup_all("second")
  # Pick up the possible granularities from lookup table
  gran_opt <- lookup_l1$order_up

  # check if the user input is correct
  gran_type <- match.arg(granularity, choices = gran_opt, several.ok = TRUE)

  # Match the input granularity from the lookup_tbl
  lookup_l2 <- lookup_all(granularity)$match_day


  if (gran_type == "minute") {
    gsec_value <- eval(parse_exp(lookup_l1$lub_match))
  }
  else if (gran_type %in% c("qhour", "hhour", "hour")) {
    gsec_value <- eval(parse_exp(lookup_l1$lub_match)) + 60 * gminute(x, gran_type)
  }
  else if (gran_type == "day") {
    gsec_value <- sec_d(x)
  }
  else if (gran_type == "semester") {
    gsec_value <- sec_d(x) + 24 * 60 * (d_sem(x) - 1)
  }
  else {
    match_value <- eval(parse_exp(lookup_l2))
    gsec_value <- sec_d(x) + 24 * 60 * (match_value - 1)
  }

  return(gsec_value)
}


sec_d <- function(x) {
  lubridate::second(x) + (lubridate::hour(x) - 1) * 60 * 60
}
