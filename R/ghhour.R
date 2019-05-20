#' Get combination of half-hour component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @param granularity the granularity to be paired up with half-hour
#' @param ... other arguments to be passed for appropriate labels
#' @return combination of the hour component of x as a number

#' @examples
#' library(ggplot2)
#' library(dplyr)
#' tsibbledata::aus_elec %>% mutate(hh_day = ghhour(Time, "day")) %>% tail()
#' ghhour(lubridate::now(), "week")
#' @export ghhour
ghhour <- function(x, granularity = "hour", ...) {

  # lookup_tbl to be used for gran hour
  lookup_l1 <- lookup_tbl("hhour")

  # Pick up the possible granularities from lookup table
  gran_opt <- lookup_l1$gran_possible


  gran_type <- match.arg(granularity, choices = gran_opt, several.ok = TRUE)

  # Match the input granularity from the lookup_tbl
  lookup_l2 <- lookup_tbl(granularity)$match_day


  if (gran_type == "hour") {
    ghalfhour_value <- eval(parse_exp(lookup_l1$match_hour))
  } else {
    ghalfhour_value <- eval(parse_exp(lookup_l1$match_day)) + 48 * (eval(parse_exp(lookup_l2)) - 1)
  }
  return(ghalfhour_value)
}


hh_d <- function(x) {
  (lubridate::hour(x) * 60 + lubridate::minute(x)) / 30
}


hh_hour <- function(x) {
  dplyr::if_else(lubridate::minute(x) < 30, 1, 2)
}
