#' Get combination of quarter of an hour component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @param granularity the granularity to be paired up with  quarter of an hour
#' @return combination of the quarter hour component of x as a number
#' @examples
#' tsibbledata::aus_elec %>% mutate(qhour_day = gqhour(Time, "hour")) %>% tail()
#' gqtrhour(lubridate::now(), "day")
#' @export gqhour
gqhour <- function(x, granularity = "day") {

  # lookup_tbl to be used for gran qhour
  lookup_l1 <- lookup_tbl("qhour")

  # Pick up the possible granularities from lookup table
  gran_opt <- lookup_l1$gran_possible

  # check if the user input is correct
  gran_type <- match.arg(granularity, choices = gran_opt, several.ok = TRUE)

  # Match the input granularity from the lookup_tbl
  lookup_l2 <- lookup_tbl(granularity)$match_day


  if (gran_type == "hhour") {
    gqhour_value <- ceiling(qh_h(x) %% 2)
  }
  else if (gran_type == "hour") {
    gqhour_value <- qh_h(x)
  }
  else {
    gqhour_value <- eval(parse_exp(lookup_l1$match_day)) + 24 * 4 * (eval(parse_exp(lookup_l2)) - 1)
  }

  return(gqhour_value)
}

qh_h <- function(x) {

  # finds which quarter of the hour
  ceiling(lubridate::minute(x) / 15)
}

qh_d <- function(x) {

  # finds which quarter of the day
  qh_h(x) + 4 * (lubridate::hour(x))
}
