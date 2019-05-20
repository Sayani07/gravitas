#' Get combination of day component of a date time
#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @param granularity the granularity to be paired up with day
#' @param ... other arguments to be passed for appropriate labels
#' @return combination of the day component of x as a number
#' @examples
#' tsibbledata::aus_elec %>% mutate(day_week = gday(Time, "week")) %>% tail()
#' gday(lubridate::now(), "month")
#' @export gday
gday <- function(x, granularity = "week", ...) {
  # lookup_tbl to be used for gran hour
  lookup_l1 <- lookup_tbl("day")

  # Pick up the possible granularities from lookup table
  gran_opt <- lookup_l1$gran_possible

  # check if the user input is correct
  gran_type <- match.arg(granularity, choices = gran_opt, several.ok = TRUE)

  # Match the input granularity from the lookup_tbl
  lookup_l2 <- lookup_tbl(granularity)$match_day

  gday_value <- eval(parse_exp(lookup_l2))

  return(gday_value)
}
