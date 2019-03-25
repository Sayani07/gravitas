#' Get hour of the week component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @return the hour of the week component of x as a number
#
#' @author Sayani Gupta
#' @examples
#' \dontrun{
#' tsibbledata::aus_elec %>% mutate(hour_week = h_w(Time))
#' hour_week(today())
#' }
#' @export
h_w = function(x)
{
  day_week <- lubridate::wday(x)
  hour_week <- lubridate::hour(x) + 24*(day_week-1)
  hour_week
}

h_m  =  function(x)
{
  day_month <- lubridate::day(x)
  hour_month <- lubridate::hour(x) + 24*(day_month-1)
  hour_month
}

