#' Get combination of week component of a date time
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
first_day_of_month_wday <- function(dy) {
  m = month(dy)
  y = year(dy)
  first_day_month = paste0(y,"-",m,"-","01")
  wday(first_day_month)
}

w_m = function(x)
{
  ceiling((day(x) + first_day_of_month_wday(x) - 1) / 7)
}


w_q  =  function(x)
{
  day_month <- day(x)
  hour_month <- hour(x) + 24*(day_month-1)
  hour_month
}

w_sem = function(x)

{
  day_quarter <- quarter(x)
  hour_quarter <- hour(x) + 24*(day_quarter-1)
  hour_quarter
}

w_y = function(x)

{
  day_semester <- semester(x)
  hour_semester <- hour(x) + 24*(day_semester-1)
  hour_semester
}
