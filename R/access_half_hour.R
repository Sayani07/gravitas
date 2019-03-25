#' Get half-hour of the week component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @return the half-hour of the week component of x as a number
#
#' @author Sayani Gupta
#' @examples
#' \dontrun{
#' tsibbledata::aus_elec %>% mutate(hh_week = h_w(Time))
#' hh_week(today())
#' }
#' @export

hh_w = function(x)
{
  day_week <- wday(x)
  hh_week <- (hour(x)*60 + minute(x))/30 + 48*(day_week-1)
  hh_week
}

h_m  =  function(x)
{
  day_month <- lubridate::day(x)
  hh_month <- lubridate::hh(x) + 48*(day_month-1)
  hh_month
}


h_q = function(x)

{
  day_quarter <- lubridate::quarter(x)
  hh_quarter <- lubridate::hh(x) + 48*(day_quarter-1)
  hh_quarter
}

h_sem = function(x)

{
  day_semester <- lubridate::semester(x)
  hh_semester <- lubridate::hh(x) + 48*(day_semester-1)
  hh_semester
}

h_y = function(x)

{
  day_year <- lubridate::yday(x)
  hh_year <- lubridate::hh(x) + 48*(day_year-1)
  hh_year
}
