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


h_q = function(x)

{
  day_quarter <- lubridate::quarter(x)
  hour_quarter <- lubridate::hour(x) + 24*(day_quarter-1)
  hour_quarter
}

h_sem = function(x)

{
  day_semester <- lubridate::semester(x)
  hour_semester <- lubridate::hour(x) + 24*(day_semester-1)
  hour_semester
}

h_y = function(x)

{
  day_year <- lubridate::yday(x)
  hour_year <- lubridate::hour(x) + 24*(day_year-1)
  hour_year
}
