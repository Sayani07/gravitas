#' Get combinations of half-hour component of a date time
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

hh_d = function(x)
{
  (hour(x)*60 + minute(x))/30
}

hh_w = function(x)
{
  day_week <- wday(x)
  hh_week <- (hour(x)*60 + minute(x))/30 + 48*(day_week-1)
  hh_week
}

hh_m  =  function(x)
{
  day_month <- day(x)
  hh_month <- (hour(x)*60 + minute(x))/30 + 48*(day_month-1)
  hh_month
}

hh_q = function(x)

{
  day_quarter <- quarter(x)
  hh_quarter <- (hour(x)*60 + minute(x))/30 + 48*(day_quarter-1)
  hh_quarter
}

hh_sem = function(x)

{
  day_semester <- semester(x)
  hh_semester <- (hour(x)*60 + minute(x))/30 + 48*(day_semester-1)
  hh_semester
}

hh_y = function(x)

{
  day_year <- yday(x)
  hh_year <- (hour(x)*60 + minute(x))/30 + 48*(day_year-1)
  hh_year
}
