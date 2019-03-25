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
#' h_w(today())
#' }

first_day_of_month_wday <- function(dy) {
  m = month(dy)
  y = year(dy)
  first_day_month = paste0(y,"-",m,"-","01")
  wday(first_day_month)
}
#' @export
w_m = function(x)
{
  ceiling((day(x) + first_day_of_month_wday(x) - 1) / 7)
}


w_q  =  function(x)
{
  ceiling((qday(x)) / 7)
}

# w_sem = function(x)
#
# {
#   ceiling((day(x) + first_day_of_month_wday(x) - 1) / 7)
# }

w_y = function(x)

{
  week(x)
}
