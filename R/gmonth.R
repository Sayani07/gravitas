#' Get combination of month component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @param granularity the granularity to be paired up with month
#' @param ... other arguments to be passed for appropriate labels
#' @return combination of the month component of x as a number
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' tsibbledata::aus_elec %>% mutate(month_year = gmonth(Time, "year")) %>% tail()
#' gmonth(lubridate::now(), "quarter")
#' @export gmonth
gmonth <- function(x, granularity = "year", ...) {

  # lookup_tbl to be used for gran hour
  lookup_l1 <- lookup_tbl("month")

  # Pick up the possible granularities from lookup table
  gran_opt <- lookup_l1$gran_possible

  # check if the user input is correct
  gran_type <- match.arg(granularity, choices = gran_opt, several.ok = TRUE)

  # Match the input granularity from the lookup_tbl
  lookup_l2 <- lookup_tbl(granularity)$match_day


  if (gran_type == "quarter") {
    gmonth_value <- lubridate::month(x) %% 3
  }
  else if (gran_type == "semester") {
    gmonth_value <- lubridate::month(x) %% 6
  }
  else {
    gmonth_value <- lubridate::month(x, ...)
  }
  return(gmonth_value)
}
