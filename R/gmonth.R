#' Get combination of month component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @param granularity the granularity to be paired up with month
#' @param ... other arguments to be passed for appropriate labels
#' @return combination of the month component of x as a number
#
#' @author Sayani Gupta
#' @examples
#' \dontrun{
#' tsibbledata::aus_elec %>% mutate(month_year = gmonth(Time, "year")) %>% tail()
#' gmonth(lubridate::now(), "quarter")
#' }
#' @export gmonth
gmonth <- function(x, granularity = "year", ...) {
  # match the gran_type
  gran_lower <- tolower(granularity)
  gran_opt <- c("quarter", "semester", "year")

  # check if the user input is correct
  if (!gran_lower %in% gran_opt) {
    stop(paste0("granularity ", gran_lower, " is not one of ", paste0(gran_opt, collapse = ", ")), call. = F)
  }

  # match the gran_type
  gran_type <- match.arg(gran_lower, gran_opt)


  if (gran_type == "quarter") {
    gmonth_value <- lubridate::month(x) %% 3
  }
  else if (gran_type == "semester") {
    gmonth_value <- lubridate::month(x) %% 2
  }
  else {
    gmonth_value <- lubridate::month(x, ...)
  }
  return(gmonth_value)
}
