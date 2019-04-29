#' Get combination of fortnight component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @param granularity the granularity to be paired up with fortnight
#' @return combination of the week component of x as a number
#' @examples
#' \dontrun{
#' tsibbledata::aus_elec %>% mutate(ft_year = gfortnight(Time, "year")) %>% tail()
#' gfortnight(lubridate::now(), "month")
#' }
#' @export gfortnight
gfortnight <- function(x, granularity = "month") {
  # match the gran_type
  gran_lower <- tolower(granularity)

  gran_opt <- c("month", "quarter", "semester", "year")

  # check if the user input is correct
  if (!gran_lower %in% gran_opt) {
    stop(paste0("granularity ", gran_lower, " is not one of ", paste0(gran_opt, collapse = ", ")), call. = F)
  }

  # match the gran_type
  gran_type <- match.arg(gran_lower, gran_opt, several.ok = TRUE)

  gfortnight_value <- ceiling((gday(x, gran_type)) / 14)

  return(gfortnight_value)
}
