#' Get combination of quarter component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @param granularity the granularity to be paired up with quarter
#' @param ... other arguments to be passed for appropriate labels
#' @return combination of the quarter component of x as a number
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' tsibbledata::aus_elec %>% mutate(quarter_year = gquarter(Time, "year")) %>% tail()
#' gquarter(lubridate::now(), "semester")
#' @export gquarter
gquarter <- function(x, granularity = "year", ...) {
  # match the gran_type
  gran_lower <- tolower(granularity)

  gran_opt <- c("semester", "year")


  if (!gran_lower %in% gran_opt) {
    stop(paste0("granularity ", gran_lower, " is not one of ", paste0(gran_opt, collapse = ", ")), call. = F)
  }

  # match the gran_type
  gran_type <- match.arg(gran_lower, gran_opt)

  if (gran_type == "semester") {
    gquarter_value <- lubridate::quarter(x) %% 2
  }
  else {
    gquarter_value <- lubridate::quarter(x, ...)
  }

  return(gquarter_value)
}
