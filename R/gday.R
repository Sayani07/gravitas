#' Get combination of day component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @param granularity the granularity to be paired up with day
#' @param ... other arguments to be passed for appropriate labels
#' @return combination of the day component of x as a number

#' @examples
#' \dontrun{
#' tsibbledata::aus_elec %>% mutate(day_week = gday(Time, "week")) %>% tail()
#' gday(lubridate::now(), "month")
#' }
#' @export gday
gday <- function(x, granularity = "week", ...) {
  gran_lower <- tolower(granularity)

  gran_opt <- c("week", "month", "quarter", "semester", "year")


  # check if the user input is correct
  if (!gran_lower %in% gran_opt) {
    stop(paste0("granularity ", gran_lower, " is not one of ", paste0(gran_opt, collapse = ", ")), call. = F)
  }

  gran_type <- match.arg(gran_lower, choices = gran_opt, several.ok = TRUE)

  lubridate_match <- c("wday", "day", "qday", "na", "yday")

  gran_type_indx <- match(gran_type, gran_opt)

  if (!gran_type == "semester") {
    match_value <- eval(parse(text = paste0("lubridate::", lubridate_match[gran_type_indx], "(x, ...)")))
  }
  else {
    match_value <- d_sem(x)
  }

  return(match_value)
}
