#' Get combination of minutes component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @param granularity the granularity to be paired up with minutes
#' @param ... other arguments to be passed for appropriate labels
#' @return combination of the hour component of x as a number
#' @examples
#' \dontrun{
#' tsibbledata::nyc_bikes %>% mutate(m_hour = gmin(Time, "hour")) %>% tail()
#' gmin(lubridate::now(), "day")
#' }
#' @export ghhour
gmin <- function(x, granularity = "hour", ...) {
  # match the gran_type
  gran_lower <- tolower(granularity)
  gran_opt <- c("qhour", "hhour", "hour", "day", "week", "month", "quarter", "semester", "year")

  # check if the user input is correct
  if (!gran_lower %in% gran_opt) {
    stop(paste0("granularity ", gran_lower, " is not one of ", paste0(gran_opt, collapse = ", ")), call. = F)
  }

  gran_type <- match.arg(gran_lower, choices = gran_opt, several.ok = TRUE)

  gran_type_indx <- match(gran_type, gran_opt)

  lubridate_match <- c("na", "na", "na", "na", "wday", "day", "qday", "na", "yday")

  if (gran_type == "qhour") {
    gmin_value <- lubridate::minute(x) %% 15
  }

  else if (gran_type == "hhour") {
    gmin_value <- lubridate::minute(x) %% 30
  }
  else if (gran_type == "hour") {
    gmin_value <- lubridate::minute(x)
  }
  else if (gran_type == "day") {
    gmin_value <- min_d(x)
  }
  else if (gran_type == "semester") {
    gmin_value <- min_d(x) + 24 * 60 * (d_sem(x) - 1)
  }
  else {
    match_value <- eval(parse(text = paste0("lubridate::", lubridate_match[gran_type_indx], "(x)")))
    gmin_value <- min_d(x) + 24 * 60 * (match_value - 1)
  }

  return(gmin_value)
}


min_d <- function(x) {
  lubridate::minute(x) + (lubridate::hour(x) - 1) * 60
}
