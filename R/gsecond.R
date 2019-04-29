#' Get combination of seconds component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @param granularity the granularity to be paired up with seconds
#' @param ... other arguments to be passed for appropriate labels
#' @return combination of the seconds component of x as a number
#' @examples
#' \dontrun{
#' tsibbledata::nyc_bikes %>% mutate(m_hour = gsecond(Time, "hour")) %>% tail()
#' gsecond(lubridate::now(), "day")
#' }
#' @export gsecond
gsecond <- function(x, granularity = "hour", ...) {
  # match the gran_type
  gran_lower <- tolower(granularity)
  gran_opt <- c("minute", "qhour", "hhour", "hour", "day", "week", "month", "quarter", "semester", "year")

  # check if the user input is correct
  if (!gran_lower %in% gran_opt) {
    stop(paste0("granularity ", gran_lower, " is not one of ", paste0(gran_opt, collapse = ", ")), call. = F)
  }

  gran_type <- match.arg(gran_lower, choices = gran_opt, several.ok = TRUE)

  gran_type_indx <- match(gran_type, gran_opt)

  lubridate_match <- c("na", "na", "na", "na", "na", "wday", "day", "qday", "na", "yday")

  if (gran_type == "minute") {
    gsec_value <- lubridate::second(x)
  }

  else if (gran_type == "qhour") {
    gsec_value <- gminute(x, "qhour") * 60 + lubridate::second(x)
  }

  else if (gran_type == "hhour") {
    gsec_value <- gminute(x, "hhour") * 60 + lubridate::second(x)
  }
  else if (gran_type == "hour") {
    gsec_value <- gminute(x, "hour") * 60 + lubridate::second(x)
  }
  else if (gran_type == "day") {
    gsec_value <- sec_d(x)
  }
  else if (gran_type == "semester") {
    gsec_value <- sec_d(x) + 24 * 60 * (d_sem(x) - 1)
  }
  else {
    match_value <- eval(parse(text = paste0("lubridate::", lubridate_match[gran_type_indx], "(x)")))
    gsec_value <- sec_d(x) + 24 * 60 * (match_value - 1)
  }

  return(gsec_value)
}


sec_d <- function(x) {
  lubridate::second(x) + (lubridate::hour(x) - 1) * 60 * 60
}
