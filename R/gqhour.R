#' Get combination of quarter of an hour component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @param granularity the granularity to be paired up with  quarter of an hour
#' @return combination of the quarter hour component of x as a number
#' @examples
#' \dontrun{
#' tsibbledata::aus_elec %>% mutate(qhour_day = gqhour(Time, "hour")) %>% tail()
#' gqtrhour(lubridate::now(), "day")
#' }
#' @export gqhour
gqhour <- function(x, granularity = "day") {
  # match the gran_type
  gran_lower <- tolower(granularity)
  gran_opt <- c("hhour", "hour", "day", "week", "month", "quarter", "semester", "year")

  # check if the user input is correct

  if (!gran_lower %in% gran_opt) {
    stop(paste0("granularity ", gran_lower, " is not one of ", paste0(gran_opt, collapse = ", ")), call. = F)
  }

  gran_type <- match.arg(gran_lower, choices = gran_opt, several.ok = TRUE)

  gran_type_indx <- match(gran_lower, gran_opt)

  lubridate_match <- c("na", "na", "na", "wday", "day", "qday", "na", "yday")

  if (gran_type == "hhour") {
    gqhour_value <- ceiling(qh_h(x) %% 2)
  }
  else if (gran_type == "hour") {
    gqhour_value <- qh_h(x)
  }
  else if (gran_type == "day") {
    gqhour_value <- qh_d(x)
  }
  else if (gran_type == "semester") {
    gqhour_value <- qh_d(x) + 24 * 4 * (gravitas:::d_sem(x) - 1)
  }

  else {
    match_value <- eval(parse(text = paste0("lubridate::", lubridate_match[gran_type_indx], "(x)")))

    gqhour_value <- qh_d(x) + 24 * 4 * (match_value - 1)
  }

  return(gqhour_value)
}

qh_h <- function(x) {

  # finds which quarter of the hour
  ceiling(lubridate::minute(x) / 15)
}

qh_d <- function(x) {

  # finds which quarter of the day
  qh_h(x) + 4 * (lubridate::hour(x))
}
