#' Get combination of half-hour component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @param granularity the granularity to be paired up with half-hour
#' @param ... other arguments to be passed for appropriate labels
#' @return combination of the hour component of x as a number

#' @examples
#' \dontrun{
#' tsibbledata::aus_elec %>% mutate(hh_day = ghour(Time, "day")) %>% tail()
#' ghhour(lubridate::now(), "week")
#' }
#' @export ghhour
ghhour <- function(x, granularity = "hour", ...) {
  # match the gran_type
  gran_lower <- tolower(granularity)
  gran_opt <- c("hour", "day", "week", "month", "quarter", "semester", "year")

  # check if the user input is correct
  if (!gran_lower %in% gran_opt) {
    stop(paste0("granularity ", gran_lower, " is not one of ", paste0(gran_opt, collapse = ", ")), call. = F)
  }

  gran_type <- match.arg(gran_lower, choices = gran_opt, several.ok = TRUE)

  gran_type_indx <- match(gran_type, gran_opt)

  lubridate_match <- c("na", "na", "wday", "day", "qday", "na", "yday")


  if (gran_type == "hour") {
    ghalfhour_value <- dplyr::if_else(lubridate::minute(x) < 30, 1, 2)
  }
  else if (gran_type == "day") {
    ghalfhour_value <- hh_d(x)
  }
  else if (gran_type == "semester") {
    ghalfhour_value <- hh_d(x) + 48 * (d_sem(x) - 1)
  }
  else {
    match_value <- eval(parse(text = paste0("lubridate::", lubridate_match[gran_type_indx], "(x)")))
    ghalfhour_value <- hh_d(x) + 48 * (match_value - 1)
  }

  return(ghalfhour_value)
}


hh_d <- function(x) {
  (lubridate::hour(x) * 60 + lubridate::minute(x)) / 30
}
