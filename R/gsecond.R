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
#' tsibbledata::nyc_bikes %>% mutate(sec_hour = gsecond(start_time, "hour")) %>% tail()
#' gsecond(lubridate::now(), "day")
#' @export gsecond
gsecond <- function(x, granularity = "hour", ...) {

  # if (!tsibble::is_tsibble(data)) {
  #   stop("must use tsibble")
  # }
  #
  # # Pick up the time index varible form the tsibble
  # x <- data[[rlang::as_string(tsibble::index(data))]]

  gran_lower <- tolower(granularity)

  # Pick up the possible granularities from lookup table
  gran_opt <- lookup_tbl("second")[[2]]

  # match the input granuarity with the possible ones
  gran_type <- match.arg(gran_lower, choices = gran_opt, several.ok = TRUE)

  # get the index which will be mapped to lubridate function
  gran_type_indx <- match(gran_type, gran_opt)

  if (gran_type == "minute") {
    gsec_value <- lubridate::second(x)
  }
  else if (gran_type %in% c("qhour", "hhour", "hour")) {
    gsec_value <- lubridate::second(x) + 60 * gminute(x, gran_type)
  }
  else if (gran_type == "day") {
    gsec_value <- sec_d(x)
  }
  else if (gran_type == "semester") {
    gsec_value <- sec_d(x) + 24 * 60 * (d_sem(x) - 1)
  }
  else {
    match_value <- eval(parse(text = paste0(lookup_tbl(gran_type)[[1]], "(x)")))
    gsec_value <- sec_d(x) + 24 * 60 * (match_value - 1)
  }

  return(gsec_value)
}


sec_d <- function(x) {
  lubridate::second(x) + (lubridate::hour(x) - 1) * 60 * 60
}
