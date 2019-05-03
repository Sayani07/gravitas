#' Get combination of hour component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @param granularity the granularity to be paired up with hour
#' @param ... other arguments to be passed for appropriate labels
#' @return combination of the hour component of x as a number
#
#' @examples
#' tsibbledata::aus_elec %>% mutate(hour_day = ghour(Time, "day")) %>% tail()
#' ghour(lubridate::now(), "week")

#' @export ghour
ghour <- function(data, granularity = "day",...) {

  if (!tsibble::is_tsibble(data)) {
    stop("must use tsibble")
  }

  # Pick up the time index varible form the tsibble
  x <- data[[rlang::as_string(tsibble::index(data))]]


  gran_lower <- tolower(granularity)

  # Pick up the possible granularities from lookup table
  gran_opt <- lookup_tbl("hour")[[2]]

  # match the input granuarity with the possible ones
  gran_type <- match.arg(gran_lower, choices = gran_opt, several.ok = TRUE)

  # get the index which will be mapped to lubridate function
  gran_type_indx <- match(gran_type, gran_opt)


  if (gran_type == "day") {
    ghour_value <- lubridate::hour(x)
  }
  else if (gran_type == "semester") {
    ghour_value <- lubridate::hour(x) + 24 * (d_sem(x) - 1)
  }
  else {
    match_value <- eval(parse(text = paste0("lubridate::",lookup_tbl(gran_type)[[1]], "(x)")))
    ghour_value <- lubridate::hour(x) + 24 * (match_value - 1)
  }

  return(ghour_value)
}


d_sem <- function(x) {

  # finds day of the semester
  which_sem <- lubridate::semester(x)
  day_x <- lubridate::yday(x)
  year_leap <- lubridate::leap_year(x)
  div_indx <- dplyr::if_else(year_leap == "FALSE", 182, 183)
  dplyr::if_else(which_sem == 1, day_x, day_x - div_indx + 1)
}
