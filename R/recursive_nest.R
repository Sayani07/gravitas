#' Get combination of granularities of a date time
#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @param gran1 the lower level granularity to be paired
#' @param gran2 the upper level granularity to be paired
#' @param ... other arguments to be passed for appropriate labels
#' @return combination of granularities of x as a number
#
#' @examples
#' library(dplyr)
#' tsibbledata::nyc_bikes %>% tail() %>% mutate(hhour_week = build_gran("hhour", "week", start_time))
#' @export build_gran



build_gran <- function(gran1, gran2, x, ...) {

  # for aperiodic granularities - gran1 less than month and gran2 more than or equal to month
  if (g_order(gran1, "month") > 0 & g_order("month", gran2) >= 0) {
    index_gran2 <- lookup_table$granularity %>% match(x = gran2)
    day_gran2 <- eval(parse_exp(lookup_table$convertday[index_gran2]))
    if (g_order(gran1, "day") > 0) {
      c_gran1_day <- gran_convert(gran1, "day")
      value <- build_gran(gran1, "day", x) + c_gran1_day * (day_gran2 - 1)
    }
    else if (g_order(gran1, "day") == 0) {
      value <- day_gran2
    }
    else {
      c_day_gran1 <- gran_convert("day", gran1)
      value <- ceiling(day_gran2 / c_day_gran1)
    }
  }
  else {
    gran1_ordr1 <- g_order(gran1, order = 1)
    if (g_order(gran1, gran2) == 1) {
      one_order <- lookup_table$convertfun[lookup_table$granularity %>% match(x = gran1)]
      return(eval(parse_exp(one_order)))
    } else {
      value <- build_gran(gran1, gran1_ordr1, x) +
        gran_convert(gran1, gran1_ordr1) *
          (build_gran(gran1_ordr1, gran2, x) - 1)
      return(value)
    }
  }
}



# the lookup table - this needs to be changed if other granularities are included
lookup_table <- tibble::tibble(
  granularity = c("second", "minute", "qhour", "hhour", "hour", "day", "week", "fortnight", "month", "quarter", "semester", "year"),
  constant = c(60, 15, 2, 2, 24, 7, 2, 2, 3, 2, 2, 1),
  convertfun = c("lubridate::second", "minute_qhour", "qhour_hhour", "hhour_hour", "lubridate::hour", "lubridate::wday", "week_fortnight", "fortnight_month", "month_quarter", "quarter_semester", "semester_year", 1),
  convertday = c("second_day", "minute_day", "qhour_day", "hhour_day", "lubridate::hour", 1, "lubridate::wday", "day_fortnight", "lubridate::mday", "lubridate::qday", "day_semester", "lubridate::yday"),
)





# provides the order difference between two granularities, also provide the upper granularity given the order
g_order <- function(gran1, gran2 = NULL, order = NULL) {
  granularity <- lookup_table$granularity
  index_gran1 <- granularity %>% match(x = gran1)
  if (!is.null(gran2)) {
    index_gran2 <- granularity %>% match(x = gran2)
    return(index_gran2 - index_gran1)
  }
  if (!is.null(order)) {
    return(granularity[index_gran1 + order])
  }
}

# provides the conversion factor between two granularities

gran_convert <- function(a, b) {
  a = tolower(a)
  b = tolower(b)


  granularity <- lookup_table$granularity
  conv_fac <- lookup_table$constant
  index_gran1 <- granularity %>% match(x = a)
  if(g_order(a, b) < 0)
  {
    stop("Second temporal resolution should be higher in order than the first one. Try reversing their position")
  }
  if (g_order(a, b) == 0) {
    return(1)
  }
  else {
    return(conv_fac[index_gran1] * gran_convert(g_order(a, order = 1), b))
  }
}



# all one order up functions


second_minute <- function(x, ...) {
  lubridate::second(x, ...)
}

minute_qhour <- function(x, ...) {
  lubridate::minute(x, ...) %% 15 + 1
}

qhour_hhour <- function(x, ...) {
  dplyr::if_else((lubridate::minute(x, ...) %% 30 + 1) <= 15, 1, 2)
}

hhour_hour <- function(x, ...) {
  dplyr::if_else(lubridate::minute(x, ...) <= 30, 1, 2)
}

week_fortnight <- function(x, ...) {
  dplyr::if_else((lubridate::yday(x, ...) %/% 14 + 1) <= 14, 1, 2)
}

month_quarter <- function(x, ...) {
  value <- lubridate::month(x, ...) %% 3
  dplyr::if_else(value == 0, 3, value)
  # otherwise remainder will change the label of the largest value to zero
}

quarter_semester <- function(x, ...) {
  value <- lubridate::quarter(x, ...) %% 2
  dplyr::if_else(value == 0, 2, value)
  # otherwise remainder will change the label of the largest value to zero
}

semester_year <- function(x, ...) {
  lubridate::semester(x, ...)
}


# convert day functions

qhour_day <- function(x, ...) {

  # finds which quarter of the day
  ceiling(lubridate::minute(x, ...) / 15) + 4 * (lubridate::hour(x, ...))
}

hhour_day <- function(x, ...) {
  (lubridate::hour(x, ...) * 60 + lubridate::minute(x, ...)) / 30
}

minute_day <- function(x, ...) {
  lubridate::minute(x, ...) + (lubridate::hour(x, ...) - 1) * 60
}
second_day <- function(x, ...) {
  lubridate::second(x, ...) + (lubridate::hour(x, ...) - 1) * 60 * 60
}


day_semester <- function(x) {

  # finds day of the semester
  which_sem <- lubridate::semester(x)
  day_x <- lubridate::yday(x)
  year_leap <- lubridate::leap_year(x)
  div_indx <- dplyr::if_else(year_leap == "FALSE", 182, 183)
  dplyr::if_else(which_sem == 1, day_x, day_x - div_indx + 1)
}

day_fortnight <- function(x) {
  value <- lubridate::yday(x) %/% 14
  dplyr::if_else(value == 0, 14, value)
}


parse_exp <- function(y) {
  if (y == "1") {
    value <- 1
  }
  else {
    value <- parse(text = paste0(y, "(x,...)"))
  }
  return(value)
}
