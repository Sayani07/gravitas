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
#' library(ggplot2)
#' library(dplyr)
#' tsibbledata::nyc_bikes %>% mutate(m_hour = gminute(start_time, "hour")) %>% tail()
#' gminute(lubridate::now(), "day")
#' @export gminute
gminute <- function(x, granularity = "hour", ...) {


  # lookup_tbl to be used for gran hour
  lookup_l1 <- lookup_tbl("minute")

  # Pick up the possible granularities from lookup table
  gran_opt <- lookup_l1$gran_possible


  # Match the input granularity from the lookup_tbl
  lookup_l2 <- lookup_tbl(granularity)$match_day


  gran_type <- match.arg(granularity, choices = gran_opt, several.ok = TRUE)


  if (gran_type == "qhour") {
    gmin_value <- lubridate::minute(x) %% 15
  }

  else if (gran_type == "hhour") {
    gmin_value <- lubridate::minute(x) %% 30
  }
  else if (gran_type == "hour") {
    gmin_value <- lubridate::minute(x)
  }
  else {
    gmin_value <- eval(parse_exp(lookup_l1$match_day)) + 24 * (eval(parse_exp(lookup_l2)) - 1)
  }

  return(gmin_value)
}


ghour <- function(x, granularity = "day", ...) {

  # lookup_tbl to be used for gran hour
  lookup_l1 <- lookup_tbl("hour")

  # Pick up the possible granularities from lookup table
  gran_opt <- lookup_l1$gran_possible

  # check if the user input is correct
  if (!granularity %in% gran_opt) {
    stop(paste0("granularity ", granularity, " is not one of ", paste0(gran_opt, collapse = ", ")), call. = F)
  }

  # Match the input granularity from the lookup_tbl
  lookup_l2 <- lookup_tbl(granularity)$match_day


  ghour_value <- eval(parse_exp(lookup_l1$match_day)) + 24 * (eval(parse_exp(lookup_l2)) - 1)

  return(ghour_value)
}



min_d <- function(x) {
  lubridate::minute(x) + (lubridate::hour(x) - 1) * 60
}
