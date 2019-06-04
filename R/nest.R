#' Get combination of hour component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @param granularity1 granularity level 1
#' @param granularity2 any higher order granularity to be paired with granularity 1
#' @param ... other arguments to be passed for appropriate labels
#' @return combination of the hour component of x as a number
#
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' aus_elec <- tsibbledata::aus_elec %>%
#'   mutate(
#'     hour_day = nest(Time, "quarter", "year")
#'   )
#' @export nest
nest <- function(x, granularity1, granularity2, ...) {

  # add that if NULL for granularity 1 and granularity 2, which one you will choose

  # use lookup table for granularity1
  lookup_l1 <- lookup_all(granularity1)

  # find the set of granularities that are higher in order than granularity 1
  gran <- lookup_l1$order_up

  # check if the user input for granularity2 is correct/ else show an error to the user
  granularity2 <- match.arg(granularity2, gran, several.ok = TRUE)


  # use lookup table for granularity1
  lookup_l2 <- lookup_all(granularity2)

  #
  if (granularity2 %in% lookup_all("day")$order_up & granularity1 %in% lookup_all("day")$order_down) {
    g_value <- eval(parse_exp(lookup_l1$match_day)) + lookup_l1$match_rel * (eval(parse_exp(lookup_l2$match_day)) - 1)
  }
  else if (granularity1 %in% lookup_all("day")$order_up) {
    if (granularity2 == "year") {
      g_value <- eval(parse_exp(lookup_l1$lub_match))
    }
    else {
      g_value <- eval(parse_exp(lookup_l1$lub_match)) %% relate_tbl(granularity1, granularity2)
    }
  }
  return(g_value)
}

