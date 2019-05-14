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
#' aus_elec <- tsibbledata::aus_elec %>%
#' mutate(hour_day = ghour(Time, "day"),
#' month_year = gmonth(Time, "year"))
#'aus_elec %>%
#'filter(hour_day %in% c(1, 10, 15),
#'month_year %in% c(4,5,6) ) %>%
#'ggplot(aes(x = as.factor(hour_day), y = Demand)) + facet_wrap(~month_year) +
#'geom_boxplot()
#'aus_quant <- aus_elec %>% filter(hour_day %in% c(1, 10, 15), month_year %in% c(4,5,6) ) %>% group_by(hour_day, month_year) %>%
#'do({x <- .$Demand
#'purrr::map_dfr(.x = c(0.1, 0.5, 0.9),
#'               .f = ~ tibble(Quantile = .x,
#'                             Value = quantile(x, probs = .x,na.rm=TRUE)))})
#'aus_quant %>%ggplot(aes(x=hour_day,y=Value,col=as.factor(Quantile))) + geom_line() + facet_wrap(~month_year)

#' @export ghour
ghour <- function(x, granularity = "day",...) {

  # lookup_tbl to be used for gran hour
  lookup_l1 <- lookup_tbl("hour")

  # Pick up the possible granularities from lookup table
  gran_opt <- lookup_l1$gran_possible

  # check if the user input is correct
  gran_type <- match.arg(granularity, choices = gran_opt, several.ok = TRUE)

  # Match the input granularity from the lookup_tbl
  lookup_l2 <-  lookup_tbl(granularity)$match_day


  ghour_value <- eval(parse_exp(lookup_l1$match_day)) + 24 * (eval(parse_exp(lookup_l2)) - 1)

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
