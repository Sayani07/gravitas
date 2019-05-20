lookup_all <- function(gran) {
  # edit this list if you add granularities
  granularity <- c("second", "minute", "qhour", "hhour", "hour", "day", "week", "fortnight", "month", "quarter", "semester", "year")

  # edit this list if you add granularities
  # find the corresponding  function match from lubridate -  na implies either no function exists in lubridate OR the function from lubridate cannot be used in the same form as required by the loop

  match_day <- c("sec_d", "min_d", "qh_d", "hh_d", "lubridate::hour", 1, "lubridate::wday", NA, "lubridate::mday", "lubridate::qday", "d_sem", "lubridate::yday")

  match_arg_day <- c(24 * 60 * 60, 24 * 60, 24 * 4, 24 * 2, 24, 1, 1 / 7, 1 / 14, NA, NA, NA, NA)

  match_hour <- c("sec_hour", "lubridate::minute", "qh_hour", "hh_hour", 1, NA, NA, NA, NA, NA, NA, NA)

  match_arg_month <- c(NA, NA, NA, NA, NA, NA, NA, NA, 1, 4, 6, 12)

  m <- match(gran, granularity)

  return(list(
    match_day = match_day[m],
    match_hour = match_hour[m],
    order_up = granularity[-(1:m)],
    order_down = granularity[1:(m - 1)],
    match_rel = match_arg_day[m],
    match_month = match_arg_month[m]
  ))
}


parse_exp <- function(y) {
  if (y == "1") {
    value <- 1
  }
  else {
    value <- parse(text = paste0(y, "(x)"))
  }
  return(value)
}
