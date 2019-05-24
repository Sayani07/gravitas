fortnight <- function(x)
  (yday(x) - 1) %/% 14 + 1

second_minute <- function(x)
{
  lubridate::second(x)
}

minute_qhour <- function(x)
{
  lubridate::minute(x)%%15
}

qhour_hhour <- function(x)
{
  dplyr::if_else(lubridate::minute(x)%%30<=15, 1, 2)
}

week_fortnight <- function(x)
{
  dplyr::if_else(fortnight(x)<=7, 1, 2)
}


g_order<- function(gran1, gran2)
{
  granularity <- c("second", "minute", "qhour", "hhour", "hour", "day", "week", "fortnight", "month", "quarter", "semester", "year")

  index_gran1 = granularity %>% match(x = gran1)
  index_gran2 = granularity %>% match(x = gran2)

  return(abs(index_gran1 - index_gran2))
}

nesting <- function(gran1, gran2) {

  if (gran1 == "second" & gran2 == "minute") {
    parse_exp("lubridate::second")
  } else if (gran1 == "minute" & gran2 == "qhour") {
    parse_exp("minute_qhour")
  } else if (gran1 == "qhour" & gran2 == "hhour") {
    parse_exp("qhour_hhour")
  } else if (gran1 == "hhour" & gran2 == "hour") {
    parse_exp("hhour_hour")
  } else if (gran1 == "hour" & gran2 == "day") {
    parse_exp("lubridate::hour")
  } else if (gran1 == "day" & gran2 == "week") {
    parse_exp("lubridate::wday")
  } else if (gran1 == "week" & gran2 == "fortnight") {
    parse_exp("week_fortnight")
  }
}

f(x, y) <-  f(x, y-1) + c(x, y-1)*[f(x, y-1) - 1]
