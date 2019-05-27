# the lookup table - this needs to be changed if other granularities are included
lookup_table <- tibble(granularity = c("second", "minute", "qhour", "hhour", "hour", "day", "week", "fortnight", "month", "quarter", "semester", "year"), constant = c(60, 15, 2, 2, 24, 7, 2, 2, 3, 2, 2, 1))


# provides the order difference between two granularities, also provide the upper granularity given the order
g_order <- function(gran1, gran2 = NULL, order = NULL) {
  granularity <- lookup_table %>% .$granularity
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
  granularity <- lookup_table %>% .$granularity
  conv_fac <- lookup_table %>% .$constant
  index_gran1 <- granularity %>% match(x = a)
  if (g_order(a, b) == 0) {
    return(1)
  }
  else {
    return(conv_fac[index_gran1] * gran_convert(g_order(a, order = 1), b))
  }
}
#
# nest <- function(gran1, gran2, x) {
#   if (g_order(gran1, gran2) == 1) {
#     return(one_order(gran1, gran2))
#   } else {
#     value <- nest(g_order(gran1, order = 1), gran2, x) + gran_convert(gran1, g_order(gran2, -1)) * (nest(g_order(gran1, order = 1), gran2, x) - 1)
#     return(value)
#   }
# }


nest<- function(gran1, gran2, x) {
  if (g_order(gran1, gran2) == 1) {
    return(eval(one_order(gran1, gran2)))
  } else {
    value <- nest(gran1, g_order(gran1, order = 1), x) +
      gran_convert(gran1, g_order(gran1, order = 1)) *
        (nest(g_order(gran1, order = 1), gran2, x) - 1)
    return(value)
  }
}

one_order <- function(gran1, gran2) {
  if (gran1 == "second" & gran2 == "minute") {
    value <- parse_exp("lubridate::second")
  } else if (gran1 == "minute" & gran2 == "qhour") {
    value <- parse_exp("minute_qhour")
  } else if (gran1 == "qhour" & gran2 == "hhour") {
    value <- parse_exp("qhour_hhour")
  } else if (gran1 == "hhour" & gran2 == "hour") {
    value <- parse_exp("hhour_hour")
  } else if (gran1 == "hour" & gran2 == "day") {
    value <- parse_exp("lubridate::hour")
  } else if (gran1 == "day" & gran2 == "week") {
    value <- parse_exp("lubridate::wday")
  } else if (gran1 == "week" & gran2 == "fortnight") {
    value <- parse_exp("week_fortnight")
  }
  return(value)
}


# all one order up functions

fortnight <- function(x)
  (yday(x) - 1) %/% 14 + 1

second_minute <- function(x) {
  lubridate::second(x)
}

minute_qhour <- function(x) {
  lubridate::minute(x) %% 15
}

qhour_hhour <- function(x) {
  dplyr::if_else(lubridate::minute(x) %% 30 <= 15, 1, 2)
}

hhour_hour <- function(x) {
  dplyr::if_else(lubridate::minute(x) <= 30, 1, 2)
}

week_fortnight <- function(x) {
  dplyr::if_else(fortnight(x) <= 7, 1, 2)
}
