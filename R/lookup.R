lookup_tbl <- function(gran)
{
  # edit this list if you add granularities
  granularity = c("second", "minute", "qhour","hhour", "hour", "day", "week", "fortnight", "month",    "quarter", "semester", "year")

  # edit this list if you add granularities
  lubridate_value = c(NA, NA, NA, NA, NA, NA, "wday",NA,  "day", "qday", "na", "yday")

  m = match(gran, granularity)

  return(list(lub_match = lubridate_value[m],  gran_possible  = granularity[-(1:m)]))
  }
