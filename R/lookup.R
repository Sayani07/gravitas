lookup_tbl <- function(gran)
{
  # edit this list if you add granularities
  granularity = c("second", "minute", "qhour","hhour", "hour", "day", "week", "fortnight", "month",    "quarter", "semester", "year")

  # edit this list if you add granularities
  # find the corresponding  function match from lubridate -  na implies either no function exists in lubridate OR the function from lubridate cannot be used in the same form as required by the loop

  lubridate_value = c(NA, NA, NA, NA, NA, NA, "wday",NA,  "day", "qday", "na", "yday")

  m = match(gran, granularity)

  return(list(lub_match = lubridate_value[m],  gran_possible  = granularity[-(1:m)]))
  }
