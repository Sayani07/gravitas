#' Get combination of half-hour component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @return combination of the hour component of x as a number
#
#' @author Sayani Gupta
#' @examples
#' \dontrun{
#' tsibbledata::aus_elec %>% mutate(hh_day = ghalfhour(Time, "day")) %>% tail()
#' ghalfhour(lubridate::now(),"week")
#' }
#' @export ghalfhour
ghalfhour <- function(x, granularity = "hour")
{
  # match the gran_type
  gran_lower <- tolower(granularity)

  #check if the user input is correct
  if(!gran_lower  %in% c("hour","day","week", "month", "quarter", "semester", "year"))
  {
    stop(paste("granularity", gran_lower, "is not one of hour, day, week, month, quarter, semester or year", sep = " "), call.=F)
  }

  # match the gran_type
  gran_pos <-  c("hour","day","week", "month", "quarter", "semester", "year")

  gran_type <- match.arg(gran_lower, choices = gran_pos, several.ok = TRUE)

  gran_type_indx <- match(gran_type, gran_pos)

  lubridate_match <- c("hour","NA","wday", "qday","semester","yday")

  match_value <- eval(parse(text = paste0("lubridate::",lubridate_match[gran_type_indx],"(x)")))

  if(gran_type_indx==1)
  {
    ghalfhour_value <- lubridate::hour(x)
  }
  else if(gran_type_indx==2)
  {
    ghalfhour_value <- lubridate::hh_d(x)
  }
  else
  {
    ghalfhour_value <- hh_d(x) + 48*(match_value - 1)
  }

  return(ghalfhour_value)

}


hh_d = function(x)
{
  (hour(x)*60 + minute(x))/30
}
