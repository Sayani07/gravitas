#' Get combination of half-hour component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @param granularity the granularity to be paired up with half-hour
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

  # there is no function in lubrdiate which computes half hour of the hour or half hour of the day
  lubridate_match <- c("wday", "day", "qday","semester","yday")


  if(gran_type_indx==1)
  {
    ghalfhour_value <- dplyr::if_else(lubridate::minute(x) <30, 1, 2)
  }
  else if(gran_type_indx==2)
  {
    ghalfhour_value <- hh_d(x)
  }
  else
  {

    match_value <- eval(parse(text = paste0("lubridate::",lubridate_match[gran_type_indx - 2],"(x)")))
    ghalfhour_value <- hh_d(x) + 48*(match_value - 1)
  }

  return(ghalfhour_value)

}


hh_d = function(x)
{
  (lubridate::hour(x)*60 + lubridate::minute(x))/30
}

