#' Get combination of quarter of an hour component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @param granularity the granularity to be paired up with  quarter of an hour
#' @return combination of the quarter hour component of x as a number
#
#' @author Sayani Gupta
#' @examples
#' \dontrun{
#' tsibbledata::aus_elec %>% mutate(hour_day = gqhour(Time, "hour")) %>% tail()
#' gqtrhour(lubridate::now(),"day")
#' }
#' @export gqhour
gqhour <- function(x, granularity = "day")
{
  # match the gran_type
  gran_lower <- tolower(granularity)
  gran_pos <-  c("hhour", "hour","day","week", "month", "quarter", "semester", "year")

  #check if the user input is correct
  if(!gran_lower  %in% gran_pos)
  {
    stop(paste("granularity", gran_lower, "is not one of hhour, hour, day, week, month, quarter, semester or year", sep = " "), call.=F)
  }

  # match the gran_type
  gran_type <- match.arg(gran_lower, choices = gran_pos, several.ok = TRUE)

  gran_type_indx <- match(gran_type, gran_pos)

  lubridate_match <- c("wday","day", "qday","semester","yday")

  if(gran_type_indx==1)
  {
    gqhour_value <- qh_h(x)%%2 + 1
  }
  else if(gran_type_indx==2)
  {
    gqhour_value <-  qh_h(x)
  }
  else if (gran_type_indx ==3)
  {
    gqhour_value = qh_d(x)
  }
  else
  {

    match_value <- eval(parse(text = paste0("lubridate::",lubridate_match[gran_type_indx - 3],"(x)")))
    gqhour_value <- qh_d(x) + 96*(match_value - 1)
  }

  return(gqhour_value)

}


qh_h <- function(x) {

  # finds which quarter of the hour
  floor(lubridate::minute(x)/15) + 1

}

qh_d <- function(x) {

  # finds which quarter of the day
  lubridate::hour(x)*qh_h(x) + qh_h(x)

}
