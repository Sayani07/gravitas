#' Get combination of month component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @param granularity the granularity to be paired up with week
#' @return combination of the week component of x as a number
#
#' @author Sayani Gupta
#' @examples
#' \dontrun{
#' tsibbledata::aus_elec %>% mutate(month_year = gmonth(Time, "year")) %>% tail()
#' gmonth(lubridate::now(),"quarter")
#' }
#' @export gmonth
gmonth <- function(x, granularity = "month",...)
{
  # match the gran_type
  gran_lower <- tolower(granularity)

  #check if the user input is correct
  if(!gran_lower  %in% c( "quarter", "semester", "year"))
  {
    stop(paste("granularity", gran_lower, "is not one of quarter, semester or year", sep = " "), call.=F)
  }

  # match the gran_type
  gran_type <- match.arg(gran_lower, choices = c("quarter", "semester", "year"))

  if(gran_type=="quarter")
  {
    gmonth_value <-  month(x)%%3

  }
  else if(gran_type=="semester")
  {

    gmonth_value <- month(x)%%2

  }
  else
  {
    gmonth_value <-  month(x,...)
  }
  return(gmonth_value)

}

