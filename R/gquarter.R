#' Get combination of quarter component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @param granularity the granularity to be paired up with quarter
#' @return combination of the quarter component of x as a number
#
#' @author Sayani Gupta
#' @examples
#' \dontrun{
#' tsibbledata::aus_elec %>% mutate(quarter_year = gquarter(Time, "year")) %>% tail()
#' gmonth(lubridate::now(),"semester")
#' }
#' @export gquarter
gquarter <- function(x, granularity = "year",...)
{
  # match the gran_type
  gran_lower <- tolower(granularity)

  #check if the user input is correct
  if(!gran_lower  %in% c( "semester", "year"))
  {
    stop(paste("granularity", gran_lower, "is not one of semester or year", sep = " "), call.=F)
  }

  # match the gran_type
  gran_type <- match.arg(gran_lower, choices = c("semester", "year"))

  if(gran_type=="semester")
  {
    gquarter_value <-  lubridate::quarter(x)%%2

  }
  else
  {

    gquarter_value <- lubridate::quarter(x,...)
  }

  return(gquarter_value)

}
