#' Get combination of semester component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @param granularity the granularity to be paired up with semester
#' @param ... other arguments to be passed for appropriate labels
#' @return combination of the semester component of x as a number
#
#' @author Sayani Gupta
#' @examples
#' \dontrun{
#' tsibbledata::aus_elec %>% mutate(semester_year = gsemester(Time, "year")) %>% tail()
#' }
#' @export gsemester
gsemester <- function(x, granularity = "year",...)
{
  # match the gran_type
  gran_lower <- tolower(granularity)

  #check if the user input is correct
  if(!gran_lower  %in% c("year"))
  {
    stop(paste("granularity", gran_lower, "is not year", sep = " "), call.=F)
  }

  # match the gran_type
  gran_type <- match.arg(gran_lower, choices = c("year"))

  if(gran_type=="year")
  {
    gsemester_value <-  lubridate::semester(x,...)
  }
  return(gsemester_value)

}
