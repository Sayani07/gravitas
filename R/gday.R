#' Get combination of day component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @return the hour of the week component of x as a number
#
#' @author Sayani Gupta
#' @examples
#' \dontrun{
#' tsibbledata::aus_elec %>% mutate(day_week = gday(Time, "week")) %>% tail()
#' gday(lubridate::now(),"month")
#' }
#' @export ghour
gday <- function(x, granularity = "week")
{
  # match the gran_type
  gran_lower <- tolower(granularity)

  #check if the user input is correct
  if(!gran_lower  %in% c("week", "month", "quarter", "semester", "year"))
  {
    stop(paste("granularity", gran_lower, "is not one of week, month, quarter, semester or year", sep = " "), call.=F)
  }

  # match the gran_type

  gran_pos <-  c("week", "month", "quarter", "semester", "year")

  gran_type <- match.arg(gran_lower, choices = gran_pos, several.ok = TRUE)

  lubridate_match <- c("wday","day", "qday","semester","yday")

  gran_type_indx <- match(gran_type, gran_pos)

  if(!gran_type_indx==4)
  {
    match_value <- eval(parse(text = paste0("lubridate::",lubridate_match[gran_type_indx],"(x)")))
  }
  else
  {
    match_value <- d_sem(x)
  }

  return(match_value)
}



d_sem <- function(x) {

  # finds day of the semester
  which_sem <- lubridate::semester(x)
  day_x <- lubridate::yday(x)
  year_leap <- lubridate::leap_year(x)
  div_indx <- if_else(year_leap == "FALSE",182, 183)
  if_else(which_sem==1,day_x, day_x - div_indx + 1)
}
