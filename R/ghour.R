#' Get combination of hour component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @param granularity the granularity to be paired up with hour
#' @return combination of the hour component of x as a number
#
#' @author Sayani Gupta
#' @examples
#' \dontrun{
#' tsibbledata::aus_elec %>% mutate(hour_day = ghour(Time, "day")) %>% tail()
#' ghour(lubridate::now(),"week")
#' }
#' @export ghour
ghour <- function(x, granularity = "day")
{
  # match the gran_type
  gran_lower <- tolower(granularity)

  #check if the user input is correct
  if(!gran_lower  %in% c("day","week", "month", "quarter", "semester", "year"))
  {
    stop(paste("granularity", gran_lower, "is not one of day, week, month, quarter, semester or year", sep = " "), call.=F)
  }

  # match the gran_type
gran_type <- match.arg(gran_lower, choices = c("day","week", "month", "quarter", "semester", "year"), several.ok = TRUE)

gran_type_indx <- match(gran_type, c("day","week", "month", "quarter", "semester", "year"))

lubridate_match <- c("hour","wday","day", "quarter","semester","yday")

match_value <- eval(parse(text = paste0("lubridate::",lubridate_match[gran_type_indx],"(x)")))

if(gran_type_indx==1)
{
   ghour_value <-  lubridate::hour(x)
}
else if (gran_type_indx==5)
{
  ghour_value <-  lubridate::hour(x) + 24*(d_sem(x) - 1)
}
else
{
  ghour_value <- lubridate::hour(x) + 24*(match_value-1)
}

return(ghour_value)
}

#
# dplyr::if_else(gran_type_indx==1, as.numeric(lubridate::hour(x)),
#                dplyr::if_else(gran_type_indx==5, as.numeric(lubridate::hour(x) + 24*(d_sem(x) - 1)), as.numeric(lubridate::hour(x) + 24*(match_value-1))))
# }


d_sem <- function(x) {

  # finds day of the semester
  which_sem <- lubridate::semester(x)
  day_x <- lubridate::yday(x)
  year_leap <- lubridate::leap_year(x)
  div_indx <- dplyr::if_else(year_leap == "FALSE",182, 183)
  dplyr::if_else(which_sem==1,day_x, day_x - div_indx + 1)
}
