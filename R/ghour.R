#' Get combination of hour component of a date time
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
#' tsibbledata::aus_elec %>% mutate(hour_week = ghour(Time, "day"))
#' hour_week(today())
#' }
#' @export ghour
ghour <- function(.data, granularity = c("day","week", "month", "quarter", "semester", "year"))
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

match_value <- eval(parse(text = paste0("lubridate::",lubridate_match[gran_type_indx],"(.data)")))

if(gran_type_indx==1)
{
   ghour_value <-  lubridate::hour(.data)
}
else if (gran_type_indx==5)
{
  ghour_value <-  lubridate::hour(.data) + 24*(d_sem(.data) - 1)
}
else
{
  ghour_value <- lubridate::hour(.data) + 24*(match_value-1)
}

return(ghour_value)
}

#
# dplyr::if_else(gran_type_indx==1, as.numeric(lubridate::hour(.data)),
#                dplyr::if_else(gran_type_indx==5, as.numeric(lubridate::hour(.data) + 24*(d_sem(.data) - 1)), as.numeric(lubridate::hour(.data) + 24*(match_value-1))))
# }


d_sem <- function(.data) {

  # finds day of the semester
  which_sem <- lubridate::semester(.data)
  day_x <- lubridate::yday(.data)
  year_leap <- lubridate::leap_year(.data)
  div_indx <- if_else(year_leap == "FALSE",182, 183)
  if_else(which_sem==1,day_x, day_x - div_indx + 1)
}
