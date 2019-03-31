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
#' tsibbledata::aus_elec %>% mutate(hour_week = h_w(Time))
#' hour_week(today())
#' }
#' @export
h_w = function(x)
{
  day_week <- wday(x)
  hour_week <- hour(x) + 24*(day_week-1)
  hour_week
}

h_m  =  function(x)
{
  day_month <- day(x)
  hour_month <- hour(x) + 24*(day_month-1)
  hour_month
}

h_q = function(x)

{
  day_quarter <- quarter(x)
  hour_quarter <- hour(x) + 24*(day_quarter-1)
  hour_quarter
}

h_sem = function(x)

{
  day_semester <- semester(x)
  hour_semester <- hour(x) + 24*(day_semester-1)
  hour_semester
}

h_y = function(x)

{
  day_year <- yday(x)
  hour_year <- hour(x) + 24*(day_year-1)
  hour_year
}


ghour <- function(x, granularity = c("day","week", "month", "quarter", "semester", "year"))
{

  # match the gran_type
  gran_type <- tolower(granularity)

  # check if the user input is correct
  # if (!gran_type  %in% c("day","week", "month", "quarter", "semester", "year")
  # {
  #   stop("granularity not found")
  # }

gran_type <- match.arg(granularity)

gran_type_indx <- match(gran_type, c("day","week", "month", "quarter", "semester", "year"))

lubridate_match <- c("hour","wday","day", "quarter","semester","yday")

match_value <- eval(parse(text = paste0("lubridate::",lubridate_match[gran_type_indx],"(x)")))

dplyr::if_else(gran_type_indx==1, as.numeric(lubridate::hour(x)),  as.numeric(lubridate::hour(x) + 24*(match_value-1)))
}
