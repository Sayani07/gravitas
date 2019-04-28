#' Get combination of semester component of a date time
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param x a date-time object
#' @param granularity the granularity to be paired up with semester
#' @param ... other arguments to be passed for appropriate labels
#' @return combination of the semester component of x as a number

#' @examples
#' \dontrun{
#' tsibbledata::aus_elec %>% mutate(semester_year = gsemester(Time, "year")) %>% tail()
#' }
#' @export gsemester
gsemester <- function(x, granularity = "year", ...) {
  # match the gran_type
  gran_lower <- tolower(granularity)
  gran_opt <- c("year")

  # check if the user input is correct

  if (!gran_lower %in% gran_opt) {
    stop(paste0("granularity ", gran_lower, " is not one of ", paste0(gran_opt, collapse = ", ")), call. = F)
  }

  # match the gran_type
  gran_type <- match.arg(gran_lower, gran_opt)

  if (gran_type == "year") {
    gsemester_value <- lubridate::semester(x, ...)
  }
  return(gsemester_value)
}
