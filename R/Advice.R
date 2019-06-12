#' Get compatibility tables for two granularities
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param data a tsibble object
#' @param gran1 the first granularity function to use
#' @param gran2 the first granularity function to use
#' @param response variable for which summary is desired per combination
#' @return compatibility table providing if the two granularities are harmonies or clashes. It also provides information on the range of the number of observations per combination and variation across number of combinations and other summery statistics.
#'
#' @examples
#' library(dplyr)
#' library(tsibbledata)
#' aus_elec %>%  compatibility("hour_day", "day_week")
#' @export compatibility
compatibility<- function(.data, gran1, gran2, response = NULL, ...) {

  if (!tsibble::is_tsibble(.data)) {
    stop("must use tsibble")
  }

  ind <- .data[[rlang::as_string(tsibble::index(.data))]]

  gran1_split <- stringr::str_split(gran1, "_", 2) %>% unlist()
  gran2_split <- stringr::str_split(gran2, "_", 2) %>% unlist()
  var1 <- gran1_split[1]
  var2 <- gran1_split[2]
  var3 <- gran2_split[1]
  var4 <- gran2_split[2]
  #parse(paste(var1, var2, sep  = "_"))
  #L1 = parse(text = paste(var1, var2, sep  = "_"))

  #Have to rename
  data_mutate <- .data %>% dplyr::mutate(L1 = nest(var1, var2, ind), L2 = nest(var3, var4, ind))

  # All possible combinations that are possible
  Allcomb <- data_mutate %>% tidyr::expand(L1, L2)


  combexist <- data_mutate  %>% tibble::as_tibble() %>% dplyr::group_by(L1, L2) %>% dplyr::summarise(
    count = n())

  output <-Allcomb %>% dplyr::left_join(combexist) %>%
    select(!!quo_name(gran1) := L1,
           !!quo_name(gran2) := L2,
           Nobs := count) %>%  mutate(Nobs = tidyr::replace_na(Nobs, 0))

  output
  }

