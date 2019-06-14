#' Get compatibility tables for two granularities
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param .data a tsibble object
#' @param gran1 the first granularity function to use
#' @param gran2 the first granularity function to use
#' @param ... added arguments to be passed
#' @param response variable for which summary is desired per combination
#' @return compatibility table providing if the two granularities are harmonies or clashes. FALSE indicates a clash. If harmony, then a tibble with desired granularities returned.
#' @examples
#' library(dplyr)
#' library(tsibbledata)
#' aus_elec %>% is.harmony("hour_day", "day_week")
#' @export is.harmony
is.harmony <- function(.data, gran1, gran2, response = NULL, ...) {
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
  # parse(paste(var1, var2, sep  = "_"))
  # L1 = parse(text = paste(var1, var2, sep  = "_"))

  # Have to rename
  data_mutate <- .data %>% dplyr::mutate(L1 = build_gran(var1, var2, ind), L2 = build_gran(var3, var4, ind))

  # All possible combinations that are possible
  Allcomb <- data_mutate %>% tidyr::expand(L1, L2)


  combexist <- data_mutate %>% tibble::as_tibble() %>% dplyr::group_by(L1, L2) %>% dplyr::summarise(
    count = n()
  )

  output <- Allcomb %>%
    dplyr::left_join(combexist, by = c("L1", "L2")) %>%
    dplyr::select(
      !!rlang::quo_name(gran1) := L1,
      !!rlang::quo_name(gran2) := L2,
      nobs := count
    ) %>%
    dplyr::mutate(nobs = tidyr::replace_na(nobs, 0))

  # All possible combination that are missing
  cmbmiss <- Allcomb %>% dplyr::anti_join(combexist, by = c("L1", "L2"))
   #dplyr::if_else(nrow(cmbmiss) != 0, "FALSE", output)
   return_output <- ifelse(nrow(cmbmiss) != 0,  "FALSE", "TRUE")

   # if(nrow(cmbmiss) != 0)
   # {
   #  return_output <- FALSE
   # }
   # else{return_output = output
   #   }
   return(return_output)
}

