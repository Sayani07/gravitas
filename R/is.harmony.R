#' Check if two temporal granularities are harmonies
#'
#' @param .data A tsibble object.
#' @param gran1 One of the temporal granularities to check for harmonies.
#' @param gran2 The second temporal granularity in the pair.
#' @param ... Added arguments to be passed
#' @param response Variable for which summary is desired per combination
#' @return TRUE if two granularties are harmonies.
#' @examples
#' library(dplyr)
#' library(tsibbledata)
#' library(ggplot2)
#' vic_elec %>% is.harmony("hour_day", "day_week")
#' @export is.harmony
is.harmony <- function(.data, gran1, gran2, response = NULL, ...) {
  harmony_object <- harmony_obj(.data, gran1, gran2, response)
  names <- names(harmony_object)
  # All possible combination that are missing
  # cmbmiss <-  harmony_object %>% filter(nobs==0)
  cmbmiss <- any(harmony_object$nobs == 0)
  facet_nlevel <- harmony_object[, 1] %>% dplyr::distinct()

  if (cmbmiss == "TRUE" | nrow(facet_nlevel) > 31) {
    return_output <- "FALSE"
  } else {
    return_output <- "TRUE"
  }

  return(return_output)
}

harmony_obj <- function(.data, gran1, gran2, response = NULL, ...) {
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

  data_mutate <- .data %>% dplyr::mutate(L1 = build_gran(ind, var1, var2), L2 = build_gran(ind, var3, var4))

  # All possible combinations that are possible
  Allcomb <- data_mutate %>% tidyr::expand(L1, L2)


  combexist <- data_mutate %>% tibble::as_tibble(name_repair = "minimal") %>% dplyr::group_by(L1, L2) %>% dplyr::summarise(
    count = dplyr::n()
  )

  output <- Allcomb %>%
    dplyr::left_join(combexist, by = c("L1", "L2")) %>%
    dplyr::select(
      !!rlang::quo_name(gran1) := L1,
      !!rlang::quo_name(gran2) := L2,
      nobs := count
    ) %>%
    dplyr::mutate(nobs = tidyr::replace_na(nobs, 0))



  # if(nrow(cmbmiss) != 0)
  # {
  #  return_output <- FALSE
  # }
  # else{return_output = output
  #   }
  return(output)
}
