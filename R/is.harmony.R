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
  harmony_object <- gran_tbl(.data, gran1, gran2, response)
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

gran_tbl <- function(.data, gran1, gran2, response = NULL, ...) {
  if (!tsibble::is_tsibble(.data)) {
    stop("must use tsibble")
  }
  match_gran1 <- match(gran1, names(.data))
  match_gran2 <- match(gran2, names(.data))

  if(!is.null(match_gran1))
  {
    var1 <- gran1
  }
  if(!is.null(match_gran2))
  {
    var2 <- gran2
  }


  ind <- .data[[rlang::as_string(tsibble::index(.data))]]

  # gran1_split <- stringr::str_split(gran1, "_", 2) %>% unlist()
  # gran2_split <- stringr::str_split(gran2, "_", 2) %>% unlist()
  # var1 <- gran1_split[1]
  # var2 <- gran1_split[2]
  # var3 <- gran2_split[1]
  # var4 <- gran2_split[2]
  # parse(paste(var1, var2, sep  = "_"))
  # L1 = parse(text = paste(var1, var2, sep  = "_"))

  # data_mutate <- .data %>% dplyr::mutate(L1 = build_gran(ind, var1, var2), L2 = build_gran(ind, var3, var4))

  data_mutate <- .data %>% create_gran(gran1) %>% create_gran(gran2)

  # All possible combinations that are possible
  Allcomb <- data_mutate %>% tidyr::expand(.data[[gran1]], .data[[gran2]])


  combexist <- data_mutate %>% tibble::as_tibble(name_repair = "minimal") %>% dplyr::group_by(.data[[gran1]], .data[[gran2]]) %>% dplyr::summarise(
    count = dplyr::n()
  )

  output <- Allcomb %>%
    dplyr::left_join(combexist, by = c(gran1, gran2)) %>%
    dplyr::select(gran1, gran2,
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
