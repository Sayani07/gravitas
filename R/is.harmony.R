#' Check if two temporal granularities are harmonies
#'
#' @param .data A tsibble object.
#' @param gran1 One of the temporal granularities to check for harmonies.
#' @param gran2 The second temporal granularity in the pair.
#' @param facet_h levels of facet variable for which facetting is allowed while plotting bivariate temporal granularities.
#' @param ... Added arguments to be passed
#' @param response Variable for which summary is desired per combination
#' @return TRUE if two granularties are harmonies.
#' @examples
#' library(dplyr)
#' library(tsibbledata)
#' library(ggplot2)
#' vic_elec %>% is.harmony("hour_day", "day_week")
#' @export is.harmony

is.harmony <- function(.data, gran1, gran2, response = NULL, facet_h = 31, ...) {

  if(gran1==gran2)
  {
    warning("the two granularities should be distinct")
  }

  harmony_object <- gran_tbl(.data, gran1, gran2, response)
  names <- names(harmony_object)
  # All possible combination that are missing
  # cmbmiss <-  harmony_object %>% filter(nobs==0)
  cmbmiss <- any(harmony_object$nobs == 0)
  facet_nlevel <- harmony_object[, 1] %>% dplyr::distinct()

  if (cmbmiss == "TRUE" | nrow(facet_nlevel) > facet_h) {
    return_output <- "FALSE"
  } else {
    return_output <- "TRUE"
  }
  if(gran1==gran2)
  {
   return_output <- "FALSE"
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

clash_reason <- function(.data, gran1, gran2, response = NULL, ...) {

 gran_full <-  gran_tbl(.data, gran1, gran2, response = NULL, ...)
 if(any(gran_full$nobs==0))
 {
  clash_combination <- gran_full %>% dplyr::filter(nobs==0) %>% dplyr::select(gran1, gran2)

 distinct_gran1 <- gran_full %>% dplyr::distinct(gran_full[[gran1]]) %>% nrow()
 distinct_gran2 <- gran_full %>% dplyr::distinct(gran_full[[gran2]]) %>% nrow()

 # inter facet homogeneity

 data_count <- gran_tbl(.data, gran1, gran2, response, ...)

 # inter_facet_homogeneity <- gran_full %>% dplyr::group_by(gran1) %>% dplyr::summarise(min_c = min(nobs), max_c = max(nobs), variation = sd(nobs)) %>% sum = sum(dplyr::if_else(min_c == max_c, 0, 1)) %>% dplyr::mutate(value = dplyr::if_else(sum == 0, "TRUE", "FALSE"))
 #
 # # intra facet homogeneity
 # intra_facet_homogeneity <- data_count %>% dplyr::group_by(!!rlang::quo_name(gran2)) %>% dplyr::summarise(min_c = min(nobs), max_c = max(nobs)) %>% dplyr::summarise(sum = sum(dplyr::if_else(min_c == max_c, 0, 1))) %>% dplyr::mutate(value = dplyr::if_else(sum == 0, "TRUE", "FALSE"))


 return(list(paste(gran1, "has", distinct_gran1, "distinct levels and", gran2, "has", distinct_gran2, "distinct levels", "with the following structurally empty combinations. They are structurally empty as the structure of calendar does not allow these combinations to appear together."), clash_combination))
}
 else{
   return(paste("Good Work! You have chosen harmonies. Go ahead and save the plot using your choice of distribution plot"))
 }

}
