#' Build temporal granularities

#' @param .data A tsibble object.
#' @param gran1 Temporal granularity required.
#' @param label Logical. TRUE will display the month as an ordered factor of character string such as "January", "February". FALSE will display the month as an ordered factor such as 1 to 12, where 1 stands for January and 12 for December.
#' @param abbr logical. FALSE will display abbreviated labels
#' @param ... Other arguments passed on to individual methods.
#' @return A tsibble with an additional column of granularity
#
#' @examples
#' library(dplyr)
#' library(tsibble)
#' tsibbledata::vic_elec %>% as_tsibble() %>% create_gran("hour_week") %>% tail()
#' @export create_gran


create_gran <- function(.data, gran1 = NULL,  label = TRUE, abbr = TRUE, ...) {


  if (!tsibble::is_tsibble(.data)) {
    stop("must use tsibble")
  }

  if(is.null(gran1)){
    stop("gran1 must be supplied")
  }

  events <- match(gran1, names(.data))
  if(!is.na(events))
  {
   return(.data)
  }

  # if (is.null(gran2)) {
  #   gran2 <- g_order(gran1, order = 1)
  #   col_name <- paste(rlang::quo_name(gran1), gran2, sep = "_")
  # }

  # if (!is.null(gran2)) {
  #   col_name <- paste(rlang::quo_name(gran1), rlang::quo_name(gran2), sep = "_")
  # }

  x <- .data[[rlang::as_string(tsibble::index(.data))]]

  gran1_split <- stringr::str_split(gran1, "_", 2) %>% unlist()
  lgran <- gran1_split[1]
  ugran <- gran1_split[2]


  data_mutate <- .data %>% dplyr::mutate(L1 = build_gran(x, lgran, ugran, ...))


  lev <- unique(data_mutate$L1)

  if (label) {
    if (lgran == "day" & ugran == "week") {
      names <- c(
        "Sunday", "Monday", "Tuesday", "Wednesday",
        "Thursday", "Friday", "Saturday"
      )
    }
    else if (lgran == "month" & ugran == "year") {
      names <- c(
        "January", "February", "March", "April",
        "May", "June", "July", "August", "September", "October", " November", "December"
      )
    }
    else {
      names <- as.character(1:length(unique(lev)))
    }
    names_abbr <- substr(names, 1, 3)

    if (abbr) names_gran <- names_abbr else names_gran <- names
  }
  else {
    names_gran <- as.character(1:length(unique(lev)))
  }

  data_mutate$L1 <- factor(data_mutate$L1, labels = names_gran)

  data_mutate %>%
    dplyr::mutate(
      !!gran1 := L1
    ) %>%
    dplyr::select(-L1)
}


build_gran <- function(x, lgran = NULL, ugran = NULL, ...) {
  # for aperiodic granularities - lgran less than month and ugran more than or equal to month

  #
  #   if (is.null(lgran) | is.null(ugran)) {
  #     stop("function requires both lgran and ugran to be specified")
  #   }


  if (is.null(lgran)) {
    stop("function requires lgran to be specified")
  }


  if (g_order(lgran, ugran) < 0) {
    stop("lgran should have lower temporal order than ugran. Try swapping lgran and ugran")
  }


  if (g_order(lgran, ugran) == 0) {
    stop("lgran and ugran should be distinct")
  }



  # if(index_lgran > index_ugran)
  # {
  #   tmp <- lgran
  #   lgran <- ugran
  #   ugran <- tmp
  # }

  if (g_order(lgran, "month") > 0 & g_order("month", ugran) >= 0) {
    index_ugran <- lookup_table$granularity %>% match(x = ugran)
    day_ugran <- eval(parse_exp(lookup_table$convertday[index_ugran]))
    if (g_order(lgran, "day") > 0) {
      c_lgran_day <- gran_convert(lgran, "day")
      value <- build_gran(x, lgran, "day") + c_lgran_day * (day_ugran - 1)
    }
    else if (g_order(lgran, "day") == 0) {
      value <- day_ugran
    }
    else {
      c_day_lgran <- gran_convert("day", lgran)
      value <- ceiling(day_ugran / c_day_lgran)
    }
  }
  else {
    lgran_ordr1 <- g_order(lgran, order = 1)
    if (g_order(lgran, ugran) == 1) {
      one_order <- lookup_table$convertfun[lookup_table$granularity %>% match(x = lgran)]
      return(eval(parse_exp(one_order)))
    } else {
      value <- build_gran(x, lgran, lgran_ordr1) +
        gran_convert(lgran, lgran_ordr1) *
          (build_gran(x, lgran_ordr1, ugran) - 1)
      return(value)
    }
  }
}



# the lookup table - this needs to be changed if other granularities are included
lookup_table <- tibble::tibble(
  granularity = c("second", "minute", "qhour", "hhour", "hour", "day", "week", "fortnight", "month", "quarter", "semester", "year"),
  constant = c(60, 15, 2, 2, 24, 7, 2, 2, 3, 2, 2, 1),
  convertfun = c("lubridate::second", "minute_qhour", "qhour_hhour", "hhour_hour", "lubridate::hour", "lubridate::wday", "week_fortnight", "fortnight_month", "month_quarter", "quarter_semester", "semester_year", 1),
  convertday = c("second_day", "minute_day", "qhour_day", "hhour_day", "lubridate::hour", 1, "lubridate::wday", "day_fortnight", "lubridate::mday", "lubridate::qday", "day_semester", "lubridate::yday"),
)





# provides the order difference between two granularities, also provide the upper granularity given the order
g_order <- function(gran1, gran2 = NULL, order = NULL) {
  granularity <- lookup_table$granularity
  index_gran1 <- granularity %>% match(x = gran1)
  if (!is.null(gran2)) {
    index_gran2 <- granularity %>% match(x = gran2)
    return(index_gran2 - index_gran1)
  }
  if (!is.null(order)) {
    return(granularity[index_gran1 + order])
  }
}

# provides the conversion factor between two granularities

gran_convert <- function(a, b = NULL, order = NULL) {
  a <- tolower(a)
  granularity <- lookup_table$granularity
  conv_fac <- lookup_table$constant
  index_gran1 <- granularity %>% match(x = a)
  granularity <- lookup_table$granularity

  if (!is.null(b)) {
    b <- tolower(b)
    if (!a %in% granularity | !b %in% granularity) {
      stop(paste0("granularity ", a, " and ", b, " both should be one of ", paste0(granularity, collapse = ", ")), call. = F)
    }


    if (g_order(a, b) < 0) {
      stop("Second temporal resolution should be higher in order than the first one. Try reversing their position")
    }
    if (g_order(a, b) == 0) {
      return(1)
    }
    else {
      return(conv_fac[index_gran1] * gran_convert(g_order(a, order = 1), b))
    }
  }
  if (!is.null(order)) {
    converter <- conv_fac[index_gran1]

    while (converter <= order) {
      index_gran1 <- index_gran1 + 1
    }
  }
}



# .shift_gran_names <- function(names, gran_start = length(gran_uniq)) {
#   if (gran_start != length(gran_uniq)) {
#     c(names[(gran_start + 1):length(gran_uniq)], names[1:gran_start])
#   } else {
#     names
#   }
# }
#









# all one order up functions


second_minute <- function(x, ...) {
  lubridate::second(x, ...)
}

minute_qhour <- function(x, ...) {
  lubridate::minute(x, ...) %% 15 + 1
}

qhour_hhour <- function(x, ...) {
  dplyr::if_else((lubridate::minute(x, ...) %% 30 + 1) <= 15, 1, 2)
}

hhour_hour <- function(x, ...) {
  dplyr::if_else(lubridate::minute(x, ...) < 30, 1, 2)
}

week_fortnight <- function(x, ...) {
  dplyr::if_else(day_fortnight(x) <= 7, 1, 2)
}

month_quarter <- function(x, ...) {
  value <- lubridate::month(x, ...) %% 3
  dplyr::if_else(value == 0, 3, value)
}

quarter_semester <- function(x, ...) {
  value <- lubridate::quarter(x, ...) %% 2
  dplyr::if_else(value == 0, 2, value)
  # otherwise remainder will change the label of the largest value to zero
}

semester_year <- function(x, ...) {
  lubridate::semester(x, ...)
}


# convert day functions
# all level starts from 0 like zero like hour_day (0, 1, 2, ....23)

# goes from 0 to 95
qhour_day <- function(x, ...) {

  # finds which quarter of the day
  ceiling(lubridate::minute(x, ...) / 15) + 4 * (lubridate::hour(x, ...))
}

# goes from 0 to (47
hhour_day <- function(x, ...) {
  (lubridate::hour(x, ...) + 1) * 2 - 1
}

# goes from 0 to (60*24 - 1)
minute_day <- function(x, ...) {
  lubridate::minute(x, ...) + (lubridate::hour(x, ...)) * 60
}
# goes from 0 to (60*60*24 - 1)
second_day <- function(x, ...) {
  lubridate::second(x, ...) + lubridate::minute(x, ...) * 60 + (lubridate::hour(x, ...)) * 60 * 60
}


day_semester <- function(x, ...) {

  # finds day of the semester
  which_sem <- lubridate::semester(x, ...)
  day_x <- lubridate::yday(x, ...)
  year_leap <- lubridate::leap_year(x, ...)
  div_indx <- dplyr::if_else(year_leap == "FALSE", 182, 183)
  dplyr::if_else(which_sem == 1, day_x, day_x - div_indx + 1)
}

day_fortnight <- function(x, ...) {
  value <- lubridate::yday(x) %% 14
  dplyr::if_else(value == 0, 14, value)
}


parse_exp <- function(y) {
  if (y == "1") {
    value <- 1
  }
  else {
    value <- parse(text = paste0(y, "(x,...)"))
  }
  return(value)
}
