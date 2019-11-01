#' Search for granularities
#'
#' Get set of possible granularities that can be considered exhaustively depending on the frequency of the data.
#'
#' @param .data A tsibble object.
#' @param lowest_unit Typically set as the finest unit required for analysis. For "regular" tsibble, lgran is the interval of the tsibble. It needs to be specified for "irregular" time intervals.For non-temporal data, default is the first unit specified in the hierarchy table.
#' @param highest_unit Typically set as the most coarse unit required for analysis.For temporal data, default is "year" and for non-temporal data, default is set as the last unit specified in the hierarchy table
#' @param hierarchy_tbl A hierarchy table specifying the hierarchy of units and their relationships.
#' @param filter_in Choices of temporal units to be kept.
#' @param filter_out Choices of temporal units to be discarded.
#' @param ... Other arguments to be passed.
#' @return Set of possible granularities.
#' @examples
#' library(tsibbledata)
#' vic_elec %>% search_gran(lowest_unit = "hour", highest_unit = "month")
#' @export
search_gran <- function(.data,
                        lowest_unit = NULL,
                        highest_unit = NULL,
                        hierarchy_tbl = NULL,
                        filter_in = NULL,
                        filter_out = NULL,
                        ...) {
  if (!tsibble::is_tsibble(.data)) {
    stop("must use tsibble")
  }

  x <- .data[[rlang::as_string(tsibble::index(.data))]]

  # if class is timestamp, then use predefined lookup table, have to state hierarchy table for non-temporal data
  if (any(class(x) %in% c("POSIXct", "POSIXt"))) {
    hierarchy_tbl <- lookup_table
  }
  else if (is.null(hierarchy_tbl)) {
    stop("Hierarchy table must be provided when class of index of the tsibble is not date-time")
  }

  units <- hierarchy_tbl$units
  convert_fct <- hierarchy_tbl$convert_fct

  # Put the last element of the vector units as the upper most unit desired - default
  if (is.null(highest_unit)) {
    highest_unit <- dplyr::last(hierarchy_tbl$units)
  }
  # check if input for highest and lowest units are in the list of units in hierarchy table

  else if (!(highest_unit %in% hierarchy_tbl$units)) {
    stop("highest unit must be listed as an element in the  hierarchy table")
  }

  # Put the first element of the vector units as the lowest most unit desired - default
  if (is.null(lowest_unit)) {
    if (any(class(x) %in% c("POSIXct", "POSIXt"))) {
      # put the interval of the tsibble as default of lowest_unit if it is missing
      if (tsibble::is_regular(.data)) {
        interval_ts <- tsibble::interval(.data)
        data_interval <- interval_ts[interval_ts != 0]
        lgran_iden <- names(data_interval)
        lgran_multiple <- data_interval[[1]]
        if (lgran_multiple == 1) {
          lowest_unit <- lgran_iden
        }
        else if (lgran_multiple > 1) {
          index_lgran <- units %>% match(x = lgran_iden)

          if (convert_fct[index_lgran] < lgran_multiple) {
            convert_fct[index_lgran] <- convert_fct[index_lgran] * convert_fct[index_lgran + 1]
            last_index <- index_lgran + 1
          }
          lowest_unit <- units[last_index + 1]
        }
      }

      else if (!tsibble::is_regular(.data)) {
        stop("lowest_unit must be provided when the tsibble is irregularly spaced")
      }
    }
    else {
      lowest_unit <- dplyr::first(units)
    }
  }

  # check if input for lowest unit is allowed
  else if (!(lowest_unit %in% units)) {
    stop("lowest unit must be listed as an element in the hierarchy table")
  }

  # check if input for highest and lowest units are distinct
  if (dynamic_g_order(lowest_unit, highest_unit, hierarchy_tbl) == 0) {
    stop("lowest_unit and highest_unit should be distinct")
  }

  # check if input for highest and lowest units are reversed

  else if (dynamic_g_order(lowest_unit, highest_unit, hierarchy_tbl) < 0) {
    stop("granularities should be of the form finer to coarser. Try swapping the order of the units.")
  }

  # if input for highest and lowest units are distinct and in the right order
  else {
    index_gran1 <- units %>% match(x = lowest_unit)
    index_gran2 <- units %>% match(x = highest_unit)
    gran2_set <- units[index_gran1:index_gran2]

    # all possible granularities from lowest to highest units except ones that have been filtered in separately
    gran <- paste(gran1 = utils::combn(gran2_set, 2)[1, ], gran2 = utils::combn(gran2_set, 2)[2, ], sep = "_")

    gran_split <- stringr::str_split(gran, "_", 2) %>%
      unlist() %>%
      unique()

    # to join units in the list of gran which are either columns from data or wknd_wday

    if (!is.null(filter_in)) {
      data_names <- names(.data)
      exhaust_set <- c(data_names, units, "wknd_wday")
      if (!all(filter_in %in% exhaust_set)) {
        stop("temporal units to be filtered in not found: make sure vector contains units which are between lowest_unit and highest_unit or appear in the data")
      }
      # if all filter_in are column variables
      if (all(filter_in %in% c("wknd_wday", data_names) == TRUE)) {
        gran_sub <- paste(gran1 = utils::combn(gran_split, 2)[1, ], gran2 = utils::combn(gran_split, 2)[2, ], sep = "_")

        gran <- c(gran_sub, filter_in)
      }

      # if all filter_in are not column variables

      else {
        filter_in_sub <- filter_in[match(units, filter_in)]
        filter_in_sub <- filter_in_sub[!is.na(filter_in_sub)]
        gran_split <- c(filter_in_sub, gran_split) %>% unique()
        gran_sub <- paste(gran1 = utils::combn(gran_split, 2)[1, ], gran2 = utils::combn(gran_split, 2)[2, ], sep = "_")

        # all are temporal units
        if (all(filter_in %in% units) == TRUE) {
          gran <- gran_sub
        }

        else {
          filter_in_sub1 <- filter_in[match(data_names, filter_in)]
          filter_in_sub2 <- filter_in_sub1[!is.na(filter_in_sub1)]
          gran <- c(gran_sub, filter_in_sub2)
        }
      }
    }

    # # to remove units in the list of gran which are mentioned in filter_out
    else if (!is.null(filter_out)) {
      if (!all(filter_out %in% units)) {
        stop("temporal units to be filtered out not found: make sure vector contains units which are between lowest_unit and highest_unit")
      }
      filter_out <- filter_out[match(units, filter_out)]
      filter_out <- filter_out[!is.na(filter_out)]
      gran_split <- gran_split[-match(filter_out, gran_split)]
      gran <- paste(gran1 = utils::combn(gran_split, 2)[1, ], gran2 = utils::combn(gran_split, 2)[2, ], sep = "_")
    }

    return(gran)
  }
}



dynamic_g_order <- function(lower_gran = NULL, upper_gran = NULL, hierarchy_tbl = NULL, order = NULL, ...) {
  units <- hierarchy_tbl$units
  convert_fct <- hierarchy_tbl$convert_fct

  index_l <- units %>% match(x = lower_gran)
  if (!is.null(upper_gran)) {
    index_h <- units %>% match(x = upper_gran)
    return(index_h - index_l)
  }
  if (!is.null(order)) {
    return(units[index_l + order])
  }
}

# provides the conversion factor between two granularities
dynamic_gran_convert <- function(lower_gran = NULL, upper_gran = NULL, hierarchy_tbl = NULL, order = NULL) {
  units <- hierarchy_tbl$units
  convert_fct <- hierarchy_tbl$convert_fct

  index_l <- units %>% match(x = lower_gran)

  if (!is.null(lower_gran)) {
    if (!lower_gran %in% units | !upper_gran %in% units) {
      stop(paste0("units ", lower_gran, " and ", upper_gran, " both should be one of ", paste0(units, collapse = ", ")), call. = FALSE)
    }

    if (dynamic_g_order(lower_gran, upper_gran, hierarchy_tbl) < 0) {
      stop("Order of second unit should be larger than the first one. Try reversing their position")
    }
    if (dynamic_g_order(lower_gran, upper_gran, hierarchy_tbl) == 0) {
      return(1)
    }
    else {
      return(convert_fct[index_l] * dynamic_gran_convert(dynamic_g_order(lower_gran, hierarchy_tbl = hierarchy_tbl, order = 1), upper_gran, hierarchy_tbl))
    }
  }
  if (!is.null(order)) {
    converter <- convert_fct[index_l]

    while (converter <= order) {
      index_l <- index_l + 1
    }
  }
}
