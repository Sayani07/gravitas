
hierarchy <- tibble(units = c("ball", "over", "quarter", "semester", "match"), convert_fct  = c(6, 5, 2, 2, 1))
hierarchy


search_gran_v1 <- function(.data, hierarchy_tbl = NULL, ugran = NULL, lgran = NULL, filter_in = NULL, filter_out = NULL, ...) {

  units <- hierarchy_tbl$units
  convert_fct <- hierarchy_tbl$convert_fct

  if (!tsibble::is_tsibble(.data)) {
    stop("must use tsibble")
  }


# Put the last element of the vector units as the upper most unit desired - default
  if (is.null(ugran)) {
    ugran = dplyr::last(hierarchy_tbl$units)
  }
  else if (!(ugran %in% units))
  {
     stop("upper unit must be listed as an element in the  hierarchy table")
    }

  # Put the first element of the vector units as the lowest most unit desired - deafult
  if (is.null(lgran)) {
    lgran = dplyr::first(hierarchy_tbl$units)
  } else if (!(lgran %in% units))
  {
    stop("lower unit must be listed as an element in the hierarchy table")
  }

  return(c(lgran, ugran))

# # Put the first element of the vector units/interval of the tsibble as the least most unit desired
#   if (tsibble::is_regular(.data)) {
#     interval_ts <- tsibble::interval(.data)
#     data_interval <- interval_ts[interval_ts != 0]
#     if (is.null(lgran)) {
#       lgran_iden <- names(data_interval)
#       lgran_multiple <- data_interval[[1]]
#       if (lgran_multiple == 1) {
#         lgran <- lgran_iden
#       }
#       else if (lgran_multiple > 1) {
#         index_lgran <- granularity %>% match(x = lgran_iden)
#
#
#         if (convert_fct[index_lgran] < lgran_multiple) {
#           convert_fct[index_lgran] <- convert_fct[index_lgran] * convert_fct[index_lgran + 1]
#           last_index <- index_lgran + 1
#         }
#         lgran <- granularity[last_index + 1]
#       }
#     }
#   }

}



# provides the order difference between two granularities, also provide the upper granularity given the order (given a hierachy table)

g_order <- function(hierarchy_tbl = lookup_table, lowest_unit = NULL, highest_unit = NULL, order = NULL,...){


  units <- hierarchy_tbl$units
  convert_fct <- hierarchy_tbl$convert_fct

  # Put the first element of the vector units as the lowest most unit desired - default
  if (is.null(lowest_unit)) {
    lowest_unit = dplyr::first(hierarchy_tbl$units)
  }
  else if (!(lowest_unit %in% units))
  {
    stop("lowest unit must be listed as an element in the  hierarchy table")
  }

  index_l <- units %>% match(x = lowest_unit)
  if (!is.null(highest_unit)) {
    index_h <- units %>% match(x = highest_unit)
    return(index_h - index_l)
  }
  if (!is.null(order)) {
    return(units[index_l + order])
  }

}

# provides the conversion factor between two granularities

gran_convert <- function(hierarchy_tbl = NULL,lowest_unit = NULL, highest_unit = NULL, order = NULL) {

  units <- hierarchy_tbl$units
  convert_fct <- hierarchy_tbl$convert_fct

  index_l <- units %>% match(x = lowest_unit)

  if (!is.null(lowest_unit)) {
    if (!lowest_unit %in% units | !highest_unit %in% units) {
      stop(paste0("units ", lowest_unit, " and ", highest_unit, " both should be one of ", paste0(units, collapse = ", ")), call. = F)
    }


    if (g_order(hierarchy_tbl, lowest_unit, highest_unit) < 0) {
      stop("Order of second unit should be larger than the first one. Try reversing their position")
    }
    if (g_order(hierarchy_tbl, lowest_unit, highest_unit) == 0) {
      return(1)
    }
    else {
      return(convert_fct[index_l] * gran_convert(hierarchy_tbl, g_order(hierarchy_tbl, lowest_unit, order = 1), highest_unit))
    }
  }
  if (!is.null(order)) {
    converter <- convert_fct[index_l]

    while (converter <= order) {
      index_l <- index_l + 1
    }
  }
}

