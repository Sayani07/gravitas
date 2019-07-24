
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



# provides the order difference between two granularities, also provide the upper granularity given the order
g_order <- function(, gran1, gran2 = NULL, order = NULL) {
  units <- hierarchy_tbl$units
  convert_fct <- hierarchy_tbl$convert_fct

  index_gran1 <- units %>% match(x = gran1)
  if (!is.null(gran2)) {
    index_gran2 <- units %>% match(x = gran2)
    return(index_gran2 - index_gran1)
  }
  if (!is.null(order)) {
    return(units[index_gran1 + order])
  }
}
