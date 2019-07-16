#' Get set of temporal granularities for a given tsibble
#'
#' @param .data a tsibble object
#' @param ugran upper granularity
#' @param ... added arguments to be passed
#' @return set of granularities
#' @examples
#' library(dplyr)
#' library(tsibbledata)
#' tsibbledata::gafa_stock %>% harmony(lgran = "hour", ugran = "week")
#' tsibbledata::vic_elec %>% harmony(ugran = "day")
#' @export search_gran
search_gran <- function(.data, ugran = "year", lgran = NULL, ...){
granularity <- lookup_table$granularity
constant <- lookup_table$constant

if (!tsibble::is_tsibble(.data)) {
  stop("must use tsibble")
}

if (is.null(ugran)) {
  stop("Argument ugran is missing, with no default")
}

if (tsibble::is_regular(.data)) {
  interval_ts <- tsibble::interval(.data)
  data_interval <- interval_ts[interval_ts != 0]
  if (is.null(lgran)) {
    lgran_iden <- names(data_interval)
    lgran_multiple <- data_interval[[1]]
    if (lgran_multiple == 1) {
      lgran <- lgran_iden
    }
    else if (lgran_multiple > 1) {
      index_lgran <- granularity %>% match(x = lgran_iden)


      if (constant[index_lgran] < lgran_multiple) {
        constant[index_lgran] <- constant[index_lgran] * constant[index_lgran + 1]
        last_index <- index_lgran + 1
      }
      lgran <- granularity[last_index + 1]
    }
  }
}

else if (!tsibble::is_regular(.data)) {
  if (is.null(lgran)) {
    stop("lgran must be provided when the tsibble is irregularly spaced")
  }
}

if (g_order(lgran, ugran) == 1) {
  stop("Only one granularity ", lgran, "_", {
    ugran
  }, " can be formed. Function requires checking compatibility for bivariate granularities")
}

ind <- .data[[rlang::as_string(tsibble::index(.data))]]
index_gran1 <- granularity %>% match(x = lgran)
index_gran2 <- granularity %>% match(x = ugran)
gran2_set <- lookup_table$granularity[index_gran1:index_gran2]



set1 <- paste(gran1 = combn(gran2_set, 2)[1, ], gran2 = combn(gran2_set, 2)[2, ], sep = "_")

return(set1)
}

filter_gran <- function(.data, ugran = "year", lgran = NULL, filter_in =  NULL, filter_out = NULL, ...){
  gran <- search_gran(.data, ugran, lgran,...)
  gran_split <- stringr::str_split(gran, "_", 2) %>% unlist() %>% unique()

  if(!is.null(filter_in)){
    gran_split <- gran_split[match(filter_in, gran_split)]
    gran <- paste(gran1 = combn(gran_split, 2)[1, ], gran2 = combn(gran_split, 2)[2, ], sep = "_")
  }

  else if(!is.null(filter_out))
  {
    gran_split <- gran_split[-match(filter_out, gran_split)]
    gran <- paste(gran1 = combn(gran_split, 2)[1, ], gran2 = combn(gran_split, 2)[2, ], sep = "_")
  }

  return(gran)
}
