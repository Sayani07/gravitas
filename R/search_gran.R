#' Get set of possible temporal granularities for a given tsibble
#'
#' @param .data A tsibble object.
#' @param ugran Upper temporal unit. Typically, it is set as the most coarse temporal unit required in the analysis. Default is "year".
#' @param lgran Lowest temporal unit. For "regular" tsibble, lgran is the interval of the tsibble. It needs to be specified for "irregular" time intervals.
#' @param filter_in Choices of temporal units to be kept.
#' @param filter_out Choices of temporal units to be discarded.
#' @param ... Other arguments to be passed.
#' @return Set of possible granularities.
#' @examples
#' library(dplyr)
#' library(tsibbledata)
#' tsibbledata::vic_elec %>% search_gran(ugran = "month", filter_out = c("hhour", "fortnight"))
#' tsibbledata::gafa_stock %>% search_gran(ugran = "month", lgran = "week")
#' @export search_gran
search_gran <- function(.data, ugran = "year", lgran = NULL, filter_in = NULL, filter_out = NULL, ...) {
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

  if (g_order(lgran, ugran) == 0) {
    stop("lgran and ugran should be distinct")
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


  gran <- paste(gran1 = combn(gran2_set, 2)[1, ], gran2 = combn(gran2_set, 2)[2, ], sep = "_")

  gran_split <- stringr::str_split(gran, "_", 2) %>% unlist() %>% unique()


  # if (!is.null(filter_in)) {
  #   if (length(filter_in) == 1) {
  #     stop("Atleast two temporal units to be provided for filter_in ")
  #   }
  if (!is.null(filter_in)) {
    data_names <- names(.data)
    exhaust_set <-  c(data_names, granularity, "wknd_wday")
    if (!all(filter_in %in% exhaust_set)) {
      stop("temporal units to be filtered in not found: make sure vector contains units which are between lgran and ugran or appear in the data")
    }
#if all filter_in are column variables
 if(all(filter_in %in% c("wknd_wday",data_names)==TRUE))
 {
    gran_sub <- paste(gran1 = combn(gran_split, 2)[1, ], gran2 = combn(gran_split, 2)[2, ], sep = "_")

    gran <- c(gran_sub, filter_in)
 }

# if all filter_in are not column variables

      else
      {
           filter_in_sub <- filter_in[match(granularity, filter_in)]
           filter_in_sub <- filter_in_sub[!is.na(filter_in_sub)]
           gran_split <- c(filter_in_sub, gran_split) %>% unique()
           gran_sub <- paste(gran1 = combn(gran_split, 2)[1, ], gran2 = combn(gran_split, 2)[2, ], sep = "_")

        #all are temporal units
        if(all(filter_in %in% granularity)==TRUE)
{
         gran <-  gran_sub
}

        else{
          filter_in_sub1 <- filter_in[match(data_names, filter_in)]
          filter_in_sub2 <- filter_in_sub1[!is.na(filter_in_sub1)]
        gran <- c(gran_sub, filter_in_sub2)
        }
      }
    }

  # if(all(filter_in %in% data_names)==TRUE){
  #       gran_input <- filter_in[!is.na(match(filter_in, data_names))]
  #       gran2 <- c(gran, gran_input)
  #       gran <- gran2
  #     }
  #

 #   {
 #    filter_in <- filter_in[match(granularity, filter_in)]
 #    filter_in <- filter_in[!is.na(filter_in)]
 #    gran_split <- gran_split[match(filter_in, gran_split)]
 #    gran1 <- paste(gran1 = combn(gran_split, 2)[1, ], gran2 = combn(gran_split, 2)[2, ], sep = "_")
 #    gran <- gran1
 # }
# if all filter_in are column variables
      # if(all(filter_in %in% data_names)==TRUE){
      # gran_input <- filter_in[!is.na(match(filter_in, data_names))]
      # gran2 <- c(gran, gran_input)
      # gran <- gran2
      # }
# if some filter_in are temporal units and others are column variables
  #     else{
  #       gran <- c(gran1, gran2)
  #       filter_in <- filter_in[match(granularity, filter_in)]
  #       filter_in <- filter_in[!is.na(filter_in)]
  #       gran_split <- gran_split[match(filter_in, gran_split)]
  #       # gran_split <- c(gran_split, filter_in) %>% unique()
  #       gran1 <- paste(gran1 = combn(gran_split, 2)[1, ], gran2 = combn(gran_split, 2)[2, ], sep = "_")
  #
  #     }
  #
  #
  #   }
  # }
  else if (!is.null(filter_out)) {
    if (!all(filter_out %in% granularity)) {
      stop("temporal units to be filtered out not found: make sure vector contains units which are between lgran and ugran")
    }
    filter_out <- filter_out[match(granularity, filter_out)]
    filter_out <- filter_out[!is.na(filter_out)]
    gran_split <- gran_split[-match(filter_out, gran_split)]
    gran <- paste(gran1 = combn(gran_split, 2)[1, ], gran2 = combn(gran_split, 2)[2, ], sep = "_")
  }

  return(gran)
}
