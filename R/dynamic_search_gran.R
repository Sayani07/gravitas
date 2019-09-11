dynamic_search_gran <- function(.data, lowest_unit = NULL, highest_unit = NULL, hierarchy_tbl = NULL, filter_in = NULL, filter_out = NULL, ...) {

  x <- .data[[rlang::as_string(tsibble::index(.data))]]


  if (!tsibble::is_tsibble(.data)) {
    stop("must use tsibble")
  }

  if (is.null(highest_unit)) {
    highest_unit = dplyr::last(hierarchy_tbl$units)
  }

  if (is.null(lowest_unit)) {
    lowest_unit = dplyr::first(hierarchy_tbl$units)
  }

  if(any(class(x) %in% c("POSIXct", "POSIXt")))
  {
    hierarchy_tbl = lookup_table
  }
  units <- hierarchy_tbl$units
  convert_fct <- hierarchy_tbl$convert_fct

  # Put the last element of the vector units as the upper most unit desired - default
  if (!(highest_unit %in% units))
  {
    stop("upper unit must be listed as an element in the  hierarchy table")
  }

  # Put the first element of the vector units as the lowest most unit desired - default

  if (!(lowest_unit %in% units))
  {
    stop("lower unit must be listed as an element in the hierarchy table")
  }


  if (dynamic_g_order(lowest_unit, highest_unit, hierarchy_tbl) == 0) {
    stop("lowest_unit and highest_unit should be distinct")
  }

else if (dynamic_g_order(lowest_unit, highest_unit, hierarchy_tbl) < 0) {
    stop("granularities should be of the form finer to coarser. Try swapping the order of the units.")
}

else{


  # if (dynamic_g_order(hierarchy_tbl, lowest_unit, highest_unit) == 1) {
  #   stop("Only one unit ", lowest_unit, "_", {
  #     highest_unit
  #   }, " can be formed. Function requires checking compatibility for bivariate granularities")
  # }

  ind <- .data[[rlang::as_string(tsibble::index(.data))]]
  index_gran1 <- units %>% match(x = lowest_unit)
  index_gran2 <- units %>% match(x = highest_unit)
  gran2_set <-   units[index_gran1:index_gran2]


  gran <- paste(gran1 = combn(gran2_set, 2)[1, ], gran2 = combn(gran2_set, 2)[2, ], sep = "_")

  gran_split <- stringr::str_split(gran, "_", 2) %>% unlist() %>% unique()


  # if (!is.null(filter_in)) {
  #   if (length(filter_in) == 1) {
  #     stop("Atleast two temporal units to be provided for filter_in ")
  #   }
  if (!is.null(filter_in)) {
    data_names <- names(.data)
    exhaust_set <-  c(data_names, units, "wknd_wday")
    if (!all(filter_in %in% exhaust_set)) {
      stop("temporal units to be filtered in not found: make sure vector contains units which are between lowest_unit and highest_unit or appear in the data")
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
      filter_in_sub <- filter_in[match(units, filter_in)]
      filter_in_sub <- filter_in_sub[!is.na(filter_in_sub)]
      gran_split <- c(filter_in_sub, gran_split) %>% unique()
      gran_sub <- paste(gran1 = combn(gran_split, 2)[1, ], gran2 = combn(gran_split, 2)[2, ], sep = "_")

      #all are temporal units
      if(all(filter_in %in% units)==TRUE)
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

  else if (!is.null(filter_out)) {
    if (!all(filter_out %in% units)) {
      stop("temporal units to be filtered out not found: make sure vector contains units which are between lowest_unit and highest_unit")
    }
    filter_out <- filter_out[match(units, filter_out)]
    filter_out <- filter_out[!is.na(filter_out)]
    gran_split <- gran_split[-match(filter_out, gran_split)]
    gran <- paste(gran1 = combn(gran_split, 2)[1, ], gran2 = combn(gran_split, 2)[2, ], sep = "_")
  }

  return(gran)
}
}



dynamic_g_order <- function(lower_gran = NULL, upper_gran = NULL, hierarchy_tbl = NULL, order = NULL,  ...){


  units <- hierarchy_tbl$units
  convert_fct <- hierarchy_tbl$convert_fct

  # Put the first element of the vector units as the lowest most unit desired - default
  # if (is.null(lowest_unit)) {
  #   lowest_unit = dplyr::first(hierarchy_tbl$units)
  # }

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

dynamic_gran_convert <- function(lower_gran = NULL, upper_gran = NULL, hierarchy_tbl = NULL,order = NULL) {



  units <- hierarchy_tbl$units
  convert_fct <- hierarchy_tbl$convert_fct

  index_l <- units %>% match(x = lower_gran)

  if (!is.null(lower_gran)) {
    if (!lower_gran %in% units | !upper_gran %in% units) {
      stop(paste0("units ", lower_gran, " and ", upper_gran, " both should be one of ", paste0(units, collapse = ", ")), call. = F)
    }



    if (dynamic_g_order(lower_gran, upper_gran, hierarchy_tbl) < 0) {
      stop("Order of second unit should be larger than the first one. Try reversing their position")
    }
    if (dynamic_g_order(lower_gran, upper_gran, hierarchy_tbl) == 0) {
      return(1)
    }
    else {
      return(convert_fct[index_l] * dynamic_gran_convert(dynamic_g_order(lower_gran, hierarchy_tbl = hierarchy_tbl, order = 1), upper_gran ,hierarchy_tbl))
    }
  }
  if (!is.null(order)) {
    converter <- convert_fct[index_l]

    while (converter <= order) {
      index_l <- index_l + 1
    }
  }
}

