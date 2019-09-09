#' Build dynamic temporal granularities

#' @param .data A tsibble object.
#' @param gran the required granularity.
#' @param hierarchy_tbl A hierarchy table
#' @param ... Other arguments passed on to individual methods.
#' @return A tsibble with an additional column of granularity
#
#' @examples
#' library(dplyr)
#' library(tsibble)
#' tsibbledata::vic_elec %>% as_tsibble() %>% create_gran("hour_week") %>% tail()
#' @export dynamic_build_gran




dynamic_create_gran <- function(.data, gran1 = NULL,  label = TRUE, abbr = TRUE, ...) {


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


  x <- .data[[rlang::as_string(tsibble::index(.data))]]

  # wkday weekend treatment open

  if(gran1=="wknd_wday")

  {
    data_mutate <- .data %>% dplyr::mutate(L1 = dynamic_build_gran(.data, lgran = "day", ugran= "week", ...)) %>% dplyr::mutate(wknd_wday =
                                                                                                                          dplyr::if_else(L1 %in% c(6,7), "Weekend", "Weekday")
    )


    data_mutate %>%
      dplyr::select(-L1)

  }

  # wkday weekend treatment open

  else{

    gran1_split <- stringr::str_split(gran1, "_", 2) %>% unlist()
    lgran <- gran1_split[1]
    ugran <- gran1_split[2]


    data_mutate <- .data %>% dplyr::mutate(L1 = dynamic_build_gran(.data, lgran, ugran, ...))


    lev <- unique(data_mutate$L1)

    if (label) {
      if (lgran == "day" & ugran == "week") {

        data_mutate <- .data %>% dplyr::mutate(L1 = dynamic_build_gran(.data, lgran, ugran, week_start = getOption("lubridate.week.start", 1),label = TRUE))
        names <- levels(data_mutate$L1)

      }
      else if (lgran == "month" & ugran == "year") {
        # data_mutate <- .data %>% dplyr::mutate(L1 = build_gran(x, lgran, ugran, label = TRUE))
        # names <- unique(data_mutate$L1)
        #
        #       should correct it later
        #
        data_mutate <- .data %>% dplyr::mutate(L1 = lubridate::month(x,label = TRUE))
        names <- levels(data_mutate$L1)

      }
      # if not day_week or month_year
      else {
        #names <- as.character(1:length(unique(lev)))
        data_mutate$L1 = factor(data_mutate$L1)
        names <- levels(data_mutate$L1)
      }

      names_abbr <- substr(names, 1, 3)

      # What to do with the name if abbreviation
      if (abbr) names_gran <- names_abbr else names_gran <- names
    }

    #if not label
    else {

      data_mutate$L1 = factor(data_mutate$L1)
      names <- levels(data_mutate$L1)
      # names_gran <- as.character(1:length(unique(lev)))
      # data_mutate$L1 <- factor(data_mutate$L1, levels = names_gran)
    }




    data_mutate %>%
      dplyr::mutate(
        !!gran1 := L1
      ) %>%
      dplyr::select(-L1)
  }
}






dynamic_build_gran <-  function(.data, lgran = NULL, ugran = NULL,  hierarchy_tbl = NULL,  ...)
{

  x = .data[[tsibble::index(.data)]] # index column
#
#   gran_split <- stringr::str_split(gran, "_", 2) %>% unlist() %>% unique()
#   lgran = gran_split[1]
#   ugran = gran_split[2]

  if(any(class(x) %in% c("POSIXct", "POSIXt")))
    value = build_gran(.data, lgran = lgran, ugran = ugran,...)
  else
  {


    lgran_ordr1 <- dynamic_g_order(hierarchy_tbl, lgran, order = 1)

    gran_init <- paste(gran_split[1], lgran_ordr1, sep="_")
    gran_final <- paste(lgran_ordr1, gran_split[2], sep="_")

    if (dynamic_g_order(hierarchy_tbl, lgran, gran_split[2]) == 1) {
      value =  create_single_gran(.data, hierarchy_tbl, gran)
    }
    else {
      value <- dynamic_build_gran(.data, lgran, lgran_ordr1,  hierarchy_tbl) +
        dynamic_gran_convert(hierarchy_tbl, lgran, lgran_ordr1) *
        (dynamic_build_gran(.data, lgran_ordr1, ugran, hierarchy_tbl) - 1)
    }
  }
  return(value)

}


validate_gran <-  function(.data, hierarchy_tbl = NULL, gran = NULL, validate_col = NULL, ...)
{
  all_gran <- dynamic_search_gran(.data, hierarchy_tbl)

  if(!(gran %in% all_gran))# which granularity needs to be checked
  {
    stop("granularity to be validated needs to be one that can be formed from the hierarchy table.")
  }
  if(!(validate_col %in% names(.data)))  # column of data which has the granularity
  {
    stop("validate_col should be one of the columns of the data")
  }

  gran_data <- dynamic_build_gran(.data, hierarchy_tbl, gran)

  data_col <- .data[[validate_col]]

  if(all.equal(data_col, gran_data)==TRUE)
    return(TRUE)
  else(FALSE)
}




create_single_gran <- function(.data, hierarchy_tbl = NULL, gran = NULL)
{
 x = .data[[tsibble::index(.data)]] # index column
 units <- hierarchy_tbl$units
 convert_fct <- hierarchy_tbl$convert_fct

 gran_split <- stringr::str_split(gran, "_", 2) %>% unlist() %>% unique()
 lgran = gran_split[1]
 ugran = gran_split[2]


 if(class(x) %in% c("POSIXct", "POSIXt"))
   value = build_gran(.data, lgran = lgran, ugran = ugran,  ...)
else
{


  index_lower_gran <- match(gran_split[1], units)
  if(all(is.na(index_lower_gran)))
  {
    stop("linear granularity to be created should be one of the units present in the hierarchy table.")
  }



  linear_gran <- ceiling(x/(dynamic_gran_convert(hierarchy_tbl, units[1], lgran)))

  denom = dynamic_gran_convert(hierarchy_tbl, lgran, ugran)


  circular_gran <-  dplyr::if_else(linear_gran %% denom == 0, denom, linear_gran %% denom)


}
}


