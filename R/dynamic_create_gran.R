#' Build temporal granularities

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
#' @export dynamic_create_gran



create_single_gran <- function(.data, hierarchy_tbl = NULL, gran = NULL)
{
#  x = .data[[tsibble::index(.data)]] # index column
#
#  if(class(x) %in% c("POSIXct", "POSIXt"))
#    value = create_gran(.data, gran, ...)
# else
# {
  units <- hierarchy_tbl$units
  convert_fct <- hierarchy_tbl$convert_fct

  gran_split <- stringr::str_split(gran, "_", 2) %>% unlist() %>% unique()
  lgran = gran_split[1]
  ugran = gran_split[2]

  index_lower_gran <- match(gran_split[1], units)
  if(all(is.na(index_lower_gran)))
  {
    stop("linear granularity to be created should be one of the units present in the hierarchy table.")
  }



  linear_gran <- ceiling(x/(dynamic_gran_convert(hierarchy_tbl, units[1], lgran)))

  denom = dynamic_gran_convert(hierarchy_tbl, lgran, ugran)


  circular_gran <-  dplyr::if_else(linear_gran %% denom == 0, denom, linear_gran %% denom)


}

dynamic_create_gran <-  function(.data, hierarchy_tbl = NULL, gran = NULL, verify_col = FALSE, ...)
{

  x = .data[[tsibble::index(.data)]] # index column

  if(any(class(x) %in% c("POSIXct", "POSIXt")))
    value = create_gran(.data, gran,...)
  else
  {
  gran_split <- stringr::str_split(gran, "_", 2) %>% unlist() %>% unique()
  lgran = gran_split[1]
  ugran = gran_split[2]

lgran_ordr1 <- dynamic_g_order(hierarchy_tbl, lgran, order = 1)

gran_init <- paste(gran_split[1], lgran_ordr1, sep="_")
gran_final <- paste(lgran_ordr1, gran_split[2], sep="_")

if (dynamic_g_order(hierarchy_tbl, lgran, gran_split[2]) == 1) {
  value =  create_single_gran(.data, hierarchy_tbl, gran)
}
 else {
  value <- dynamic_create_gran(.data, hierarchy_tbl, gran_init) +
    dynamic_gran_convert(hierarchy_tbl, lgran, lgran_ordr1) *
    (dynamic_create_gran(.data, hierarchy_tbl, gran_final) - 1)
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

  gran_data <- dynamic_create_gran(.data, hierarchy_tbl, gran)

  data_col <- .data[[validate_col]]

  if(all.equal(data_col, gran_data)==TRUE)
    return(TRUE)
  else(FALSE)
}


