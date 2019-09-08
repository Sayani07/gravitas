create_single_gran <- function(.data, hierarchy_tbl = NULL, gran = NULL)
{
 x = .data[[index(.data)]] # index column

 if(class(x) %in% c("POSIXct", "POSIXt"))
   value = create_gran(.data, hierarchy_tbl, gran, ...)
else
{
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


  circular_gran <-  if_else(linear_gran %% denom == 0, denom, linear_gran %% denom)

}

}

dynamic_create_gran <-  function(.data, hierarchy_tbl = NULL, gran = NULL)
{

  if(class(x) %in% c("POSIXct", "POSIXt"))
    value = create_gran(.data, hierarchy_tbl = lookup_table, gran, ...)
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
    dynamic_gran_convert(hierarchy, lgran, lgran_ordr1) *
    (dynamic_create_gran(.data, hierarchy_tbl, gran_final) - 1)
 }
}
return(value)
}

#
# single_odr_gran <- function(.data, hierarchy_tbl = NULL, lower_gran = NULL, aperiodic_col = NULL, base_col = NULL,...) {
#
#   units <- hierarchy_tbl$units
#   convert_fct <- hierarchy_tbl$convert_fct
#
# # only oen aperiodic elements considered
#   index_aperiodic <- match(convert_fct, NA)
#   index_aperiodic <-  index_aperiodic[!is.na(index_aperiodic)]
#
#   if(all(is.na(index_aperiodic)))
#   {
#     denom <- dynamic_gran_convert(hierarchy_tbl,
#                                                        lower_gran,
#                                                        upper_gran = dynamic_g_order(hierarchy_tbl, lower_gran, order = 1))
#     value = .data[[base_col]] %% denom
#
#     value = dplyr::if_else(value!=0, value,denom)
#
#
#   }
#   else
#   {
#
# # index of lower gran in the hierarchy table
#     index_lower_gran <- match(lower_gran, units)
#
#
#
#
#
#     if(index_lower_gran==index_aperiodic)
#     {
#     value = .data[[aperiodic_col]]
#     }
#     else if(index_lower_gran > index_aperiodic)
#     {
#
#     denom <- dynamic_gran_convert(hierarchy_tbl, units[index_aperiodic+1], lower_gran)
#
#     value = .data[[base_col]] %% denom
#
#     value = dplyr::if_else(value!=0, value,denom)
#     }
#   }
#     return(value)
#   }
