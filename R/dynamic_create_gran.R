# data = read_rds("data/smart_meter_50_tbl.rds")
# smart_meter_50 <- data %>%
#   filter(lubridate::year(reading_datetime)==2013) %>%
#   mutate(reading_datetime = case_when(
#     duplicated(reading_datetime) ~ reading_datetime + lubridate::minutes(1),
#     TRUE ~ reading_datetime
#   ))
#
# smart_meter_50 %>% as_tsibble(index = reading_datetime, key = customer_id)


hierarchy <- tibble(units = c("index", "ball", "over", "inning", "match"), convert_fct  = c(1, 6, 20, 2, 1))
hierarchy

dynamic_create_gran <- function(.data, hierarchy_tbl = NULL, gran = NULL)
{
 x = .data[[index(.data)]] # index column

 if(class(x) %in% c("POSIXct", "POSIXt"))
   value = create_gran(.data, hierarchy_tbl, gran, ...)
else
{
  units <- hierarchy_tbl$units
  convert_fct <- hierarchy_tbl$convert_fct

  index_lower_gran <- match(gran, units)
  if(all(is.na(index_lower_gran)))
  {
    stop("linear granularity to be created should be one of the units present in the hierarchy table.")
  }

  gran_split <- stringr::str_split(gran, "_", 2) %>% unlist() %>% unique()


  linear_gran <- ceiling(x/(dynamic_gran_convert(hierarchy_tbl, units[1], gran_split[1])))

  circular_gran <-  linear_gran %% dynamic_gran_convert(hierarchy_tbl, gran_split[1], gran_split[2])


}

}



single_odr_gran <- function(.data, hierarchy_tbl = NULL, lower_gran = NULL, aperiodic_col = NULL, base_col = NULL,...) {

  units <- hierarchy_tbl$units
  convert_fct <- hierarchy_tbl$convert_fct

# only oen aperiodic elements considered
  index_aperiodic <- match(convert_fct, NA)
  index_aperiodic <-  index_aperiodic[!is.na(index_aperiodic)]

  if(all(is.na(index_aperiodic)))
  {
    denom <- dynamic_gran_convert(hierarchy_tbl,
                                                       lower_gran,
                                                       upper_gran = dynamic_g_order(hierarchy_tbl, lower_gran, order = 1))
    value = .data[[base_col]] %% denom

    value = dplyr::if_else(value!=0, value,denom)


  }
  else
  {

# index of lower gran in the hierarchy table
    index_lower_gran <- match(lower_gran, units)





    if(index_lower_gran==index_aperiodic)
    {
    value = .data[[aperiodic_col]]
    }
    else if(index_lower_gran > index_aperiodic)
    {

    denom <- dynamic_gran_convert(hierarchy_tbl, units[index_aperiodic+1], lower_gran)

    value = .data[[base_col]] %% denom

    value = dplyr::if_else(value!=0, value,denom)
    }
  }
    return(value)
  }
