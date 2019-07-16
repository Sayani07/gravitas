#harmony_tbl <- tsibbledata::nyc_bikes %>% harmony(lgran ="second", ugran = "year")
#save(harmony_tbl, file = "harmony_tbl.Rda")
#load("harmony_tbl.Rda")
harmony_tbl <- function(gran = NULL)
{
  granularity <- lookup_table$granularity
  gran_ind <- granularity[match(gran, granularity)]

  gran1_split <- stringr::str_split(harmony_tbl$x, "_", 2) %>% unlist()

}
