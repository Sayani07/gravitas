#' Get combination of granularities of a date time
#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param .data a tsibble
#' @param lgran the lower level granularity to be paired
#' @param ugran the upper level granularity to be paired
#' @param pair the harmony pair that is required to be plotted
#' @param response response variable to be plotted
#' @param ... other arguments to be passed for appropriate labels
#' @return combination of granularities of x as a number
#
#' @examples
#' library(dplyr)
#' library(tsibble)
#' tsibbledata::aus_elec %>%as_tsibble() %>% tail() %>%  create_gran("hour", "week")
#' @export granplot


granplot = function(.data, ugran = NULL, lgran = NULL, pair = 1, response = NULL, ...)
{
  mat = .data %>% harmony(ugran = ugran, lgran = lgran)
  mat$x = as.character(mat$x)
  mat$y = as.character(mat$y)

  gran1 = mat$x[pair]
  gran2 = mat$y[pair]

  gran1_split <- stringr::str_split(gran1, "_", 2) %>% unlist()
  gran2_split <- stringr::str_split(gran2, "_", 2) %>% unlist()
  var1 <- gran1_split[1]
  var2 <- gran1_split[2]
  var3 <- gran2_split[1]
  var4 <- gran2_split[2]

  data_mutate <- .data %>% create_gran(var1, var2) %>% create_gran(var3, var4)


  data_mutate  %>% as_tibble() %>%
    ggplot2::ggplot(ggplot2::aes(x = gran1, y = response)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(~ data_mutate[[gran2]]) +
    ggplot2::ylab("Total Count") +
    ggplot2::ggtitle(paste0("Plot of ", gran1, " given ", gran2))

}
