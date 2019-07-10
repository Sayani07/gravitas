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

  for (i in nrow(mat)) {

    gran1[i] = mat$x[i]
    gran2[i] = mat$y[i]
    advice <- gran_advice(.data, gran1[i], gran2[i], response, ...)
    # create plot for each county in mat
    gran1_split <- stringr::str_split(gran1[i], "_", 2) %>% unlist()
    gran2_split <- stringr::str_split(gran2[i], "_", 2) %>% unlist()
    var1 <- gran1_split[1]
    var2 <- gran1_split[2]
    var3 <- gran2_split[1]
    var4 <- gran2_split[2]

    data_mutate <- .data %>% create_gran(var1, var2) %>% create_gran(var3, var4)

    plot <-
      data_mutate  %>% as_tibble(.name_repair = "minimal") %>%
      ggplot2::ggplot(ggplot2::aes(x = data_mutate[[gran2]], y = response))+       geom_() +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(~ data_mutate[[gran1]]) +
      ggplot2::ylab("Response") +
      ggplot2::ggtitle(paste0("Plot of ", gran1, " given ", gran2))

    print(plot)
  }
  }

  }




}

# advise function for which plots to choose depending on levels of facets and x-axis
#gran_advice(.data, gran1="hour_week", gran2 = "day_month")
gran_advice <- function(.data, gran1, gran2, response = NULL, ...)
{
  if (!tsibble::is_tsibble(.data)) {
    stop("must use tsibble")
  }

  proxy_harmony <- is.harmony(.data, gran1, gran2, response = NULL, ...)
  proxy_homogenous <- is.homogenous(.data, gran1, gran2, response = NULL, ...)

  if(proxy_harmony == "FALSE"){
    warning("granularities chosen are clashes")
  }
  if(proxy_homogenous$inter_facet_homo =="FALSE"){
    warning("Number of observations for one or more combinations vary across facets")
  }
  if(proxy_homogenous$intra_facet_homo =="FALSE"){
    warning("Number of observations for one or more combinations vary within facets")
  }
  if(proxy_homogenous$decile_nobs_proxy !=0){
    warning("Decile plot not recommended as number of observation too few for one or more combinations")
  }
  if(proxy_homogenous$percentile_nobs_proxy !=0){
    warning("Percentile plot not recommended as number of observations too few for one or more combinations")
  }

  #inter facet homogeneity

data_count <- harmony_obj(.data, gran1, gran2, response, ...)


  gran1_level <- data_count %>% dplyr::select(!!rlang::quo_name(gran1)) %>% dplyr::distinct() %>%  nrow()
  gran2_level <- data_count %>% dplyr::select(!!rlang::quo_name(gran2)) %>%  dplyr::distinct() %>%  nrow()

  medium_facet = 12
  min_x = 15
  max_facet = 31
  max_x =  1000

  facet_h <- 31
  facet_m <- 15
  facet_l <- 9

  x_h <- 31
  x_m <- 15
  x_l <- 9
  # very high facet levels
  if(gran1_level > facet_h)
  {
    warning(paste("Facetting not recommended: too many categories in ", gran1))
  } # high facet levels
  else if(dplyr::between(gran1_level, facet_m, facet_h) &  gran2_level> x_h ) # (high, very high)
  {
    plots_list = c("percentile", "decile")
  }

  else if(dplyr::between(gran1_level, facet_m, facet_h) & dplyr::between(gran2_level, x_m, x_h ))#(high, high)
  {
    plots_list = c("percentile", "decile")
  }

  else if(dplyr::between(gran1_level, facet_m, facet_h) &  dplyr::between(gran2_level, x_l, x_m ))#(high, medium)
  {
    plots_list = c("percentile", "decile")
  }
  else if(dplyr::between(gran1_level, facet_m, facet_h) &  gran2_level< x_l )#(high, low)
  {
    plots_list = c("ridge", "violin", "lv", "density")
  }
  # medium facet levels
  else if(dplyr::between(gran1_level, facet_l, facet_m) &  gran2_level> x_h )# (medium, very high)
  {
    plots_list = c("percentile", "decile")
  }
  else if(dplyr::between(gran1_level, facet_l, facet_m) & dplyr::between(gran2_level, x_m, x_h)) # (medium, high)
  {
    plots_list = c("percentile", "decile")
  }

  else if(dplyr::between(gran1_level, facet_l, facet_m) &  dplyr::between(gran2_level, x_l, x_m)) # (medium, medium)
  {
    plots_list = c("percentile", "decile")
  }
  else if(dplyr::between(gran1_level, facet_l, facet_m)  &  gran2_level< x_l ) # (medium, low)
  {
    plots_list = c("ridge", "violin", "lv", "density")
  }
  # low facet levels
  else if(gran1_level < facet_l &  gran2_level> x_h ) #(low, very high)
  {
    plots_list = c("percentile", "decile")
  }
  else if(gran1_level < facet_l & dplyr::between(gran2_level, x_m, x_h)) #(low, high)
  {
    plots_list = c("percentile", "decile", "boxplot", "lv")
  }

  else if(gran1_level < facet_l &  dplyr::between(gran2_level, x_l, x_m)) #(low, medium)
  {
    plots_list = c("ridge", "violin", "lv", "density", "percentile", "decile")
  }
  else if(gran1_level < facet_l  &  gran2_level< x_l ) #(low, low)
  {
    plots_list = c("ridge", "violin", "lv", "density", "percentile", "decile")
  }

  return(plots_list)

}



is.homogenous <- function(.data, gran1, gran2, response = NULL, ...)
{
  if (!tsibble::is_tsibble(.data)) {
    stop("must use tsibble")
  }

  #inter facet homogeneity

  data_count <- harmony_obj(.data, gran1, gran2, response, ...)

  inter_facet_homogeneity <- data_count %>% dplyr::group_by(!!rlang::quo_name(gran1)) %>% dplyr::summarise(min_c = min(nobs), max_c = max(nobs)) %>% dplyr::summarise(sum = sum(dplyr::if_else(min_c==max_c, 0, 1))) %>% dplyr::mutate(value = dplyr::if_else(sum==0, "TRUE", "FALSE"))

  # intra facet homogeneity
  intra_facet_homogeneity <- data_count %>% dplyr::group_by(!!rlang::quo_name(gran2)) %>% dplyr::summarise(min_c = min(nobs), max_c = max(nobs)) %>% dplyr::summarise(sum = sum(dplyr::if_else(min_c==max_c, 0, 1))) %>% dplyr::mutate(value = dplyr::if_else(sum==0, "TRUE", "FALSE"))

  decile_nobs <- sum(if_else(data_count$nobs<30, 1, 0))
  percentile_nobs <- sum(if_else(data_count$nobs<300, 1, 0))

value_r <- tibble(inter_facet_homo = inter_facet_homogeneity$value, intra_facet_homo = intra_facet_homogeneity$value, decile_nobs_proxy = decile_nobs, percentile_nobs_proxy = percentile_nobs)

value_r
}
