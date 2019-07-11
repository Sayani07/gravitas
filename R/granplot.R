#' Get combination of granularities of a date time
#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param .data a tsibble
#' @param lgran the lower level granularity to be paired
#' @param ugran the upper level granularity to be paired
#' @param plot_type type of distribution plot.
#' @param response response variable to be plotted
#' @param ... other arguments to be passed for appropriate labels
#' @return combination of granularities of x as a number
#
#' @examples
#' library(dplyr)
#' library(tsibble)
#' tsibbledata::aus_elec %>%as_tsibble() %>% tail() %>%  create_gran("hour", "week")
#' @export granplot


granplot = function(.data, ugran = NULL, lgran = NULL, response = NULL, plot_type = NULL, ...)
{

  if(is.null(response))
  {
   stop("requires the following missing aesthetics: response")
  }

  mat = .data %>% harmony(ugran = ugran, lgran = lgran)
  mat$x = as.character(mat$x)
  mat$y = as.character(mat$y)


  for (i in nrow(mat)) {

    gran1 = mat$x[i]
    gran2 = mat$y[i]
    advice <- gran_advice(.data, gran1, gran2, response, ...)
    # create plot for each row in mat
    gran1_split <- stringr::str_split(gran1, "_", 2) %>% unlist()
    gran2_split <- stringr::str_split(gran2, "_", 2) %>% unlist()
    var1 <- gran1_split[1]
    var2 <- gran1_split[2]
    var3 <- gran2_split[1]
    var4 <- gran2_split[2]

    data_mutate <- .data %>% create_gran(var1, var2) %>% create_gran(var3, var4)

    if(is.null(plot_type)){
    plot_type <- advice[1]
    }

    p = data_mutate  %>% as_tibble(.name_repair = "minimal") %>%
      ggplot2::ggplot(ggplot2::aes(x = data_mutate[[gran2]], y = data_mutate[[response]])) +  ggplot2::facet_wrap(~ data_mutate[[gran1]]) +
      ggplot2::ggtitle(paste0(plot_type," plot across ", gran2, " given ", gran1)) + xlab(gran2) + ylab(response) +
      scale_fill_brewer()

    if(plot_type=="boxplot"){
    plot <- p +  ggplot2::geom_boxplot(...)
    }
    else if (plot_type=="violin")
    {
      plot <- p + ggplot2::geom_violin(...)
    }

    else if (plot_type=="lv")
    {
      plot <-
         p + geom_lv(aes(fill=..LV..),outlier.colour = "red",outlier.shape = 1, k=9)
    }
    else if(plot_type=="ridge")
    {
      plot <- data_mutate %>%
        ggplot2::ggplot(aes(x = data_mutate[[response]],y=data_mutate[[response]],group=data_mutate[[gran2]])) +
        ggridges::geom_density_ridges2() +
        ggplot2::facet_wrap(~ data_mutate[[gran1]]) +
        ggplot2::xlab(response) +
        ggplot2::ylab(data_mutate[[gran2]]) +
        ggplot2::ggtitle(paste0(plot_type," plot across ", gran2, " given ", gran1))
    }
    else if(plot_type=="decile")
    {
      d <- seq(0.1, 0.9, by = 0.1)
      decile_names <- purrr::map_chr(d, ~paste0(.x*100, "%"))

      decile_funs <- purrr::map(d, ~purrr::partial(quantile, probs = .x, na.rm = TRUE)) %>%
        rlang::set_names(nm = decile_names)


  data_dec <- data_mutate %>% as_tibble() %>%
        dplyr::group_by(data_mutate[[gran1]], data_mutate[[gran2]]) %>%
    dplyr::summarize_at(response, decile_funs) %>%
    tidyr::gather(quantile, value, - c(`data_mutate[[gran1]]`, `data_mutate[[gran2]]`)) %>% dplyr::select(
      !!rlang::quo_name(gran1) := `data_mutate[[gran1]]`,
      !!rlang::quo_name(gran2) := `data_mutate[[gran2]]`,
      quantile := quantile,
      value := value)


  plot <- data_dec %>%
        ggplot2::ggplot(aes(x = data_dec[[gran2]],y=value,group=as.factor(quantile), color = as.factor(quantile)))+
        ggplot2::geom_line() +
        ggplot2::facet_wrap(~ data_dec[[gran1]]) +
        ggplot2::ylab(response) +
        ggplot2::xlab(gran1) +
        ggplot2::ggtitle(paste0(plot_type," plot across ", gran2, " given ", gran1))
    }
    else if(plot_type=="percentile")
    {
      p <- seq(0.01, 0.99, by = 0.01)
      percentile_names <- purrr::map_chr(p, ~paste0(.x*100, "%"))

      percentile_funs <- purrr::map(p, ~purrr::partial(quantile, probs = .x, na.rm = TRUE)) %>%
        rlang::set_names(nm = percentile_names)


      data_pcntl <- data_mutate %>% tibble::as_tibble() %>%
        dplyr::group_by(data_mutate[[gran1]], data_mutate[[gran2]]) %>%
        dplyr::summarize_at(response, percentile_funs) %>%
        tidyr::gather(quantile, value, - c(`data_mutate[[gran1]]`, `data_mutate[[gran2]]`)) %>% dplyr::select(
          !!rlang::quo_name(gran1) := `data_mutate[[gran1]]`,
          !!rlang::quo_name(gran2) := `data_mutate[[gran2]]`,
          quantile := quantile,
          value := value)


      plot <- data_pcntl %>%
        ggplot2::ggplot(aes(x = data_pcntl[[gran2]],y=value,group=as.factor(quantile), color = quantile))+
        ggplot2::geom_line() +
        ggplot2::facet_wrap(~ data_pcntl[[gran1]]) +
        ggplot2::ylab(response) +
        ggplot2::xlab(gran1) +
        ggplot2::ggtitle(paste0(plot_type," plot across ", gran2, " given ", gran1)) + scale_fill_brewer()
    }
    print(plot)
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
    warning(paste("Number of observations for one or more combinations of",gran1, "and", gran2, "vary across facets"))
  }
  if(proxy_homogenous$intra_facet_homo =="FALSE"){
    warning(paste("Number of observations for one or more combinations of",gran1, "and", gran2, "vary within facets"))
  }
  if(proxy_homogenous$decile_nobs_proxy !=0){
    warning("Decile plot not recommended as number of observations too few for one or more combinations")
  }
  if(proxy_homogenous$percentile_nobs_proxy !=0){
    warning("Percentile plot not recommended as number of observations too few for one or more combinations")

  }

  #inter facet homogeneity

data_count <- harmony_obj(.data, gran1, gran2, response, ...)


  gran1_level <- data_count %>% dplyr::select(!!rlang::quo_name(gran1)) %>% dplyr::distinct() %>%  nrow()
  gran2_level <- data_count %>% dplyr::select(!!rlang::quo_name(gran2)) %>%  dplyr::distinct() %>%  nrow()

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
  if(dplyr::between(gran1_level, facet_m, facet_h) &  gran2_level> x_h ) # (high, very high)
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

  if( c("percentile" %in% plots_list & proxy_homogenous$percentile_nobs_proxy !=0))
  {
    plots_list <- plots_list[-which(plots_list=="percentile")]
  }

  if( c("decile" %in% plots_list & proxy_homogenous$decile_nobs_proxy !=0))
  {
    plots_list <- plots_list[-which(plots_list=="decile")]
  }


  print(plots_list)

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

  decile_nobs <- sum(dplyr::if_else(data_count$nobs<30, 1, 0))
  percentile_nobs <- sum(dplyr::if_else(data_count$nobs<300, 1, 0))

value_r <- tibble(inter_facet_homo = inter_facet_homogeneity$value, intra_facet_homo = intra_facet_homogeneity$value, decile_nobs_proxy = decile_nobs, percentile_nobs_proxy = percentile_nobs)

value_r
}
