#' Plot probability distribution of univariate series across bivariate temporal granularities

#' @param .data a tsibble
#' @param gran1 the lower level granularity to be paired
#' @param gran2 the upper level granularity to be paired
#' @param hierarchy_tbl A hierarchy table specifying the hierarchy of units and their relationships
#' @param plot_type type of distribution plot
#' @param response response variable to be plotted
#' @param facet_h levels of facet variable for which facetting is allowed while plotting bivariate temporal granularities
#' @param quantile_prob numeric vector of probabilities with value in [0,1]  whose sample quantiles are wanted. Default is set to "Decile" plot
#' @param overlay If FALSE, only lines are drawn for quantiles. If TRUE, area is drawn instead of lines
#' @param alpha level of transperancy for the quantile area
#' @param ... other arguments to be passed for appropriate labels
#' @return a ggplot object
#
#' @examples
#' library(gravitas)
#' library(tsibbledata)
#' library(dplyr)
#' library(tsibble)
#' library(ggplot2)
#' vic_elec %>% granplot("hour_day", "day_week", response = "Demand", plot_type = "boxplot")
#'
#' cricket_tsibble <- cricketdata %>%
#'   mutate(data_index = row_number()) %>%
#'   as_tsibble(index = data_index)
#'
#' hierarchy_model <- tibble::tibble(
#'   units = c("index", "ball", "over", "inning", "match"),
#'   convert_fct = c(1, 6, 20, 2, 1)
#' )
#'
#' cricket_tsibble %>%
#'   granplot("inning_match", "ball_inning",
#'     hierarchy_model,
#'     response = "total_runs",
#'     plot_type = "quantile",
#'     quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9))
#'
#'
#' granplot(
#'   .data = tsibbledata::vic_elec,
#'   gran1 = "day_week",
#'   gran2 = "hour_day",
#'   response = "Demand",
#'   plot_type = "quantile",
#'   quantile_prob = c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99),
#'   overlay = FALSE
#' )
#' @export granplot
# Recommendation plot function for two granularities
granplot <- function(.data,
                     gran1 = NULL,
                     gran2 = NULL,
                     hierarchy_tbl = NULL,
                     response = NULL,
                     plot_type = NULL,
                     quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9),
                     facet_h = NULL,
                     overlay = TRUE,
                     alpha = 0.8, ...) {
  if (is.null(facet_h)) {
    facet_h <- 31
  }


  if (is.null(response)) {
    response <- tsibble::measured_vars(.data)[1]
    message("The first measured variable
            plotted since no response variable specified")
  }
  # Warn if they have chosen clashes asking to look at the table of harmonies

  proxy_harmony <- is.harmony(.data,
                              gran1,
                              gran2,
                              hierarchy_tbl,
                              response = NULL,
                              facet_h, ...)

  if (proxy_harmony == "FALSE") {
    warning("Granularities chosen are Clashes.
            \nYou might be interested to look at the
            set of harmonies in Harmony table.")
  }

  proxy_homogenous <- is.homogenous(.data,
                                    gran1,
                                    gran2,
                                    hierarchy_tbl,
                                    response = NULL, ...)

  # get recommended plots list
  advice <- gran_advice(.data,
                        gran1,
                        gran2,
                        hierarchy_tbl,
                        response, ...)
  if (is.null(plot_type)) {
    plot_type <- advice[1]
  }

  data_count <- gran_tbl(.data, gran1, gran2, hierarchy_tbl, response, ...)

  gran1_level <- data_count %>% dplyr::select(!!rlang::quo_name(gran1)) %>% dplyr::distinct() %>% nrow()
  gran2_level <- data_count %>% dplyr::select(!!rlang::quo_name(gran2)) %>% dplyr::distinct() %>% nrow()

  # Facetting not recommended for so many levels

  if (gran1_level > facet_h & gran2_level > facet_h) {
    warning(paste("Facetting not recommended:
                  too many categories in ",
                  gran1,
                  "and", gran2))
  }
  else if (gran1_level > facet_h & gran2_level <= facet_h) {
    warning(paste("Facetting not recommended:
                  too many categories in ",
                  gran1, ". Try using",
                  gran2, "as the facet variable."))
  }



  data_mutate <- .data %>%
    create_gran(gran1,
                hierarchy_tbl = hierarchy_tbl, ...) %>%
    create_gran(gran2,
                  hierarchy_tbl = hierarchy_tbl, ...)


  p <- data_mutate %>%
    as_tibble(.name_repair = "minimal") %>%
    ggplot2::ggplot(ggplot2::aes(x = data_mutate[[gran2]],
                                 y = data_mutate[[response]])) +
    ggplot2::facet_wrap(~ data_mutate[[gran1]]) +
    ggplot2::ggtitle(paste0(plot_type,
                            " plot across ",
                            gran2,
                            " given ",
                            gran1)) +
    xlab(gran2) + ylab(response) +
    scale_fill_brewer()

  if (plot_type == "boxplot") {
    plot <- p + ggplot2::geom_boxplot(...)
  }
  else if (plot_type == "violin") {
    plot <- p + ggplot2::geom_violin(...)
  }
  #
  #   else if (plot_type == "density") {
  #     plot <- p + ggplot2::geom_density(position = "stack",...)
  #   }

  else if (plot_type == "lv") {
    plot <-
      p + geom_lv(aes(fill = ..LV..),
                  outlier.colour = "red",
                  outlier.shape = 1,
                  k = 5)
  }
  else if (plot_type == "ridge") {
    plot <- data_mutate %>%
      ggplot2::ggplot(aes(x = data_mutate[[response]],
                          y = data_mutate[[gran2]],
                          group = data_mutate[[gran2]])) +
      ggridges::geom_density_ridges() +
      ggplot2::facet_wrap(~ data_mutate[[gran1]]) +
      ggplot2::xlab(response) +
      ggplot2::ylab(data_mutate[[gran2]]) +
      ggplot2::ggtitle(paste0(plot_type,
                              " plot across ",
                              gran2,
                              " given ",
                              gran1))
  }
  else if (plot_type == "decile") {
    d <- seq(0.1, 0.9, by = 0.1)
    decile_names <- purrr::map_chr(d, ~ paste0(.x * 100, "%"))

    decile_funs <- purrr::map(d,
                              ~ purrr::partial(quantile,
                                               probs = .x,
                                               na.rm = TRUE)) %>%
      rlang::set_names(nm = decile_names)


    data_dec <- data_mutate %>%
      as_tibble() %>%
      dplyr::group_by(data_mutate[[gran1]],
                      data_mutate[[gran2]]) %>%
      dplyr::summarize_at(response, decile_funs) %>%
      tidyr::gather(quantile, value,
                    -c(`data_mutate[[gran1]]`,
                       `data_mutate[[gran2]]`)) %>%
      dplyr::select(
        !!rlang::quo_name(gran1) := `data_mutate[[gran1]]`,
        !!rlang::quo_name(gran2) := `data_mutate[[gran2]]`,
        quantile := quantile,
        value := value
      )


    plot <- data_dec %>%
      ggplot2::ggplot(aes(x = data_dec[[gran2]], y = value, group = quantile, color = quantile)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~ data_dec[[gran1]]) +
      ggplot2::ylab(response) +
      ggplot2::xlab(gran2) +
      ggplot2::ggtitle(paste0(plot_type,
                              " plot across ",
                              gran2, " given ",
                              gran1)) +
      ggplot2::scale_x_discrete(breaks = pretty(as.integer(unique(data_dec[[gran2]]))))
    # prettify needs to work correctly

    if (proxy_homogenous$decile_nobs_proxy != 0) {
      warning("Decile plot not recommended as number of observations too few for one or more combinations")
    }
  }
  else if (plot_type == "quantile") {
    p <- quantile_prob

    quantile_names <- purrr::map_chr(p, ~ paste0(.x * 100, "%"))

    quantile_funs <- purrr::map(p, ~ purrr::partial(quantile, probs = .x, na.rm = TRUE)) %>%
      rlang::set_names(nm = quantile_names)

    if (overlay) {
      data_mutate <- .data %>%
        create_gran(gran1,
                            hierarchy_tbl = hierarchy_tbl
        ) %>%
        create_gran(gran2,
                            hierarchy_tbl = hierarchy_tbl
        )


      data_mutate_obj <- data_mutate %>%
        tibble::as_tibble() %>%
        dplyr::group_by(data_mutate[[gran1]], data_mutate[[gran2]]) %>%
        dplyr::summarize_at(response, quantile_funs) %>%
        dplyr::rename(
          !!rlang::quo_name(gran1) := `data_mutate[[gran1]]`,
          !!rlang::quo_name(gran2) := `data_mutate[[gran2]]`
        )

      if (length(quantile_names) %% 2 == 0) {
        stop("Provide an odd number of probabilities")
      }
      if (length(quantile_names) %% 2 != 0) {
        y_mid <- stats::median(quantile_names)
      }
      mid_pos <- p %>% match(x = median(p))

      # how many colors needed
      color_l <- ceiling(length(quantile_names) / 2)
      color_set <- RColorBrewer::brewer.pal(color_l, "Dark2")

      ymin <- array()
      ymax <- array()

      for (i in 1:(mid_pos - 1))
      {
        ymin[i] <- quantile_names[i]
        ymax[mid_pos - i] <- quantile_names[length(p) - mid_pos + i + 1]
      }


      l <- purrr::map(1:(mid_pos - 1), ~ ribbon_function(., ymin, ymax, x, gran1, gran2, group, color_set, alpha))


      plot <- eval(sum_expr(l)) + ggplot2::geom_line(aes(
        x = data_mutate_obj[[!!gran2]], y = data_mutate_obj[[quantile_names[mid_pos]]],
        group = data_mutate_obj[[!!gran1]]
      ),
      size = 1
      ) +
        ggplot2::facet_wrap(~ data_mutate_obj[[gran1]]) +
        ggplot2::scale_x_discrete(breaks = pretty(as.integer(unique(data_mutate_obj[[gran2]]))))
    }
    else {

      # no overlay - simple line graphs for quantiles

      data_pcntl <- data_mutate %>%
        tibble::as_tibble() %>%
        dplyr::group_by(data_mutate[[gran1]], data_mutate[[gran2]]) %>%
        dplyr::summarize_at(response, quantile_funs) %>%
        tidyr::gather(quantile, value, -c(`data_mutate[[gran1]]`, `data_mutate[[gran2]]`)) %>%
        dplyr::select(
          !!rlang::quo_name(gran1) := `data_mutate[[gran1]]`,
          !!rlang::quo_name(gran2) := `data_mutate[[gran2]]`,
          quantile := quantile,
          value := value
        )



      plot <- data_pcntl %>%
        ggplot2::ggplot(aes(x = data_pcntl[[gran2]], y = value, group = as.factor(quantile), color = quantile)) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(~ data_pcntl[[gran1]]) +
        ggplot2::scale_x_discrete(breaks = pretty(as.integer(unique(data_pcntl[[gran2]]))))
    }

    plot <- plot +
      ggplot2::ylab(response) +
      ggplot2::xlab(gran2) +
      ggplot2::ggtitle(paste0(plot_type, " plot across ", gran2, " given ", gran1)) +
      scale_fill_brewer(palette = "Dark2")

    if (proxy_homogenous$percentile_nobs_proxy != 0) {
      warning("Percentile plot not recommended as number of observations too few for one or more combinations")
    }
  }
  plot + ggplot2::theme(legend.position = "bottom",
                        strip.text = ggplot2::element_text(size = 7,
                                                           margin = ggplot2::margin()))
}

# advise function for which plots to choose depending on levels of facets and x-axis
# gran_advice(.data, gran1="hour_week", gran2 = "day_month")
gran_advice <- function(.data,
                        gran1,
                        gran2,
                        hierarchy_tbl,
                        response = NULL, ...) {
  if (!tsibble::is_tsibble(.data)) {
    stop("must use tsibble")
  }

  proxy_harmony <- is.harmony(.data,
                              gran1,
                              gran2,
                              hierarchy_tbl,
                              response = NULL, ...)
  proxy_homogenous <- is.homogenous(.data,
                                    gran1,
                                    gran2,
                                    hierarchy_tbl, response = NULL, ...)


  data_count <- gran_tbl(.data,
                         gran1,
                         gran2,
                         hierarchy_tbl,
                         response, ...)


  gran1_level <- data_count %>%
    dplyr::select(!!rlang::quo_name(gran1))
  %>% dplyr::distinct() %>% nrow()

  gran2_level <- data_count %>%
    dplyr::select(!!rlang::quo_name(gran2))
  %>% dplyr::distinct() %>% nrow()

  facet_h <- 31
  facet_m <- 14
  facet_l <- 7

  x_h <- 31
  x_m <- 14
  x_l <- 7
  # very high facet levels
  if (gran1_level > facet_h) {
    plots_list <- c("decile", "percentile")
    # warning(paste("Facetting not recommended: too many categories in ", gran1))
  } # high facet levels
  if (dplyr::between(gran1_level, facet_m, facet_h) & gran2_level > x_h) # (high, very high)
  {
    plots_list <- c("decile", "percentile")
  }

  else if (dplyr::between(gran1_level, facet_m, facet_h) & dplyr::between(gran2_level, x_m, x_h)) # (high, high)
  {
    plots_list <- c("decile", "percentile")
  }

  else if (dplyr::between(gran1_level, facet_m, facet_h) & dplyr::between(gran2_level, x_l, x_m)) # (high, medium)
  {
    plots_list <- c("decile", "percentile")
  }
  else if (dplyr::between(gran1_level, facet_m, facet_h) & gran2_level < x_l) # (high, low)
  {
    plots_list <- c("ridge", "violin", "lv", "density")
  }
  # medium facet levels
  else if (dplyr::between(gran1_level, facet_l, facet_m) & gran2_level > x_h) # (medium, very high)
  {
    plots_list <- c("decile", "percentile")
  }
  else if (dplyr::between(gran1_level, facet_l, facet_m) & dplyr::between(gran2_level, x_m, x_h)) # (medium, high)
  {
    plots_list <- c("decile", "percentile")
  }

  else if (dplyr::between(gran1_level, facet_l, facet_m) & dplyr::between(gran2_level, x_l, x_m)) # (medium, medium)
  {
    plots_list <- c("decile", "percentile")
  }
  else if (dplyr::between(gran1_level, facet_l, facet_m) & gran2_level < x_l) # (medium, low)
  {
    plots_list <- c("ridge", "violin", "lv", "density")
  }
  # low facet levels
  else if (gran1_level < facet_l & gran2_level > x_h) # (low, very high)
  {
    plots_list <- c("decile", "percentile")
  }
  else if (gran1_level < facet_l & dplyr::between(gran2_level, x_m, x_h)) # (low, high)
  {
    plots_list <- c("boxplot", "lv", "percentile", "decile")
  }

  else if (gran1_level < facet_l & dplyr::between(gran2_level, x_l, x_m)) # (low, medium)
  {
    plots_list <- c("ridge", "violin", "lv", "density", "percentile", "decile")
  }
  else if (gran1_level < facet_l & gran2_level < x_l) # (low, low)
  {
    plots_list <- c("ridge", "violin", "lv", "density", "percentile", "decile")
  }

  # if( c("percentile" %in% plots_list & proxy_homogenous$percentile_nobs_proxy !=0))
  # {
  #   plots_list <- plots_list[-which(plots_list=="percentile")]
  # }
  #
  # if( c("decile" %in% plots_list & proxy_homogenous$decile_nobs_proxy !=0))
  # {
  #   plots_list <- plots_list[-which(plots_list=="decile")]
  # }

  plots_list
}



is.homogenous <- function(.data,
                          gran1,
                          gran2,
                          hierarchy_tbl = NULL,
                          response = NULL, ...) {
  if (!tsibble::is_tsibble(.data)) {
    stop("must use tsibble")
  }

  # inter facet homogeneity

  data_count <- gran_tbl(.data,
                         gran1,
                         gran2,
                         hierarchy_tbl,
                         response, ...)

  inter_facet_homogeneity <- data_count %>%
    dplyr::group_by(!!rlang::quo_name(gran1)) %>%
    dplyr::summarise(min_c = min(nobs),
                     max_c = max(nobs)) %>%
    dplyr::summarise(sum = sum(dplyr::if_else(min_c == max_c, 0, 1))) %>% dplyr::mutate(value = dplyr::if_else(sum == 0, "TRUE", "FALSE"))

  # intra facet homogeneity
  intra_facet_homogeneity <- data_count %>%
    dplyr::group_by(!!rlang::quo_name(gran2)) %>%
    dplyr::summarise(min_c = min(nobs), max_c = max(nobs)) %>%
    dplyr::summarise(sum = sum(dplyr::if_else(min_c == max_c, 0, 1))) %>% dplyr::mutate(value = dplyr::if_else(sum == 0, "TRUE", "FALSE"))

  decile_nobs <- sum(dplyr::if_else(data_count$nobs < 30, 1, 0))
  percentile_nobs <- sum(dplyr::if_else(data_count$nobs < 100, 1, 0))

  value_r <- tibble(inter_facet_homo = inter_facet_homogeneity$value, intra_facet_homo = intra_facet_homogeneity$value, decile_nobs_proxy = decile_nobs, percentile_nobs_proxy = percentile_nobs)

  value_r
}


ribbon_function <-   function(i, ymin, ymax, x, gran1, gran2,  group, color_set, alpha)
{

  rlang::expr(
    ggplot2::geom_ribbon(aes(ymin = data_mutate_obj[[!!ymin[i]]],
                    ymax =  data_mutate_obj[[!!ymax[i]]],
                    x = data_mutate_obj[[!!gran2]],
                    group = data_mutate_obj[[!!gran1]]),
                colour= !!color_set[i],
                fill = !!color_set[i],
                alpha = !!alpha))
}



sum_expr <-    function(v = NULL)
{

  if(length(v)==1)
  {
    count <-  rlang::expr(ggplot2::ggplot(data_mutate_obj) + !!v[[1]])
  }
  else
  {
    count <-  rlang::expr(ggplot2::ggplot(data_mutate_obj) + !!v[[1]])
    for(i in 2:length(v))
    {
      count <-  rlang::expr(!!count + !!v[[i]] )
    }
  }

  return(count)
}



