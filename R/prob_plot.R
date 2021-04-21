#' Plotting probability distributions across granularities
#'
#' Plot probability distribution of univariate series across bivariate temporal granularities.
#'
#' @param .data a tsibble
#' @param gran1 the granularity which is to be placed across facets. Can be column names if required granularity already exists in the tsibble. For example, a column with public holidays which needs to be treated as granularity, can be included here.
#' @param gran2 the granularity to be placed across x-axis. Can be column names if required granularity already exists in the tsibble.
#' @param hierarchy_tbl A hierarchy table specifying the hierarchy of units and their relationships.
#' @param plot_type type of distribution plot. Options include "boxplot", "lv" (letter-value), "quantile", "ridge" or "violin".
#' @param response response variable to be plotted.
#' @param facet_h levels of facet variable for which facetting is allowed while plotting bivariate temporal granularities.
#' @param quantile_prob numeric vector of probabilities with value in [0,1]  whose sample quantiles are wanted. Default is set to "decile" plot.
#' @param symmetric If TRUE, symmetic quantile area <- is drawn. If FALSE, only quantile lines are drawn instead of area. If TRUE, length of quantile_prob should be odd and ideally the quantile_prob should be a symmetric vector with median at the middle position.
#' @param alpha level of transperancy for the quantile area
#' @param threshold_nobs the minimum number of observations below which only points would be plotted
#' @param ... other arguments to be passed for customising the obtained ggplot object.
#' @return a ggplot object which can be customised as usual.
#
#' @examples
#' library(tsibbledata)
#' library(ggplot2)
#' library(tsibble)
#' library(lvplot)
#' library(dplyr)
#'
#' smart_meter10 %>%
#'   dplyr::filter(customer_id %in% c("10017936")) %>%
#'   prob_plot(
#'     gran1 = "day_week", gran2 = "hour_day",
#'     response = "general_supply_kwh", plot_type = "quantile",
#'     quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9),
#'     symmetric = TRUE,
#'     outlier.colour = "red",
#'     outlier.shape = 2, palette = "Dark2"
#'   ) + scale_y_continuous(breaks = c(0.01, 0.1, 1.00))
#'
#'
#' cricket_tsibble <- cricket %>%
#'   mutate(data_index = row_number()) %>%
#'   as_tsibble(index = data_index)
#'
#' hierarchy_model <- tibble::tibble(
#'   units = c("index", "over", "inning", "match"),
#'   convert_fct = c(1, 20, 2, 1)
#' )
#'
#' cricket_tsibble %>%
#'   prob_plot("inning", "over",
#'     hierarchy_tbl = hierarchy_model,
#'     response = "runs_per_over",
#'     plot_type = "lv"
#'   )
#' @export prob_plot

prob_plot <- function(.data,
                      gran1 = NULL,
                      gran2 = NULL,
                      hierarchy_tbl = NULL,
                      response = NULL,
                      plot_type = NULL,
                      quantile_prob = c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99),
                      facet_h = NULL,
                      symmetric = TRUE,
                      alpha = 0.8,
                      threshold_nobs = NULL,
                      # begin = 0,
                      # end = 1,
                      # direction = 1,
                      # palette = "YlGnBu",
                      # package = "RColorBrewer",
                      ...) {
  # data must be tsibble
  if (!tsibble::is_tsibble(.data)) {
    stop("must use tsibble")
  }

  if (is.null(gran1) | is.null(gran2)) {
    stop("Specify the granularities that are to be plotted")
  }

  if (is.null(response)) {
    response <- tsibble::measured_vars(.data)[1]
    message("The first measured variable
            plotted since no response variable specified")
  }

  if (is.null(plot_type)) {
    gran_advice <- gran_advice(
      .data,
      gran1,
      gran2,
      hierarchy_tbl,
      ...
    )
    plot_type <- gran_advice$plot_choices[1]
  }

  ## making data mutate and basic ggplot object without a geom

  data_mutate <- .data %>%
    create_gran(gran1,
      hierarchy_tbl = hierarchy_tbl
    ) %>%
    create_gran(gran2,
      hierarchy_tbl = hierarchy_tbl
    )
  # conditional ggplot2 (if number iof observation few then show scatter plot)

  gran_pair_obs <- data_mutate %>% gran_tbl(gran1, gran2, hierarchy_tbl)

  if (is.null(threshold_nobs)) {
    if (plot_type == "boxplot") {
      threshold_nobs <- 10
    }
    else if (plot_type == "quantile") {
      threshold_nobs <- max(10, length(quantile_prob))
    }
    else {
      threshold_nobs <- 30
    }
  }


  data_sub1 <- data_mutate %>%
    tibble::as_tibble(.name_repair = "minimal") %>%
    dplyr::left_join(gran_pair_obs) %>%
    dplyr::filter(nobs <= threshold_nobs)

  data_sub2 <- data_mutate %>%
    tibble::as_tibble(.name_repair = "minimal") %>%
    dplyr::left_join(gran_pair_obs) %>%
    dplyr::filter(nobs > threshold_nobs)

  x_var <- dplyr::if_else(plot_type == "ridge", response, gran2)
  y_var <- dplyr::if_else(plot_type == "ridge", gran2, response)

  # <- data_mutate %>%
  #  tibble::as_tibble(.name_repair = "minimal") %>%
  p <- data_mutate %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data[[x_var]],
      y = .data[[y_var]]
    ))

  if (nrow(data_sub1) == 0 & nrow(data_sub2) == 0) {
    plot <- p
  }

  if (nrow(data_sub1) != 0) {
    p <- p +
      ggplot2::geom_point(
        data = data_sub1,
        ggplot2::aes(
          x = .data[[x_var]],
          y = .data[[y_var]]
        ), alpha = 0.5, colour = "red", ...
      )
  }

  if (nrow(data_sub2) != 0) {
    if (plot_type == "boxplot") {
      p <- p +
        ggplot2::geom_boxplot(data = data_sub2, ...)
    }
    else if (plot_type == "violin") {
      p <- p + ggplot2::geom_violin(data = data_sub2, ...)
    }

    else if (plot_type == "lv") {
      p <-
        p + lvplot::geom_lv(
          data = data_sub2, ggplot2::aes(fill = ..LV..),
          k = 5,
          ...
        )
    }

    else if (plot_type == "ridge") {
      p <- p +
        ggridges::geom_density_ridges(data = data_sub2, ...)
    }

    else if (plot_type == "quantile") {
      p <- quantile_plot(
        .data = data_sub2,
        gran1,
        gran2,
        hierarchy_tbl,
        response,
        symmetric,
        quantile_prob,
        alpha,
        ...
      )
    }
  }

  # +
  # ggplot2::ggtitle(paste0(plot_type,
  #                         " plot across ",
  #                         gran2, "on x-axis",
  #                         " given ",
  #                         gran1, "on facets"
  # )) +
  # ggplot2::xlab(gran2) + ggplot2::ylab(response) +
  # ggplot2::scale_fill_brewer()


  plot_return <- p +
    ggplot2::facet_wrap(dplyr::vars(!!rlang::sym(gran1))) +
    ggplot2::theme(
      legend.position = "bottom",
      strip.text = ggplot2::element_text(size = 7, margin = ggplot2::margin())
    ) +
    ggplot2::ylab(y_var) +
    ggplot2::xlab(x_var) +
    ggplot2::ggtitle(paste0(
      plot_type,
      " plot across ",
      gran2,
      " given ",
      gran1
    )) +
    ggplot2::scale_fill_brewer(palette = "Dark2")

  gran_warn(.data,
    gran1,
    gran2,
    hierarchy_tbl = hierarchy_tbl,
    response = response, ...
  )

  return(plot_return)
}

## ----quantile_plot

quantile_plot <- function(.data,
                          gran1 = NULL,
                          gran2 = NULL,
                          hierarchy_tbl = NULL,
                          response = NULL,
                          symmetric = TRUE,
                          quantile_prob,
                          alpha = 0.8,
                          begin = 0,
                          end = 1,
                          direction = 1,
                          palette = "YlGnBu",
                          package = "RColorBrewer",
                          size = 1,
                          ...) {
  p <- quantile_prob

  quantile_names <- purrr::map_chr(p, ~ paste0(.x * 100, "%"))

  quantile_funs <- purrr::map(p, ~ purrr::partial(stats::quantile, probs = .x, na.rm = TRUE)) %>%
    rlang::set_names(nm = quantile_names)

  data_mutate <- .data


  data_mutate_obj <- data_mutate %>%
    tibble::as_tibble() %>%
    dplyr::group_by(
      data_mutate[[gran1]],
      data_mutate[[gran2]]
    ) %>%
    dplyr::summarize_at(
      response,
      quantile_funs
    ) %>%
    dplyr::rename(
      !!rlang::quo_name(gran1) := `data_mutate[[gran1]]`,
      !!rlang::quo_name(gran2) := `data_mutate[[gran2]]`
    )

  if (symmetric) {
    if (length(quantile_names) %% 2 == 0) {
      stop("Provide an odd number of probabilities for symmetric quantiles")
    }
    if (length(quantile_names) %% 2 != 0) {
      y_mid <- stats::median(quantile_names)
    }
    mid_pos <- p %>% match(x = stats::median(p))

    # how many colors needed
    #  color_l <- length(quantile_names) - 1
    # color_set <- palap::palap(color_l,
    #                           alpha = alpha,
    #                           begin = begin,
    #                           end = end,
    #                           direction = direction,
    #                           palette = palette,
    #                           package = package)



    # color_set <- paletteer_d(package = "RColorBrewer", palette = "YlGnBu", n = color_l)


    color_l <- ceiling(length(quantile_names) / 2)
    color_set <- RColorBrewer::brewer.pal(color_l, "Dark2")

    ymin <- array()
    ymax <- array()

    for (i in 1:(mid_pos - 1))
    {
      ymin[i] <- quantile_names[i]
      ymax[mid_pos - i] <- quantile_names[length(p) - mid_pos + i + 1]
    }

    l <- purrr::map(
      1:(mid_pos - 1),
      ~ ribbon_function(
        ., ymin, ymax, x,
        gran1, gran2, group, color_set, alpha
      )
    )

    ggplot2::ggplot(data = data_mutate_obj) +
      ggplot2::geom_ribbon(ggplot2::aes(
        ymin = data_mutate_obj[[ymin]],
        ymax = data_mutate_obj[[ymax]],
        x = data_mutate_obj[[gran2]],
        group = data_mutate_obj[[gran1]],
        colour = color_set
      ), ,
      fill = color_set,
      alpha = alpha
      )


    plot <- eval(sum_expr(l)) +
      ggplot2::geom_line(ggplot2::aes(
        x = data_mutate_obj[[!!gran2]],
        y = data_mutate_obj[[quantile_names[mid_pos]]],
        group = data_mutate_obj[[!!gran1]]
      ),
      size
      ) +
      ggplot2::facet_wrap(~ data_mutate_obj[[gran1]]) +
      ggplot2::scale_x_discrete(breaks = pretty(as.integer(unique(data_mutate_obj[[gran2]]))))
  }

  else {
    data_pcntl <- data_mutate %>%
      tibble::as_tibble() %>%
      dplyr::group_by(
        data_mutate[[gran1]],
        data_mutate[[gran2]]
      ) %>%
      dplyr::summarize_at(
        response,
        quantile_funs
      ) %>%
      tidyr::gather(
        quantiles,
        value,
        -c(
          `data_mutate[[gran1]]`,
          `data_mutate[[gran2]]`
        )
      ) %>%
      dplyr::select(
        !!rlang::quo_name(gran1) := `data_mutate[[gran1]]`,
        !!rlang::quo_name(gran2) := `data_mutate[[gran2]]`,
        quantiles := quantiles,
        value := value
      )

    plot <- data_pcntl %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(
        x = data_pcntl[[gran2]],
        y = value,
        group = as.factor(quantiles),
        color = quantiles
      )) +
      ggplot2::facet_wrap(~ data_pcntl[[gran1]]) +
      # ggplot2::scale_x_discrete(breaks = pretty(as.integer(unique(data_pcntl[[gran2]])))) +
      ggplot2::scale_fill_brewer()
  }
  return(plot)
}

## ----ribbon_function

ribbon_function <- function(i, ymin, ymax, x,
                            gran1, gran2, group,
                            color_set, alpha) {
  rlang::expr(
    ggplot2::geom_ribbon(ggplot2::aes(
      ymin = data_mutate_obj[[!!ymin[i]]],
      ymax = data_mutate_obj[[!!ymax[i]]],
      x = data_mutate_obj[[!!gran2]],
      group = data_mutate_obj[[!!gran1]]
    ),
    colour = !!color_set[i],
    fill = !!color_set[i],
    alpha = !!alpha
    )
  )
}

## ----sum_exp

sum_expr <- function(v = NULL) {
  if (length(v) == 1) {
    count <- rlang::expr(ggplot2::ggplot(data_mutate_obj) + !!v[[1]])
  }
  else {
    count <- rlang::expr(ggplot2::ggplot(data_mutate_obj) + !!v[[1]])
    for (i in 2:length(v))
    {
      count <- rlang::expr(!!count + !!v[[i]])
    }
  }

  return(count)
}
