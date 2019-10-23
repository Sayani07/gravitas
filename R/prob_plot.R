#' Plot probability distribution of univariate series across bivariate temporal granularities

#' @param .data a tsibble
#' @param gran1 the lower level granularity to be paired
#' @param gran2 the upper level granularity to be paired
#' @param hierarchy_tbl A hierarchy table specifying the hierarchy of units and their relationships
#' @param plot_type type of distribution plot
#' @param response response variable to be plotted
#' @param facet_h levels of facet variable for which facetting is allowed while plotting bivariate temporal granularities
#' @param quantile_prob numeric vector of probabilities with value in [0,1]  whose sample quantiles are wanted. Default is set to "Decile" plot
#' @param symmetric If TRUE, symmetic quantile area plot is drawn. If FALSE, only quantile lines are drawn instead of area. If TRUE, length of quantile_prob should be odd and ideally the quantile_prob should be a symmetric vector with median at the middle position.
#' @param alpha level of transperancy for the quantile area
#' @param ... other arguments to be passed for appropriate labels
#' @return a ggplot object
#
#' @examples
#' library(tsibbledata)
#' library(ggplot2)
#' library(tsibble)
#'
#' vic_elec %>% prob_plot(
#'   gran1 = "hour_day", gran2 = "day_week",
#'   response = "Demand", plot_type = "quantile",
#'   quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9),
#'   symmetric = TRUE, outlier.colour = "red", outlier.shape = 2, palette = "Dark2"
#' )
#'
#' cricket_tsibble <- cricket %>%
#'   mutate(data_index = row_number()) %>%
#'   as_tsibble(index = data_index)
#'
#' hierarchy_model <- tibble::tibble(
#'   units = c("index", "ball", "over", "inning", "match"),
#'   convert_fct = c(1, 6, 20, 2, 1)
#' )
#'
#' cricket_tsibble %>%
#'   prob_plot("inning_match", "over_inning",
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
    gran_advice <- gran_advice(.data,
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

  x_var <- dplyr::if_else(plot_type == "ridge", response, gran2)
  y_var <- dplyr::if_else(plot_type == "ridge", gran2, response)

  p <- data_mutate %>%
    as_tibble(.name_repair = "minimal") %>%
    ggplot2::ggplot(ggplot2::aes(x = data_mutate[[x_var]],
                                 y = data_mutate[[y_var]]
    )) +
    ggplot2::facet_wrap(~ data_mutate[[gran1]]) +
    ggplot2::ggtitle(paste0(plot_type,
                            " plot across ",
                            gran2, "on x-axis",
                            " given ",
                            gran1, "on facets"
    )) +
    xlab(gran2) + ylab(response) +
    scale_fill_brewer()

  if (plot_type == "boxplot") {
    plot <- p + ggplot2::geom_boxplot(...)
  }
  else if (plot_type == "violin") {
    plot <- p + ggplot2::geom_violin(...)
  }

  else if (plot_type == "lv") {
    plot <-
      p + geom_lv(aes(fill = ..LV..),
                  k = 5,
                  ...
      )
  }

  else if (plot_type == "ridge") {
    plot <- p +
      ggridges::geom_density_ridges(...)
  }

  else if (plot_type == "quantile") {
    plot <- quantile_plot(.data,
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

  plot_return <- plot +
    ggplot2::theme(
      legend.position = "bottom",
      strip.text = ggplot2::element_text(
        size = 7,
        margin = ggplot2::margin()
      )
    ) +
    ggplot2::ylab(y_var) +
    ggplot2::xlab(x_var) +
    ggplot2::ggtitle(paste0(
      plot_type,
      " plot across ",
      gran2,
      " given ",
      gran1
    ))

  gran_warn(.data,
            gran1,
            gran2,
            hierarchy_tbl = hierarchy_tbl,
            response = response,
            ...
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
                          ...) {
  p <- quantile_prob

  quantile_names <- purrr::map_chr(p, ~ paste0(.x * 100, "%"))

  quantile_funs <- purrr::map(p, ~ purrr::partial(quantile, probs = .x, na.rm = TRUE)) %>%
    rlang::set_names(nm = quantile_names)

  data_mutate <- .data %>%
    create_gran(gran1,
      hierarchy_tbl = hierarchy_tbl
    ) %>%
    create_gran(gran2,
      hierarchy_tbl = hierarchy_tbl
    )


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
    mid_pos <- p %>% match(x = median(p))

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


    plot <- eval(sum_expr(l)) +
      ggplot2::geom_line(aes(
        x = data_mutate_obj[[!!gran2]],
        y = data_mutate_obj[[quantile_names[mid_pos]]],
        group = data_mutate_obj[[!!gran1]]
      ),
      size = 1
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
        quantile,
        value,
        -c(
          `data_mutate[[gran1]]`,
          `data_mutate[[gran2]]`
        )
      ) %>%
      dplyr::select(
        !!rlang::quo_name(gran1) := `data_mutate[[gran1]]`,
        !!rlang::quo_name(gran2) := `data_mutate[[gran2]]`,
        quantile := quantile,
        value := value
      )



    plot <- data_pcntl %>%
      ggplot2::ggplot(aes(
        x = data_pcntl[[gran2]],
        y = value,
        group = as.factor(quantile),
        color = quantile
      )) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~ data_pcntl[[gran1]]) +
      ggplot2::scale_x_discrete(breaks = pretty(as.integer(unique(data_pcntl[[gran2]])))) +
      ggplot2::scale_fill_brewer()
  }
  return(plot)
}

## ----ribbon_function

ribbon_function <- function(i, ymin, ymax, x, gran1, gran2, group, color_set, alpha) {
  rlang::expr(
    ggplot2::geom_ribbon(aes(
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
