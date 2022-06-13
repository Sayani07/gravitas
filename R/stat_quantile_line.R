#' @title Stat quantiles for line or area
#' @description calculate quantiles of line or area quantiles
#' @param geom PARAM_DESCRIPTION, Default: 'line'
#' @param position PARAM_DESCRIPTION, Default: 'dodge2'
#' @param mapping Default: NULL
#' @param data Default: NULL
#' @param ... ...
#' @param coef Default: 1.5
#' @param na.rm Default: FALSE
#' @param show.legend Default: NA
#' @param inherit.aes Default: TRUE
#' @rdname stat_quantile_line
#' @return A [ggplot2::Stat] representing the data transformations with required
#' mappings for plotting HDRs using [geom_hdr_boxplot()] and [geom_hdr_rug()].
#' @example
#' data <- smart_meter10
#' gran1 <- "hour_day"
#' gran2 <- "day_week"
#' y <- "general_supply_kwh"
#' w = create_quantiles(data, gran1, gran2, y,  quantile_prob = c(0.25, 0.5, 0.75))
#' ggplot(w) + geom_area(aes(x = hour_day, y = y, group = quantile),
#' colour = "blue", fill = "grey", alpha = 1) + facet_wrap(~day_week)

#' @export
stat_quantile_line <- function(mapping = NULL, data = NULL,
                     geom = "quantile", position = "dodge2",
                     ...,
                     coef = 1.5,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatQuantileLine,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      coef = coef,
      ...
    )
  )
}

#' @title stat_hdr
#' @description stat for hdr
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 Stat
#' @rdname StatHdr
#' @export
#' @keywords internal
StatQuantileLine <- ggproto("StatQuantileLine", Stat,
                   optional_aes = c("gran1", "y"),
                   # non_missing_aes = "weight",

                   # setup_params = ggplot2::StatBoxplot$setup_params,

                   setup_data = function(data, params) {
                     # How are missing values handled?
                     data
                   },

                   compute_group = function(data, scales, width = NULL, probs = NULL,
                                            na.rm = FALSE) {
                     # initialise 1 row data.frame
                     df <- structure(list(),
                                     .Names = character(0), row.names = c(NA, -1L),
                                     class = "data.frame"
                     )

                     df <- create_quantiles(data, gran1, gran2,  y,  quantile_prob = c(0.25, 0.5, 0.75))
                     df$width <- width
                     df


                   }
)

#' @title create_quantiles
#' @description compute  quantiles for each category
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 Stat
#' @rdname StatHdr
#' @export
#' @keywords internal
#' @example
#' data <- smart_meter10
#' x <- "hour_day"
#' y <- "general_supply_kwh"
#' result <- create_quantiles(data, x, y,  quantile_prob = c(0.25, 0.5, 0.75))

create_quantiles <- function(data,
                             gran1 = NULL,
                             gran2 = NULL,
                             y,
                             quantile_prob = c(0.25, 0.5, 0.75)){

  if (any(quantile_prob<0|quantile_prob>1))
    rlang::abort("`quantile_prob` must be between 0 and 1")


  if (is.null(gran1))
    rlang::abort("`specify at least one granularity")


####---------------if both granularities are specified

  if(!is.null(gran2))
  {
  # create the granularity and split the data by granularity
  result <- data %>%
    create_gran(gran1)%>%
    create_gran(gran2)%>%
    tibble::as_tibble() %>%
    select(!!rlang::sym(gran1),!!rlang::sym(gran2), !!rlang::sym(y)) %>%
    group_split(!!rlang::sym(gran1), !!rlang::sym(gran2))

  # create quantiles for each level of the granularity
  quantiles <- map(seq_len(length(result)), function(i) {

    result_i <- result %>%
      magrittr::extract2(i)


    result_i %>%
      magrittr::extract2(y) %>%
      stats::quantile(probs = quantile_prob)

  }) %>% dplyr::bind_rows()

  # adding the name of the level and transforming to long form for geom to use

  output <- dplyr::bind_cols(result %>% bind_rows() %>%
                               distinct(!!rlang::sym(gran1), !!rlang::sym(gran2)),
                             quantiles) %>%
    tidyr::pivot_longer(-c(1, 2), names_to = "quantile",
                 values_to = "y")
  }


####---------------no second granularity

  if(is.null(gran2)){
    result <- data %>%
      create_gran(gran1 = gran1)%>%
      tibble::as_tibble() %>%
      select(!!rlang::sym(x), !!rlang::sym(y)) %>%
      group_split(!!rlang::sym(x))

    # create quantiles for each level of the granularity
    quantiles <- map(seq_len(length(result)), function(i) {

      result_i <- result %>%
        magrittr::extract2(i)


      result_i %>%
        magrittr::extract2(y) %>%
        stats::quantile(probs = quantile_prob)

    }) %>% dplyr::bind_rows()

    output <- dplyr::bind_cols(result %>% bind_rows() %>%
                                 distinct(!!rlang::sym(gran1)),
                               quantiles) %>%
      tidyr::pivot_longer(-c(1), names_to = "quantile",
                          values_to = "y")
  }


  # output <- output %>%
  #   mutate(group = !!rlang::sym(gran1))
  output
}


# data <- smart_meter10
# x <- "hour_day"
# y <- "general_supply_kwh"
#
# result <- data %>%
#   create_gran(gran1 = x)%>%
#   tibble::as_tibble() %>%
#   select(!!rlang::sym(x), !!rlang::sym(y)) %>%
#   group_split(!!rlang::sym(x)) %>%
#   bind_rows()
#
# p <- ggplot(result, aes(hour_day, general_supply_kwh)) +
#   stat_summary(geom = 'point', fun = quantile, fun.args = list(probs = c(0.25, 0.5, 0.75))

