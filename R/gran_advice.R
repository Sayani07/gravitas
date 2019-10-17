#' Recommendations on plot choices and summary of interaction, number of observations and intra or inter facet homogeneity

#' @param .data a tsibble
#' @param gran1,gran2 granularities
#' @param hierarchy_tbl A hierarchy table specifying the hierarchy of units and their relationships
#' @param ... other arguments to be passed for appropriate labels
#' @return a list of summary check points before visualizing distribution across bivariate granularities
#
#' @examples
#' library(tsibbledata)
#' vic_elec %>% gran_advice(
#'   gran1 = "hour_day",
#'   gran2 = "day_month"
#' )
#'
#' @export
gran_advice <- function(.data,
                        gran1,
                        gran2,
                        hierarchy_tbl = NULL,
                        ...) {

  # data must be tsibble
  if (!tsibble::is_tsibble(.data)) {
    stop("must use tsibble")
  }


  if (is.null(gran1) | is.null(gran2)) {
    stop("Specify the granularities that are to be plotted")
  }

  # checking if input data is harmony
  harmony <- is_harmony(
    .data,
    gran1,
    gran2,
    hierarchy_tbl
  )


  homogenous <- is_homogenous(
    .data,
    gran1,
    gran2,
    hierarchy_tbl
  )


  plot_choices <- plot_choices(.data,
    gran1,
    gran2,
    hierarchy_tbl,
    facet_h = 31,
    facet_m = 14,
    facet_l = 7,
    x_h = 31,
    x_m = 14,
    x_l = 7,
    ...
  )


  gran_obs <- gran_obs(
    .data,
    gran1,
    gran2,
    hierarchy_tbl,
    ...
  )

  return(list(harmony = harmony, homogenous = homogenous, plot_choices = plot_choices, gran_obs = gran_obs))
}


## ----gran_warn


gran_warn <- function(.data,
                      gran1,
                      gran2,
                      hierarchy_tbl = NULL,
                      facet_h = NULL, ...) {
  gran_advice <- gran_advice(
    .data,
    gran1,
    gran2,
    hierarchy_tbl
  )

  gran_tbl <- gran_tbl(
    .data,
    gran1,
    gran2,
    hierarchy_tbl
  )

  gran1_level <- gran_tbl %>%
    dplyr::select(!!rlang::quo_name(gran1)) %>%
    dplyr::distinct() %>%
    nrow()

  gran2_level <- gran_tbl %>%
    dplyr::select(!!rlang::quo_name(gran2)) %>%
    dplyr::distinct() %>%
    nrow()

  if (is.null(facet_h)) {
    facet_h <- 31
  }


  if (gran_advice$harmony == "FALSE") {
    warning("Granularities chosen are Clashes.
            \nYou might be interested to look at the
            set of harmonies using harmony(data).")
  }

  else {
    if (any(gran_tbl$nobs < 30)) {
      warning("Some combinations of granularities have less than 30 observations.
            Check gran_obs() to find combinations which have low observations.
            Analyze the distribution of these combinations with caution.")
    }


    # Facetting not recommended for so many levels

    if (gran1_level > facet_h & gran2_level > facet_h) {
      warning(paste(
        "Facetting not recommended:
                  too many categories in ",
        gran1,
        "and", gran2
      ))
    }
    else if (gran1_level > facet_h & gran2_level <= facet_h) {
      warning(paste(
        "Facetting not recommended:
                  too many categories in ",
        gran1, ". Try using",
        gran2, "as the facet variable."
      ))
    }

    if (gran_advice$homogenous$intra_facet == "FALSE" | gran_advice$homogenous$inter_facet == "FALSE") {
      warning("Number of observations for few combinations of
     granularities vary within or across facets.
     Use gran_obs() to find combinations which have low
            observations or very different number of observations.")
    }
  }
}






## ----plot_choices

plot_choices <- function(.data,
                         gran1,
                         gran2,
                         hierarchy_tbl = NULL,
                         facet_h = 31,
                         facet_m = 14,
                         facet_l = 7,
                         x_h = 31,
                         x_m = 14,
                         x_l = 7) {
  data_count <- gran_tbl(
    .data,
    gran1,
    gran2,
    hierarchy_tbl
  )


  gran1_level <- data_count %>%
    dplyr::select(!!rlang::quo_name(gran1)) %>%
    dplyr::distinct() %>%
    nrow()

  gran2_level <- data_count %>%
    dplyr::select(!!rlang::quo_name(gran2)) %>%
    dplyr::distinct() %>%
    nrow()


  # very high facet levels
  ## except when the number of levels across x-axis is low
  ## only quantile plots are suggested


  if (gran1_level > facet_h) {
    if (gran2_level < x_l) {
      plots_list <- c("ridge", "violin", "lv", "quantile", "boxplot")
    }
    else {
      plots_list <- c("quantile")
    }
  }

  if (dplyr::between(gran1_level, facet_m, facet_h) &
    gran2_level > x_h) # (high, very high)
  {
    plots_list <- c("quantile")
  }

  else if (dplyr::between(gran1_level, facet_m, facet_h) &
    dplyr::between(gran2_level, x_m, x_h)) # (high, high)
  {
    plots_list <- c("quantile")
  }

  else if (dplyr::between(gran1_level, facet_m, facet_h) &
    dplyr::between(gran2_level, x_l, x_m)) # (high, medium)
  {
    plots_list <- c("quantile")
  }
  else if (dplyr::between(gran1_level, facet_m, facet_h) &
    gran2_level < x_l) # (high, low)
  {
    plots_list <- c("ridge", "violin", "lv", "quantile", "boxplot")
  }


  # medium facet levels
  # for high and very high levels in x-axis quantiles are suggested
  # for medium and low levels in x-axis, all other plots might be drawn
  # for medium, ridge is removed from list

  else if (dplyr::between(gran1_level, facet_l, facet_m) &
    gran2_level > x_h) # (medium, very high)
  {
    plots_list <- c("quantile")
  }
  else if (dplyr::between(gran1_level, facet_l, facet_m) &
    dplyr::between(gran2_level, x_m, x_h)) # (medium, high)
  {
    plots_list <- c("quantile")
  }

  else if (dplyr::between(gran1_level, facet_l, facet_m) &
    dplyr::between(gran2_level, x_l, x_m)) # (medium, medium)
  {
    plots_list <- c("violin", "lv", "quantile", "boxplot")
  }
  else if (dplyr::between(gran1_level, facet_l, facet_m) &
    gran2_level < x_l) # (medium, low)
  {
    plots_list <- c("ridge", "violin", "lv", "quantile", "boxplot")
  }

  # low facet levels
  ## for very high, only quantiles are suggested
  ## for  high and medium, all except ridge plots suggested
  ## for low, all plots can be drawn

  else if (gran1_level < facet_l &
    gran2_level > x_h) # (low, very high)
  {
    plots_list <- c("quantile")
  }
  else if (gran1_level < facet_l &
    dplyr::between(gran2_level, x_m, x_h)) # (low, high)
  {
    plots_list <- c("violin", "lv", "quantile", "boxplot")
  }

  else if (gran1_level < facet_l &
    dplyr::between(gran2_level, x_l, x_m)) # (low, medium)
  {
    plots_list <- c("violin", "lv", "quantile", "boxplot")
  }
  else if (gran1_level < facet_l & gran2_level < x_l) # (low, low)
  {
    plots_list <- c("ridge", "violin", "lv", "quantile")
  }

  plots_list
}

## ----is_homogenous

is_homogenous <- function(.data,
                          gran1,
                          gran2,
                          hierarchy_tbl = NULL) {

  # inter facet homogeneity

  data_count <- gran_tbl(
    .data,
    gran1,
    gran2,
    hierarchy_tbl
  )

  inter_facet_homogeneity <- data_count %>%
    dplyr::group_by(!!rlang::quo_name(gran1)) %>%
    dplyr::summarise(
      min_c = min(nobs),
      max_c = max(nobs)
    ) %>%
    dplyr::summarise(sum = sum(dplyr::if_else(min_c == max_c, 0, 1))) %>%
    dplyr::mutate(value = dplyr::if_else(sum == 0, "TRUE", "FALSE"))

  # intra facet homogeneity
  intra_facet_homogeneity <- data_count %>%
    dplyr::group_by(!!rlang::quo_name(gran2)) %>%
    dplyr::summarise(min_c = min(nobs), max_c = max(nobs)) %>%
    dplyr::summarise(sum = sum(dplyr::if_else(min_c == max_c, 0, 1))) %>%
    dplyr::mutate(value = dplyr::if_else(sum == 0, "TRUE", "FALSE"))


  value_r <- tibble(inter_facet = inter_facet_homogeneity$value, intra_facet = intra_facet_homogeneity$value)

  value_r
}

## ----gran_tbl

gran_tbl <- function(.data, gran1, gran2, hierarchy_tbl = NULL) {
  if (!tsibble::is_tsibble(.data)) {
    stop("must use tsibble")
  }
  match_gran1 <- match(gran1, names(.data))
  match_gran2 <- match(gran2, names(.data))

  if (!is.null(match_gran1)) {
    var1 <- gran1
  }
  if (!is.null(match_gran2)) {
    var2 <- gran2
  }

  ind <- .data[[rlang::as_string(tsibble::index(.data))]]


  data_mutate <- .data %>%
    create_gran(gran1, hierarchy_tbl) %>%
    create_gran(gran2, hierarchy_tbl)

  # All possible combinations that are possible
  Allcomb <- data_mutate %>%
    tidyr::expand(.data[[gran1]], .data[[gran2]])


  combexist <- data_mutate %>%
    tibble::as_tibble(name_repair = "minimal") %>%
    dplyr::group_by(.data[[gran1]], .data[[gran2]]) %>%
    dplyr::summarise(
      count = dplyr::n()
    )

  output <- Allcomb %>%
    dplyr::left_join(combexist, by = c(gran1, gran2)) %>%
    dplyr::select(
      gran1, gran2,
      nobs := count
    ) %>%
    dplyr::mutate(nobs = tidyr::replace_na(nobs, 0))


  return(output)
}
## ----gran_obs



#' Cross tabulation of granularities
#' useful for validating if number of observations are sufficient to compute probability distributions
#'
#' @param .data A tsibble object.
#' @param gran1 One of the temporal granularities to check for harmonies.
#' @param gran2 The second temporal granularity in the pair.
#' @param hierarchy_tbl A hierarchy table specifying the hierarchy of units and their relationships
#' @param ... Added arguments to be passed
#' @return TRUE if two granularties are harmonies.
#' @examples
#' library(dplyr)
#' library(tsibbledata)
#' library(ggplot2)
#' vic_elec %>% gran_obs("hour_day", "day_week")
#' @export gran_obs



gran_obs <- function(.data, gran1, gran2, hierarchy_tbl = NULL) {
  gran_tbl(.data, gran1, gran2, hierarchy_tbl) %>%
    tidyr::spread(
      key = !!gran1,
      value = nobs
    )
}
