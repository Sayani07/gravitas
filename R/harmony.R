#' Get possible set of harmonies for a given tsibble
#'
#' Interaction of pair of granularities, categorised as harmony and clash. harmony () screens out harmony pairs from
#' list of all possible pairs of granularities.
#' @param .data A tsibble object.
#' @param ugran Typically set as the most coarse unit required in the analysis.
#'  Default is "year".
#' @param lgran For "regular" tsibble, lgran is the interval of the tsibble.
#' It needs to be specified for "irregular" time intervals. Typically serves as the finest unit required for analysis.
#' @param hierarchy_tbl A hierarchy table specifying the hierarchy of units and
#' their relationships
#' @param facet_h highest level of facets allowed.
#' @param filter_in Choices of temporal units to be kept. Can be column names if #' required granularity already exists in the tsibble. For example, a column with public holidays which needs to be treated as granularity, can be included here for checking how it interacts with other granularities.
#' @param filter_out Choices of temporal units to be discarded.
#' @param ... added arguments to be passed.
#' @return  A tibble of harmonies and their levels.The levels can be used to
#' decide which granularities to be plotted across x-axis/facets for
#' exploratory aid.
#' @examples
#' library(tsibbledata)
#' vic_elec %>% harmony(lgran = "hour", ugran = "week")
#' @export harmony
harmony <- function(.data,
                    ugran = "year",
                    lgran = NULL,
                    hierarchy_tbl = NULL,
                    filter_in = NULL,
                    filter_out = NULL,
                    facet_h = NULL, ...) {
  set1 <- search_gran(.data,
                      lowest_unit = lgran,
                      highest_unit = ugran,
                      hierarchy_tbl,
                      filter_in,
                      filter_out,
                      ...)

  if (is.null(facet_h)) {
    facet_h <- 31
  }
  if (length(set1) == 1) {
    stop("Only one granularity ", set1, " can be formed. Function requires checking compatibility for bivariate granularities")
  }

  set1_merge <- merge(set1, set1) %>%
    tibble::as_tibble(.name_repair = "minimal") %>%
    dplyr::filter(x != y) %>%
    purrr::map_dfr(as.character)

  # getting the levels of facets and x-axis and storing them
  data_mutate <- .data
  for (i in seq_along(set1)){
    data_mutate <- data_mutate %>% create_gran(set1[i], hierarchy_tbl)
  }

  ilevel <- array()
  for (i in seq_along(set1))
  {
    ilevel[i] <- data_mutate %>%
      dplyr::distinct(.data[[set1[[i]]]]) %>%
      nrow()
  }

  levels_tbl <- tibble::tibble(set1, ilevel, .name_repair = "minimal")

  har_data <- array(0, nrow(set1_merge))

  for (i in seq_len(nrow(set1_merge)))
  {
    har_data[i] <- is_harmony(.data, gran1 = set1_merge$x[i], gran2 = set1_merge$y[i], hierarchy_tbl, facet_h = facet_h)
  }

  return_output <- set1_merge %>%
    dplyr::mutate(harmony = har_data) %>%
    dplyr::filter(harmony == "TRUE") %>%
    dplyr::rename(facet_variable = x, x_variable = y) %>%
    dplyr::left_join(levels_tbl, by = c("facet_variable" = "set1")) %>%
    dplyr::left_join(levels_tbl, by = c("x_variable" = "set1")) %>%
    dplyr::rename("facet_levels" = "ilevel.x", "x_levels" = "ilevel.y") %>%
    dplyr::select(-harmony)

  return(return_output)
}
