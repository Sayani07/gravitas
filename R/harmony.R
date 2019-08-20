#' Get possible set of harmonies for a given tsibble
#'
#' @param .data A tsibble object.
#' @param ugran Upper temporal unit. Typically, it is set as the most coarse temporal unit required in the analysis. Default is "year".
#' @param lgran Lowest temporal unit. For "regular" tsibble, lgran is the interval of the tsibble. It needs to be specified for "irregular" time intervals.
#' @param facet_h highest level of facets allowed.
#' @param filter_in Choices of temporal units to be kept.
#' @param filter_out Choices of temporal units to be discarded.
#' @param ... added arguments to be passed.
#' @return compatibility table providing if the two granularities are harmonies or clashes. FALSE indicates a clash. If harmony, then a tibble with desired granularities returned.
#' @examples
#' library(dplyr)
#' library(tsibbledata)
#' tsibbledata::vic_elec %>% harmony(ugran = "day")
#' @export harmony
# tsibbledata::gafa_stock %>% harmony(lgran = "hour", ugran = "week")

harmony <- function(.data, ugran = "year", lgran = NULL, filter_in = NULL, filter_out = NULL, facet_h, ...) {


  set1 <- search_gran(.data, ugran, lgran, filter_in,  filter_out, ...)

  if (length(set1) == 1) {
    stop("Only one granularity ", set1, " can be formed. Function requires checking compatibility for bivariate granularities")
  }

  set1_merge <- merge(set1, set1) %>% as_tibble(.name_repair = "minimal") %>% dplyr:: filter(x!=y) %>% purrr::map_dfr(as.character)




  # getting the levels of facets and x-axis and storing them
  data_mutate <-  .data

  for(i in 1 :length(set1))
  {
    data_mutate <- data_mutate %>% create_gran(set1[i])
  }

  ilevel <- array()
  for(i in 1 : length(set1))
  {
    ilevel[i] <- data_mutate %>% dplyr::distinct(.data[[set1[[i]]]]) %>% nrow()
  }

  levels_tbl <- tibble::tibble(set1, ilevel, .name_repair = "minimal")

  ## In a matrix storing if each pair is harmony or clash

  harmony <- array(0, nrow(set1_merge))

  for (i in 1:nrow(set1_merge))
  {
    harmony[i] = is.harmony(.data, gran1 = set1_merge$x[i], gran2 = set1_merge$y[i], facet_h)
  }


  return_output <- set1_merge %>%
    dplyr::mutate(harmony = harmony)%>%
    dplyr::filter(harmony == "TRUE") %>%
    dplyr::rename(facet_variable = x, x_variable = y) %>%
    dplyr::left_join(levels_tbl, by = c("facet_variable" = "set1")) %>%
    dplyr::left_join(levels_tbl, by = c("x_variable" = "set1")) %>%
    dplyr::rename("facet_levels"="ilevel.x", "x_levels"="ilevel.y") %>%
    dplyr::select(-harmony)



  return_output





  ################################################
#
#   # capturing levels of all granularities from search_gran
#   data_mutate <-  .data
#
#   for(i in 1 :length(set1))
#   {
#   data_mutate <- data_mutate %>% create_gran(set1[i])
#   }
#
#   ilevel <- array()
#   for(i in 1 : length(set1))
#   {
#     ilevel[i] <- data_mutate %>% dplyr::distinct(.data[[set1[[i]]]]) %>% nrow()
#   }
#
#   levels_tbl <- tibble::tibble(set1, ilevel, .name_repair = "minimal")
#
#
#   if (length(set1) == 1) {
#     stop("Only one granularity ", set1, " can be formed. Function requires checking compatibility for bivariate granularities")
#   }
#
#
#   Allcomb <- t(combn(set1, 2))
#   colnames(Allcomb) <- c("gran1", "gran2")
#   Allcomb <- as_tibble(Allcomb)
#
#
#   harmony <- array(0, nrow(Allcomb))
#
#
#   for (i in 1:nrow(Allcomb))
#   {
#     harmony[i] <- is.harmony(.data, gran1 = Allcomb$gran1[i], gran2 = Allcomb$gran2[i], facet_h)
#   }
#
#   harmony_mt <- cbind(Allcomb, harmony) %>% as_tibble(name_repair = "minimal") %>% dplyr::rename(granularities = gran1)
#
#
#   set1_merge <- merge(set1, set1)
#
#   united_merge <- purrr::map_dfr(set1_merge, as.character) %>% dplyr::left_join(harmony_mt, by = c(x = "granularities", y = "gran2"))
#
#   united_merge$output <- array(NA, nrow(united_merge))
#
#   # just manipulation to put it in a matrix format
#
#   for (i in 1:length(united_merge$x))
#   {
#     for (j in 1:length(united_merge$y))
#     {
#       if (united_merge$x[i] == united_merge$y[i]) {
#         united_merge$output[i] <- FALSE
#       }
#       else if (united_merge$x[i] == united_merge$y[j] & united_merge$y[i] == united_merge$x[j]) {
#         united_merge$output[i] <- max(united_merge$harmony[i], united_merge$harmony[j], na.rm = TRUE)
#         united_merge$output[j] <- max(united_merge$harmony[i], united_merge$harmony[j], na.rm = TRUE)
#       }
#     }
#   }
#
#   united_merge <- united_merge %>% dplyr::mutate(check_harmony = dplyr::if_else(output == FALSE, 0, 1))
#
#   united_merge$x <- factor(united_merge$x, levels = set1)
#   united_merge$y <- factor(united_merge$y, levels = set1)
#
#   united_merge %>% dplyr::arrange(x, y) %>% dplyr::select(-c(harmony, output)) %>% dplyr::filter(check_harmony == 1) %>% dplyr::select(-check_harmony) %>% dplyr::rename(facet_variable = x, x_variable = y) %>% dplyr::left_join(levels_tbl, by = c("facet_variable" = "set1")) %>% dplyr::left_join(levels_tbl, by = c("x_variable" = "set1")) %>% dplyr::rename("facet_levels"="ilevel.x", "x_levels"="ilevel.y")
}

#
# comp_tbl <- function(.data, ugran = "year", lgran = NULL, filter_in = NULL, filter_out = NULL, ...) {
#   harmony(.data, ugran, lgran,  filter_in, filter_out, ...) %>% dplyr::mutate(check_harmony = 1) %>% tidyr::spread(granularity2, check_harmony) %>% replace(., is.na(.), "")
# }
