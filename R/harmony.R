#' Get possible set of harmonies for a given tsibble
#'
#' @param .data A tsibble object.
#' @param ugran Upper temporal unit. Typically, it is set as the most coarse temporal unit required in the analysis. Default is "year".
#' @param lgran Lowest temporal unit. For "regular" tsibble, lgran is the interval of the tsibble. It needs to be specified for "irregular" time intervals.
#' @param filter_in Choices of temporal units to be kept.
#' @param filter_out Choices of temporal units to be discarded.
#' @param ... added arguments to be passed
#' @return compatibility table providing if the two granularities are harmonies or clashes. FALSE indicates a clash. If harmony, then a tibble with desired granularities returned.
#' @examples
#' library(dplyr)
#' library(tsibbledata)
#' tsibbledata::vic_elec %>% harmony(ugran = "day")
#' @export harmony
# tsibbledata::gafa_stock %>% harmony(lgran = "hour", ugran = "week")

harmony <- function(.data, ugran = "year", lgran = NULL, filter_in = NULL, filter_out = NULL, ...) {

  set1 <- search_gran(.data, ugran, lgran, filter_in,  filter_out, ...)


  if (length(set1) == 1) {
    stop("Only one granularity ", set1, " can be formed. Function requires checking compatibility for bivariate granularities")
  }
  # set2 <- merge(prime, gran2_set) %>% as_tibble() %>% dplyr::mutate(x_y=paste(prime, gran2_set, sep="_"))

  # All_set <- c(set1, set2$x_y)


  Allcomb <- t(combn(set1, 2)) %>% as_tibble(name_repair = "minimal")
  # colnames(Allcomb) = c("Category 1, Category 2")

  # output <- .data %>% is.harmony(gran1 = Allcomb$V1[1], gran2 = Allcomb$V2[1])

  harmony <- array(0, nrow(Allcomb))


  for (i in 1:nrow(Allcomb))
  {
    harmony[i] <- is.harmony(.data, gran1 = Allcomb$V1[i], gran2 = Allcomb$V2[i])
  }

  harmony_mt <- cbind(Allcomb, harmony) %>% as_tibble(name_repair = "minimal") %>% dplyr::rename(granularities = V1)


  set1_merge <- merge(set1, set1)

  united_merge <- purrr::map_dfr(set1_merge, as.character) %>% dplyr::left_join(harmony_mt, by = c(x = "granularities", y = "V2"))

  united_merge$output <- array(NA, nrow(united_merge))

  # just manipulation to put it in a matrix format

  for (i in 1:length(united_merge$x))
  {
    for (j in 1:length(united_merge$y))
    {
      if (united_merge$x[i] == united_merge$y[i]) {
        united_merge$output[i] <- FALSE
      }
      else if (united_merge$x[i] == united_merge$y[j] & united_merge$y[i] == united_merge$x[j]) {
        united_merge$output[i] <- max(united_merge$harmony[i], united_merge$harmony[j], na.rm = TRUE)
        united_merge$output[j] <- max(united_merge$harmony[i], united_merge$harmony[j], na.rm = TRUE)
      }
    }
  }

  united_merge <- united_merge %>% dplyr::mutate(check_harmony = dplyr::if_else(output == FALSE, 0, 1))

  united_merge$x <- factor(united_merge$x, levels = set1)
  united_merge$y <- factor(united_merge$y, levels = set1)

  united_merge %>% dplyr::arrange(x, y) %>% dplyr::select(-c(harmony, output)) %>% dplyr::filter(check_harmony == 1) %>% dplyr::select(-check_harmony) %>% dplyr::rename(granularity1 = x, granularity2 = y)
}

#
# comp_tbl <- function(.data, ugran = "year", lgran = NULL, filter_in = NULL, filter_out = NULL, ...) {
#   harmony(.data, ugran, lgran,  filter_in, filter_out, ...) %>% dplyr::mutate(check_harmony = 1) %>% tidyr::spread(granularity2, check_harmony) %>% replace(., is.na(.), "")
# }
