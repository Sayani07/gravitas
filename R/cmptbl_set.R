#' Get harmonies for data sets
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param .data a tsibble object
#' @param lgran lower granularity
#' @param ugran gran granularity
#' @param ... added arguments to be passed
#' @return compatibility table providing if the two granularities are harmonies or clashes. FALSE indicates a clash. If harmony, then a tibble with desired granularities returned.
#' @examples
#' library(dplyr)
#' library(tsibbledata)
#' aus_elec %>% comp_tbl(lgran = "hour", ugran = "week")
#' @export comp_tbl
comp_tbl <- function(.data, lgran, ugran, ...) {
  if (!tsibble::is_tsibble(.data)) {
    stop("must use tsibble")
  }

  if (g_order(lgran, ugran) == 1) {
    stop("Only one granularity ", lgran, "_", {
      ugran
    }, " can be formed. Function requires checking compatibility for bivariate granularities")
  }



  ind <- .data[[rlang::as_string(tsibble::index(.data))]]
  granularity <- lookup_table$granularity
  index_gran1 <- granularity %>% match(x = lgran)
  index_gran2 <- granularity %>% match(x = ugran)
  gran2_set <- lookup_table$granularity[index_gran1:index_gran2]



  set1 <- paste(gran1 = combn(gran2_set, 2)[1, ], gran2 = combn(gran2_set, 2)[2, ], sep = "_")

  # set2 <- merge(prime, gran2_set) %>% as_tibble() %>% dplyr::mutate(x_y=paste(prime, gran2_set, sep="_"))

  # All_set <- c(set1, set2$x_y)

  Allcomb <- t(combn(set1, 2)) %>% as_tibble()
  # colnames(Allcomb) = c("Category 1, Category 2")

  # output <- .data %>% is.harmony(gran1 = Allcomb$V1[1], gran2 = Allcomb$V2[1])

  harmony <- array(0, nrow(Allcomb))

  for (i in 1:nrow(Allcomb))
  {
    harmony[i] <- is.harmony(.data, gran1 = Allcomb$V1[i], gran2 = Allcomb$V2[i])
  }

  harmony_mt <- cbind(Allcomb, harmony) %>% as_tibble() %>% dplyr::rename(granularities = V1)


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


  united_merge$x <- factor(united_merge$x, levels = set1)
  united_merge$y <- factor(united_merge$y, levels = set1)

  united_merge %>% dplyr::arrange(x, y) %>% dplyr::select(-harmony) %>% tidyr::spread(y, output) %>% dplyr::rename(granularities = "x")
}
