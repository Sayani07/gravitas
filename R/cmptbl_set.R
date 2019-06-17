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
#'aus_elec %>% comp_tbl(lgran = "hour", ugran = "week")
#' @export comp_tbl
comp_tbl <- function(.data, lgran, ugran,  ...) {
  if (!tsibble::is_tsibble(.data)) {
    stop("must use tsibble")
  }

  ind <- .data[[rlang::as_string(tsibble::index(.data))]]
  granularity <- lookup_table$granularity
  index_gran1 <- granularity %>% match(x = lgran)
  index_gran2 <- granularity %>% match(x = ugran)
  gran2_set <- lookup_table$granularity[index_gran1:index_gran2]

  set1 <- paste(gran1 = combn(gran2_set, 2)[1,], gran2 = combn(gran2_set, 2)[2,], sep = "_")

  #set2 <- merge(prime, gran2_set) %>% as_tibble() %>% dplyr::mutate(x_y=paste(prime, gran2_set, sep="_"))

  #All_set <- c(set1, set2$x_y)

  Allcomb <-t(combn(set1, 2)) %>% as_tibble()
  #colnames(Allcomb) = c("Category 1, Category 2")

  #output <- .data %>% is.harmony(gran1 = Allcomb$V1[1], gran2 = Allcomb$V2[1])

  harmony = array(0, nrow(Allcomb))

  for(i in 1:nrow(Allcomb))
  {
  harmony[i] = is.harmony(.data, gran1 = Allcomb$V1[i], gran2 = Allcomb$V2[i])
  }

  output <- cbind(Allcomb, harmony) %>% as_tibble() %>% rename(granularities = V1)
  output


}


#
# %>% spread(V2, harmony)
#
# output
#
# #output = array(0, length(md$gran1))
#
# for(i in 1:length(md$gran1))
# {
#   for(j in 1:length(md$gran2))
#   {
#     if(md$gran1[i] == md$gran2[i])
#     {
#       md$output[i] = FALSE
#     }
#     else if(md$gran1[i] == md$gran2[j] & md$gran2[i]==md$gran1[j])
#     {
#       md$output[i] = max(md$harmony[i], md$harmony[j], na.rm =TRUE)
#       md$output[j] = max(md$harmony[i], md$harmony[j], na.rm =TRUE)
#     }
#
#   }
# }

