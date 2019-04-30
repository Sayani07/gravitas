#' Get compatibility tables for two granularities
#'

#' Date-time must be a  POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo,
#' zooreg, timeDate, xts, its, ti, jul, timeSeries, and fts objects.
#'

#' @param data a tsibble object
#' @param fn1 the first granularity function to use
#' @param fn2 the first granularity function to use
#' @param argfn1 granularity to be fed in fn1
#' @param argfn2 granularity to be fed in fn2
#'
#' @return compatibility table providing if the two granularities are harmonies or clashes. It also provides information on the range of the number of observations per combination and variation across number of combinations and other summery statistics.
#
#' @examples
#' \dontrun{
#' compatible_table(tsibbledata::aus_elec, gday, "week", ghour, "day" )
#' @export compatible_table

compatible_table <-  function(data, fn1, argfn1 = "week", fn2, argfn2 = "day")
{
  if(!tsibble::is_tsibble(data))
  {
    stop("must use tsibble")
  }

  ind =  data[[as_string(tsibble::index(data))]]

  arg_opt = c("minute", "qhour", "hhour", "hour", "day", "week", "month", "quarter", "semester", "year")

  argfn1 <- tolower(argfn1)
  argfn2 <- tolower(argfn2)

  if (!argfn1 %in% arg_opt || !argfn2 %in% arg_opt) {
    stop(paste0("argument is not one of ", paste0(arg_opt, collapse = ", ")), call. = F)
  }

  mk_data = data %>% mutate(L1 = fn1(ind, argfn1),
                            L2 = fn2(ind, argfn2))

  #All possible combinations that are possible
  Allcomb <- tidyr::expand(mk_data, L1, L2)
  #All possible combinations that  exist
  combexist <- tidyr::expand(mk_data, tidyr::nesting(L1, L2))
  #All possible combination that are missing
  cmbmiss <- Allcomb %>% anti_join(combexist)

  Type = if_else(nrow(cmbmiss)!= 0, "Clashes", "Harmonies")
  Obs_combinations = mk_data %>% as_tibble() %>% group_by(L1, L2) %>% count()
  Obs_comb_min = range(Obs_combinations$n)[1]
  Obs_comb_max = range(Obs_combinations$n)[2]
  Obs_comb_var = round(sd(Obs_combinations$n)/mean(Obs_combinations$n), digits = 3)

  Output = tibble(Type, `Min Obs` = Obs_comb_min, `Max Obs` = Obs_comb_max, `CV` = Obs_comb_var)

  Output
}

