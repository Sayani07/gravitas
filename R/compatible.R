

compatible_table <-  function(.data, fn1,argfn1 = "week", fn2, argfn2 = "day")
{


  arg_opt = c("minute", "qhour", "hhour", "hour", "day", "week", "month", "quarter", "semester", "year")

  if (!argfn1 %in% arg_opt || !argfn2 %in% arg_opt) {
    stop(paste0("argument is not one of ", paste0(arg_opt, collapse = ", ")), call. = F)
  }

  argfn1 <- tolower(argfn1)
  argfn2 <- tolower(argfn2)

  mk_data = data %>% mutate(L1 <- fn1(x, argfn1),
                            L2 <- fn2(x, argfn2))

  #All possible combinations that are possible
  Allcomb <- tidyr::expand(mk_data, L1, L2)
  #All possible combinations that  exist
  combexist <- tidyr::expand(mk_data, nesting(L1, L2))
  #All possible combination that are missing
  cmbmiss <- Allcomb %>% anti_join(combexist)

  cmp_table = tibble()


}

