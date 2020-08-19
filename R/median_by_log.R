# distance matrix between categories across facets

# x list input for each facet category
# x = normalise_max_JSdist(harmony_datai)
median_by_log <-function(x, base = 2){
  xunfold <- unlist(x)
  lenx <- length(xunfold)
  stats::median(xunfold)/log(lenx)
}
