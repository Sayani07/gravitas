# distance matrix between categories across facets

# x vector/matrix of distances across x-axis
median_by_log2 <-function(x){
  lenx <- length(x)
  stats:median(x)/log(lenx, base = 2)
}
