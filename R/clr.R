gm_mean = function(x, include.zeroes, na.rm=TRUE) {
  if (include.zeroes) {
    exp(sum(log(x[!is.na(x)]), na.rm=na.rm) / length(x[!is.na(x)]))
  } else {
    exp(sum(log(x[x > 0 & !is.na(x)]), na.rm=na.rm) / length(x[x > 0 & !is.na(x)]))
  }
}

clr = function(x, include.zeroes){
  x <- log((x / gm_mean(x, include.zeroes=include.zeroes)))
  x[!is.finite(x) | is.na(x)] <- 0.0
  return(x)
}

#' Centered log-ratio (CLR) transform community data
#'
#' @param x A vegan-format matrix of samples by taxa
#' @param include.zeroes Include zero values when calculating geometric mean? (default=FALSE)
#'
#' @return CLR-transformed matrix
#' @export
clr_transform = function(x, include.zeroes=FALSE) {
  return(t(apply(x, 1, clr, include.zeroes=include.zeroes)))
}
