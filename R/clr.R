gm_mean = function(x, ignore.zeroes=TRUE, na.rm=TRUE){
  if (ignore.zeroes) {
    exp(sum(log(x[x > 0 & !is.na(x)]), na.rm=na.rm) / length(x))
  } else {
    exp(sum(log(x[!is.na(x)]), na.rm=na.rm) / length(x))
  }
}

#' Centered log-ratio (CLR) transform community data
#'
#' @param x A vegan-format matrix of samples by taxa
#' @param base Base for logarithm (default=2)
#' @param ignore.zeroes Should zeros be included when calculating geometric mean (default=TRUE)
#'
#' @return CLR-transformed matrix
#' @export
clr_transform = function(x, base=2, ignore.zeroes=TRUE) {
  x <- log((x / gm_mean(x, ignore.zeroes)), base)
  x[!is.finite(x) | is.na(x)] <- 0.0
  return(t(apply(x, 1, clr)))
}
