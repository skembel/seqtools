gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0 & !is.na(x)]), na.rm=na.rm) / length(x))
}

clr = function(x, base=2){
  x <- log((x / gm_mean(x)), base)
  x[!is.finite(x) | is.na(x)] <- 0.0
  return(x)
}

#' Centered log-ratio (CLR) transform community data
#'
#' @param x A vegan-format matrix of samples by taxa
#'
#' @return CLR-transformed matrix
#' @export
clr_transform = function(x) {
  return(t(apply(x, 1, clr)))
}
