#' Round object to zero if it's too small
setGeneric("zero.round", function(x, eps = ZERO_EPS) {
  x[abs(x) < eps] <- 0
  return(x)
})
setMethod("zero.round", signature(x = P), function(x, eps = ZERO_EPS) {
  return(polynom::polynomial(zero.round(as.numeric(x), eps = eps)))
})
setMethod("zero.round", signature(x = PM), function(x, eps = ZERO_EPS) {
  return(polyMatrix(zero.round(x@coef, eps = eps), nrow(x), ncol(x), degree(x)))
})