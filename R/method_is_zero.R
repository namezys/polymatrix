ZERO_EPS <- 1e-5

#' Test if an something is zero
#'
#' Diffent types of objects can be treated as a zero in different way.
#' Numerical type can be compare by absolute value with eps.
#' Customer types should define an customer method.
#' @param x The checked object
#' @param eps Minimal numerical value which will not treat as zero
#' @return TRUE if object can be treat as zero
#'
#' @examples
#' is.zero(0) # TRUE
#' is.zero(0.0001, eps=0.01) # TRUE
#' is.zero(c(0, 1, 0)) # TRUE, FALSE, TRUE
#' is.zero(matrix(c(1, 9, 0, 0), 2, 2))
#' # FALSE TRUE
#' # FALSE TRUE
setGeneric("is.zero", function (x, eps=ZERO_EPS) {abs(x) < eps})
setMethod("is.zero", signature(x=P), function (x, eps=ZERO_EPS) {all(abs(as.numeric(x)) < eps)})
setMethod("is.zero", signature(x=PM), function (x, eps=ZERO_EPS) {
  return(polyMatrix.apply(x, function(x) {is.zero(x, eps=eps)}))
})