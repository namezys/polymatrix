ZERO_EPS <- 1e-5

#' Test if something is zero
#'
#' Generic function to check if we can treat on object as being zero.
#' For matrices the result is a matrix of the same size.
#'
#' @param x The checked object
#' @param eps Minimal numerical value which will not treat as zero
#' @return TRUE if the object can be treat as zero
#'
#' @details
#' Different type of objects can be treated as a zero in different ways:
#' * Numerical types can be compare by absolute value with \code{eps}.
#' * Customer types should define an an customer method.
#'
#' By befault eps:
#' ```{r}
#' ZERO_EPS
#' ```
#'
#' @seealso [zero.round()]
#'
#' @examples
#'
#' # numericals and matrices
#' is.zero(0) ## TRUE
#' is.zero(0.0001, eps=0.01) ## TRUE
#' is.zero(c(0, 1, 0)) ## TRUE, FALSE, TRUE
#' is.zero(matrix(c(1, 9, 0, 0), 2, 2))
#' # FALSE TRUE
#' # FALSE TRUE
#'
#' @export
setGeneric("is.zero", function (x, eps=ZERO_EPS) {abs(x) < eps})

#' @describeIn is.zero a polynomail can be treated as zero
#' if all its coefficients can be treated as zero
#'
#' @examples
#'
#' # polynomials
#' is.zero(parse.polynomial("0.1 - 0.5 x")) ## FALSE
#' is.zero(parse.polynomial("0.0001 - 0.0005 x + 0.00002 x^2"), eps=0.01) ## TRUE
#'
#' @export
setMethod("is.zero", signature(x=P), function (x, eps=ZERO_EPS) {all(abs(as.numeric(x)) < eps)})

#' @describeIn is.zero for a polunomial matrix every item is checked as polynomial
#'
#' @export
setMethod("is.zero", signature(x=PM), function (x, eps=ZERO_EPS) {
  return(polyMatrix.apply(x, function(x) {is.zero(x, eps=eps)}))
})