#' Rounds objects to zero if there is too small
#'
#' @param x an \R object
#' @param eps Minimal numerical value which will not be treated as zero
#'
#' @details
#'
#' By befault `eps` = `{r} ZERO_EPS`
#'
#' @examples
#'
#' # numerical
#' zero.round(1)  ## 1
#' zero.round(0)  ## 0
#' zero.round(0.1, eps=0.5) ## 0
#' zero.round(c(1, 0, .01, 1e-10)) ##  1.00 0.00 0.01 0.00
#'
#' @seealso [is.zero()]
#'
#' @export
setGeneric("zero.round", function(x, eps = ZERO_EPS) {
  if (eps <= 0) {
    stop("'eps' must be greater than zero")
  }
  x[abs(x) < eps] <- 0
  return(x)
})

#' @describeIn zero.round rounding of a polynomial means rounding of each coefficient
#'
#' @examples
#'
#' # polynomials
#' zero.round(parse.polynomial("0.1 + x + 1e-7 x^2")) ## 0.1 + x
#' zero.round(parse.polynomial("0.1 + x + 1e-7 x^2"), eps=0.5) ## x
#'
#' @export
setMethod("zero.round", signature(x = P), function(x, eps = ZERO_EPS) {
  return(polynom::polynomial(zero.round(as.numeric(x), eps = eps)))
})
#' @describeIn zero.round rounding of a polynomial matrix
#'
#' @examples
#'
#' # polynomial matrix
#' zero.round(parse.polyMatrix(
#'   "1 + 0.1 x, 10 + x + 3e-8 x^2, 1e-8",
#'   "0.1 + x^2,     .1 + 1e-8 x^4, 1e-8 x^5"
#' ))
#' ##             [,1]     [,2]   [,3]
#' ## [1,]    1 + 0.1x   10 + x      0
#' ## [2,]   0.1 + x^2      0.1      0
#'
#' zero.round(parse.polyMatrix(
#'   "1 + 0.1 x, 10 + x + 3e-8 x^2, 1e-8",
#'   "0.1 + x^2,     .1 + 1e-8 x^4, 1e-8 x^5"
#' ), eps=0.5)
#' ##        [,1]     [,2]   [,3]
#' ## [1,]      1   10 + x      0
#' ## [2,]    x^2        0      0
#'
#' @export
setMethod("zero.round", signature(x = PM), function(x, eps = ZERO_EPS) {
  return(polyMatrix(zero.round(x@coef, eps = eps), nrow(x), ncol(x), degree(x)))
})
