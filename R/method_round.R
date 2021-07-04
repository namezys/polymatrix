# Title     : Roung of polynomial matrix
# Created by: namezys
# Created on: 2020. 10. 16.

#' @describeIn polyMatrix round of polynomial matrix is rounding of polynomial coefficients
#'
#' @examples
#'
#' # round
#' round(parse.polyMatrix(
#'   "      1.0001 - x,            1 - x^2, 1 + 2.0003*x + x^2",
#'   "0.0001 + x - x^2, 1 + x + 0.0001 x^2, 1 - 2*x + x^2"
#' ))
#' ##           [,1]      [,2]           [,3]
#' ## [1,]     1 - x   1 - x^2   1 + 2x + x^2
#' ## [2,]   x - x^2     1 + x   1 - 2x + x^2
setMethod("round", signature(x = PM), function(x, digits = 0) {
  return(polyMatrix(round(x@coef, digits = digits), nrow(x), ncol(x), degree(x)))
})