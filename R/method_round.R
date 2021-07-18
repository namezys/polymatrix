# Title     : Rounding of a polynomial matrix
# Created by: namezys
# Created on: 2020. 10. 16.

#' @describeIn polyMatrix rounding of a polynomial matrix is rounding of polynomial coefficients
#'
#' @param x a matrix object
#' @param digits an integer indicating the number of decimal places (round)
#'  or significant digits (signif) to be used
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
#'
#' @export
setMethod("round", signature(x = PM), function(x, digits = 0) {
  return(polyMatrix(round(x@coef, digits = digits), nrow(x), ncol(x), degree(x)))
})
