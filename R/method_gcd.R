# Title     : Greater commmon divisor of matrix
# Created by: namezys
# Created on: 2021. 05. 09.

#' GCD for polynomial matrices
#'
#' The greatest common divisor of polynomials or polynomial matrices.
#'
#' @param ... an list of polynomial objects
#' @seealso polynomial implementation [polynom::GCD()] and [LCM()]
#'
#' @export
setGeneric("GCD", polynom::GCD)
#' @describeIn GCD the greatest common divisor of all elements of the polynomial matrice
#'
#' @examples
#'
#' # GCD of polynomial matrix
#'
#' GCD(parse.polyMatrix(
#'  "  1 - x, 1 - x^2, 1 + 2*x + x^2",
#'  "x - x^2,   1 + x, 1 - 2*x + x^2"
#' ))  ## 1
#'
#' @export
setMethod("GCD", signature(...=PM), function (...) { polynom::GCD(.as.list.polyMatrix(...)) })
