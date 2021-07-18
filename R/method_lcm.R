# Title     : Largest common multiplier of a matrix
# Created by: namezys
# Created on: 2021. 05. 09.

#' LCM for polynomial matrices
#'
#' The least common multiple of polynomials or polynomial matrices.
#'
#' @param ... a list of polynomial objects
#' @seealso polynomial implementation [polynom::GCD()] and [GCD()]
#'
#' @export
setGeneric("LCM", polynom::LCM)
#' @describeIn LCM the least common multiple of polynomial matrices
#'
#' @examples
#'
#' # LCM of polynomial matrix
#' LCM(parse.polyMatrix(
#'  "  1 - x, 1 - x^2, 1 + 2*x + x^2",
#'  "x - x^2,   1 + x, 1 - 2*x + x^2"
#' ))  ## 0.25*x - 0.5*x^3 + 0.25*x^5
#'
#' @export
setMethod("LCM", signature(...=PM), function (...) { polynom::LCM(.as.list.polyMatrix(...)) })
