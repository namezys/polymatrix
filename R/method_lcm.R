# Title     : Lagest common multiplier of matrix
# Created by: namezys
# Created on: 2021. 05. 09.

#' @describeIn GCD the least common multiple of polynomial
#'
#' @export
setGeneric("LCM", polynom::LCM)
#' @describeIn GCD the least common multiple of polynomial matrices
#'
#' @examples
#'
#' # LCM of polynomial matrix
#' LCM(parse.polyMatrix(
#'  "  1 - x, 1 - x^2, 1 + 2*x + x^2",
#'  "x - x^2,   1 + x, 1 - 2*x + x^2"
#' ))  ## 0.25*x - 0.5*x^3 + 0.25*x^5
setMethod("LCM", signature(...=PM), function (...) { polynom::LCM(.as.list.polyMatrix(...)) })
