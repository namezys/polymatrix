# Title     : Class of characteristic polinomail of polynomail matrix
# Objective : We use polyMatrix to store polynomial coefficients
# Created by: namezys
# Created on: 2021. 05. 05.


PMCP <- "polyMatrixCharPolynomial"

.check.polyMatrixCharClass <- function (object) {
  if (nrow(object@coef) != 1) {
    return("Char polynomial matrix should contrains only one row")
  }
  return(TRUE)
}

#' A class to repesent characteristic polynomial of polynomial matrix
#'
#' Characteristic polynomial of polynomial matrix is polynomial with polynomial coefficients
#' @export
polyMatrixCharClass <- setClass(
  PMCP,
  slots = c(coef=PM),
  validity = .check.polyMatrixCharClass
)
