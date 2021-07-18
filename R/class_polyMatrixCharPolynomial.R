# Title     : Class of characteristic polynomial of a polynomial matrix
# Objective : We use polyMatrix to store polynomial coefficients
# Created by: namezys
# Created on: 2021. 05. 05.


PMCP <- "polyMatrixCharPolynomial"

.check.polyMatrixCharClass <- function (object) {
  if (nrow(object@coef) != 1) {
    return("Char polynomial matrix should contain only one row")
  }
  return(TRUE)
}

#' A class to repesent characteristic polynomial of a polynomial matrix
#'
#' Characteristic polynomial of a polynomial matrix is a polynomial with polynomial coefficients
#' @export
polyMatrixCharClass <- setClass(
  PMCP,
  slots = c(coef=PM),
  validity = .check.polyMatrixCharClass
)
