# Title     : Inverse matrix
# Created by: namezys
# Created on: 2021. 05. 09.

#' Inverse polynomial matrix
#'
#' During inversion we will try to round elememnts to zero.
#'
#' @param x a polynomial matrix
#' @param eps zero threshold
#' @details
#' Right now only matrices with numerical determinant are supported.
#'
#' @export
inv <- function (x, eps=ZERO_EPS) {
  dd <- zero.round(det(x), eps=eps)
  if (degree(dd) != 0) {
    stop("Only matrices with numerical determinant are supported")
  }
  return((1/dd[1]) * adjoint(x))
}
