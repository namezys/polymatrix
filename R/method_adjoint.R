# Title     : cofactor and adjoint matrix
# Created by: namezys
# Created on: 2021. 04. 29.

#' Cofactor of matrix
#'
#' @param x an matrix
#' @param r,c the row and column
#' @return cofactor which is number or polynomial
#'
#' @seealso [adjoint()]
#'
#' @export
cofactor <- function(x, r, c) {
  cc <- if((r + c) %% 2 == 0) 1 else -1
  return(cc * minor(x, r, c))
}

.adjoint.generic <- function(x) {
  n <- nrow(x)
  if(nrow(x) != ncol(x)) {
    stop("Square matrix is expected")
  }
  t(outer(1:n, 1:n, Vectorize(
    function(i, j) cofactor(x, i, j)
  )))
}

.adjoint.polyMatrix <- function(x) {
  if(nrow(x) != ncol(x)) {
    stop("Square matrix is expected")
  }
  result <- polyMatrix(0, nrow(x), ncol(x))
  for(r in seq_len(nrow(x))) {
    for(c in seq_len(ncol(x))) {
      result[r, c] <- cofactor(x, c, r)
    }
  }
  return(result)
}

#' Adjugate or classical adjoint of a square matrix
#'
#' The adjugate or classical adjoint of a square matrix is the transpose of its cofactor matrix.
#' It is also occasionally known as adjunct matrix,[ though this nomenclature appears to have decreased in usage.
#'
#' @param x an matrix
#'
#' @export
setGeneric("adjoint", .adjoint.generic)
#' @describeIn adjoint adjugate of polynomial matrix
#' @export
setMethod("adjoint", signature(x = PM), .adjoint.polyMatrix)
