# Title     : transpose
# Created by: namezys
# Created on: 2020. 10. 25.

.t.polyMatrix <- function(x) {
  nc <- ncol(x)
  nr <- nrow(x)
  d <- degree(x)
  col_idx <- seq_len(nc)
  row_idx <- seq_len(nr)
  c <- matrix(NA, nc, nr * (d + 1))
  for(i in 0:d) {
    c[, row_idx + i * nr] <- t(x@coef[, col_idx + i * nc])
  }
  return(polyMatrix(c, nc, nr, d))
}

#' @export
setGeneric("t")

#' Polynomial matrix transpose
#'
#' Given a polyMatrix, \code{t} returns the transpose of x
#' @param x a polyMatrix
#'
#' @examples
#' pm <- parse.polyMatrix("1, x, x^2",
#'                        "x, 1, x^3")
#' t(pm)
#' ##        [,1]   [,2]
#' ## [1,]      1      x
#' ## [2,]      x      1
#' ## [3,]    x^2    x^3
#' @seealso [base::t()] for numerical matrix tranpose
#'
#' @export
setMethod("t", signature(x = PM), .t.polyMatrix)
