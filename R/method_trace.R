# Title     : Method trace
# Created by: namezys
# Created on: 2020. 10. 21.

#' Trace of a 'matrix' or 'polyMatrix' class matrix
#'
#' Trace of a matrix is the sum of the diagonal elements of the given matrix.
#'
#' @param x an matrix or a polynomial matrix
#' @return Returns the trace of the given matrix as a number or a polynomial.
#'
#' @details
#' If the given matrix is a polynomial matrix, the result will be a polynomial.
#'
#' @examples
#' # numerical matrices
#' m <- matrix(1:12, 3, 4)
#' ##      [,1] [,2] [,3] [,4]
#' ## [1,]    1    4    7   10
#' ## [2,]    2    5    8   11
#' ## [3,]    3    6    9   12
#' tr(m)  ## 15
#'
#' # polynomial matrix
#' pm <- parse.polyMatrix(
#'   "-3 + x^2, 2 + 4 x,  -x^2",
#'   "       1,       2, 3 + x",
#'   "     2*x,       0, 2 - 3 x"
#' )
#' tr(pm)  ## 1 - 3*x + x^2
#'
#' @export
tr <- function(x) {
  r <- 0
  for(i in seq_len(min(dim(x)))) {
    r <- r + x[i, i]
  }
  return(r)
}

