# Title     : Apply for all element in numerical or polynomial matrix
# Created by: namezys
# Created on: 2020. 10. 23.

#' Apply for polynomial matrix
#'
#' Apply function to each element of matrix
#'
#' @param x an polynomial matrix
#' @param f an function with only one argument
#'
#' @export
polyMatrix.apply <- function(x, f) {
  if (ncol(x) == 0 || nrow(x) == 0) {
    return(matrix(NA, ncol(x), nrow(x)))
  }
  ff <- f(x[1, 1])
  if (polynom::is.polynomial(ff)) {
    res <- polyMatrix(0, nrow(x), ncol(x))
    res[1, 1] <- ff
  } else {
    res <- matrix(ff, nrow(x), ncol(x))
  }
  for(r in seq_len(nrow(x))) {
    for(c in seq_len(ncol(x))) {
      if (r == 1 && c == 1) {
        next
      }
      res[r, c] <- f(x[r, c])
    }
  }
  return(res)
}
