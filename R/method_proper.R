# Title     : Row and column proper prooperty of polyMatrix
# Created by: namezys

#' Proper polynomial matrices
#'
#' Polynomial matrix is column (row, full) proper (or reduced) if associated matrix has same rank
#' as the number of column (row)
#'
#' @param pm a polyMatrix objects
#' @return True if object \code{pm} is a (row-/column-) proper matrix
#'
#' @description Tests the proper property of polynomial matrix.
#'   A polynomial matrix is proper if the associeted matrix has a full rank.
#'
#' @examples
#'   pm <- parse.polyMatrix(
#'     "-1 + 7x     , x",
#'     " 3 - x + x^2, -1 + x^2 - 3 x^3"
#'   )
#'   is.column.proper(pm)
#'   is.row.proper(pm)
#'   is.proper(pm)
#'
#' @export
is.proper <- function (pm) {
  return(is.column.proper(pm) && is.row.proper(pm))
}

#' @describeIn is.proper tests if its argument is a column-proper matrix
#'
#' @export
is.column.proper <- function(pm) {
  col_degree <- apply(matrix.degree(pm), 2,  max)
  c_matrix <- matrix(NA, nrow(pm), ncol(pm))
  for(c in seq_len(ncol(pm))) {
    d <- col_degree[c]
    c_matrix[, c] <- pm[[d]][, c]
  }
  return(Matrix::rankMatrix(c_matrix) == ncol(pm))
}

#' @describeIn is.proper tests if its argument is a row-proper matrix
#'
#' @export
is.row.proper <- function (pm) {
  row_degree <- apply(matrix.degree(pm), 1, max)
  c_matrix <- matrix(NA, nrow(pm), ncol(pm))
  for(r in seq_len(nrow(pm))) {
    d <- row_degree[r]
    c_matrix[r, ] <- pm[[d]][r, ]
  }
  return(Matrix::rankMatrix(c_matrix) == nrow(pm))
}
