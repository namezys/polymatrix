# Title     : determinant of polyMatrix
# Created by: namezys
# Created on: 2021. 04. 02.

.number.nonzero.item.in.row <- function(pm, r)
{
  # We should do it very fast, use coef matrix directly
  positions <- ncol(pm) * 0:degree(pm)
  result <- 0
  for(i in seq_len(ncol(pm))) {
    if(any(pm@coef[r, positions + i] != 0)) {
      result <- result + 1
    }
  }
  return(result)
}

.number.nonzero.item.in.column <- function(pm, c)
{
  positions <- ncol(pm) * 0:degree(pm) + c
  result <- 0
  for(i in seq_len(nrow(pm))) {
    if(any(pm@coef[i, positions] != 0)) {
      result <- result + 1
    }
  }
  return(result)
}

.det.polyMatrix.by.row <- function(pm, r)
{
  nr <- nrow(pm)
  result <- polynom::polynomial(0)
  for(i in seq_len(nr)) {
    c <- pm[r, i]
    if(c != 0) {
      cc <- if((i + r) %% 2 == 0) c else -c
      result <- result + cc * .det.polyMatrix(pm[1:nr != r, 1:nr != i])
    }
  }
  return(result)
}

.det.polyMatrix.by.col <- function(pm, c)
{
  nc <- ncol(pm)
  result <- polynom::polynomial(0)
  for(i in seq_len(nc)) {
    c <- pm[i, c]
    if(c != 0) {
      cc <- if((i + c) %% 2 == 0) c else -c
      result <- result + cc * .det.polyMatrix(pm[1:nc != i, 1:nc != c])
    }
  }
  return(result)
}

.det.polyMatrix <- function(pm) {
  if(polynom::is.polynomial(pm) || is.numeric(pm)) {
    return(pm)
  }
  stopifnot(is.polyMatrix(pm))
  nr <- nrow(pm)
  if(ncol(pm) != nr) {
    stop("a square matrix is expected")
  }
  if(nr == 1) {
    return(pm[1, 1])
  }
  best_row <- 1
  best_row_non_zero <- ncol(pm)
  for(i in seq_len(nrow(pm))) {
    non_zero <- .number.nonzero.item.in.row(pm, i)
    if(non_zero < best_row_non_zero) {
      best_row_non_zero <- non_zero
      best_row <- i
    }
  }
  best_col <- 1
  best_col_non_zero <- nrow(pm)
  for(i in seq_len(ncol(pm))) {
    non_zero <- .number.nonzero.item.in.column(pm, i)
    if(non_zero < best_col_non_zero) {
      best_col_non_zero <- non_zero
      best_col <- i
    }
  }
  if(best_col_non_zero < best_col_non_zero) {
    return(.det.polyMatrix.by.col(pm, best_col))
  }
  return(.det.polyMatrix.by.row(pm, best_row))
}

#' @export
setGeneric("det")
#' @describeIn polyMatrix determinant of a polynomial matrix
#'
#' @export
setMethod("det", signature(x = PM), function(x) { .det.polyMatrix(x) })

#' Minor of matrix item
#'
#' A minor of a matrix A is the determinant of some smaller square matrix,
#' cut down from A by removing one or more of its rows and columns.
#' Minors obtained by removing just one row and one column from square matrices (first minors).
#'
#' @param x a matrix
#' @param r,c row and column
#'
#' @export
minor <- function(x, r, c) {
  det(x[-r, -c])
}
