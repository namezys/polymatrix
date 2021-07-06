# Title     : get diagnola matrix from polynomail
# Created by: namezys
# Created on: 2021. 04. 02.

.diag.polynomail <- function (x, nrow, ncol)
{
  if (missing(nrow) && missing(ncol)) {
    stop("'nrow' or 'ncol' must be provided when 'x' is a polynomial")
  }
  if (missing(nrow) && !missing(ncol)) {
    nrow <- ncol
  }
  if (!missing(nrow) && missing(ncol)) {
    ncol <- nrow
  }
  result <- polyMatrix(0, nrow, ncol)
  for(i in seq_len(min(nrow, ncol))) {
    result[i, i] <- x
  }
  return(result)
}

.diag.polyMatrix <- function (x, nrow, ncol)
{
  if (!missing(nrow) || !missing(ncol)) {
    stop("'nrow' or 'ncol' cannot be specified when 'x' is a matrix")
  }
  size <- min(dim(x)[1], dim(x)[2])
  result <- polyMatrix(0, 1, size)
  for(i in seq_len(size)) {
    result[1, i] <- x[i, i]
  }
  return(result)
}

#' Polynomial matrix Diagonals
#' Extract or construct a diagonal polynomial matrix.
#'
#' @param x a polynomial matrix, or a polynomial, or an \R object
#' @param nrow,ncol optional dimensions for the result when x is not a matrix.
#' @param names not usedd
#' @details
#'
#' In case of polynomail objets, `diag` has 2 distinct usage:
#'
#' * \code{x} is a polynomial, it returns a polynomial matrix the given diagonal
#'   and zero off-diagonal entries.
#' * \code{x} is a polynomial matrix, it returns a vector as a polynomial matrix of
#'    diagonal elements
#'
#' @seealso Base [base::diag()] for numericals and numerical matrices
#'
#' @examples
#'
#' # numericals and numerical matrix
#' diag(matrix(1:12, 3, 4)) ## 1 5 8
#' diag(9, 2, 2)
#' ##      [,1] [,2]
#' ## [1,]    9    0
#' ## [2,]    0    9
#'
#'
#' @export
setGeneric("diag", diag)
#' @describeIn diag for a polynomial, returns polynomial matrix with given diagonal
#'
#' @details
#'
#' For polynomial, either \code{nrow} or \code{ncol} must be provided.
#'
#' @examples
#'
#' # polynomial
#' diag(parse.polynomial("1+x+3x^2"), 2, 3)
#' ##                [,1]           [,2]   [,3]
#' ## [1,]   1 + x + 3x^2              0      0
#' ## [2,]              0   1 + x + 3x^2      0
#'
#' @export
setMethod("diag", signature(x=P), .diag.polynomail)
#' @describeIn diag for a polynomial matrix extract diagonal
#'
#' For polynomial matrix, neither \code{nrow} and \code{ncol} can't be provided.
#'
#' @examples
#'
#' # polynomial matrix
#' diag(parse.polyMatrix(
#'   "-3 + x^2, 2 + 4 x,  -x^2",
#'   "       1,       2, 3 + x",
#'   "      2x,       0, 2 - 3x"
#' ))
#' ##            [,1]   [,2]     [,3]
#' ## [1,]   -3 + x^2      2   2 - 3x
#'
#' @export
setMethod("diag", signature(x=PM), function (x) {.diag.polyMatrix(x)})
