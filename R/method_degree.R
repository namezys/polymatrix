# Title     : Get degree of polynomail matrix: max degree, degree matrix
# Created by: namezys
# Created on: 2020. 10. 17.

#' Gets the maximum degree of polynomial objects
#'
#' Returns the maximum degree as an integer number.
#'
#' @param x an \R objects
#' @return The value is an integer number which can be different from zero only
#' for polynomial objects.
#'
#' @details
#' By default, this function raises error for unknown type of object.
#'
#' @export
setGeneric("degree", function(x) {
  stop(paste("Unknown object type:", class(x)[1]))
})

#' @describeIn degree a scalar argument always has zero degree
#'
#' @details
#' A numerical scalar can be treated as a polynomial with zero degree.
#'
#' @examples
#'
#' # numerical
#' degree(1)  ## 0
#'
#' @export
setMethod("degree", signature(x = NUM), function(x) {
  if(length(x) != 1) {
    stop("Numeric sequence is unsupported")
  }
  return(0)
})

#' @describeIn degree a numerical matrix always has zero degree
#'
#' @details
#' A numerical matrix has zero degree as each of its items has zero degree as well.
#'
#' @examples
#'
#' # numerical matrix
#' degree(matrix(1:6, 3, 2)) ## 0
#'
#' @export
setMethod("degree", signature(x = M), function(x) { as.integer(0) })

#' @describeIn degree the degree of a polynomial
#'
#' @details
#' For polynomials this function returns the highest degree of its terms with non-zero coefficient.
#'
#' @examples
#'
#' # polinomial
#' degree(parse.polynomial("1")) ## 0
#' degree(parse.polynomial("1 + x")) ## 1
#' degree(parse.polynomial("1 + x^3")) ## 3
#'
#' @export
setMethod("degree", signature(x = P), function(x) { length(x) - 1 })

#' @describeIn degree the degree of a polynomial matrix is the highest degree of its elements
#'
#' @examples
#'
#' # polynomial matrices
#' degree(parse.polyMatrix(
#'    "x; x^2 + 1",
#'    "0; 2x"))
#' ## 2
#'
#' @export
setMethod("degree", signature(x = PM), function(x) { as.integer(ncol(x@coef) / x@ncol - 1) })
#' @describeIn charpolynom the degree of char polynomial of polynomial matrix
#'
#' @export
setMethod("degree", signature(x = PMCP), function(x) { as.integer(ncol(x@coef) - 1) })

#' Degree of each item of the matrix
#'
#' Returns a matrix obtained by applying a function [degree()]
#' for each element of the matrix.
#'
#' @param x an \R object
#' @return
#' If the argument is a matrix, the result is a matrix of the same size
#' containing the degrees of the matrix items.
#'
#' @details
#' Degree of each item is calculated using [degree()] which is defined for polynomials
#' as the highest degree of the terms with non-zero coefficients.
#'
#' For convenience this function is defined for any object,
#' but returns zero for non polynomial objects.
#'
#' @export
setGeneric("matrix.degree", function(x) { 0 })
#' @describeIn matrix.degree the degree of a numerical matrix is a zero matrix for compatibility
#' @return For a numerical matrix the value is always a zero matrix of the same size
#'
#' @examples
#'
#' # numerical matrices
#' matrix.degree(matrix(1:6, 2, 3))
#' ##      [,1] [,2] [,3]
#' ## [1,]    0    0    0
#' ## [2,]    0    0    0
setMethod("matrix.degree", signature(x = M), function(x) { matrix(0, nrow(x), ncol(x)) })
#' @describeIn matrix.degree the degree of a polynomial
#' @return For a polynomial the value is the degree of the polynomial
#'
#' @examples
#'
#' # polynomials
#' matrix.degree(parse.polynomial("x + 1")) ## 1
#' matrix.degree(parse.polynomial("x^3 + 1")) ## 3
#' matrix.degree(parse.polynomial("1")) ## 0
setMethod("matrix.degree", signature(x = P), function(x) { degree(x) })
#' @describeIn matrix.degree a matrix of degrees for each polynomial item of the source matrix
#'
#' @examples
#'
#' # polynomial matrices
#' matrix.degree(parse.polyMatrix(
#'    "x; x^2 + 1",
#'    "0; 2x"))
#' ##      [,1] [,2]
#' ## [1,]    1    2
#' ## [2,]    0    1
setMethod("matrix.degree", signature(x = PM), function(x) {
  res <- matrix(NA, nrow(x), ncol(x))
  for(r in seq_len(nrow(res))) {
    for(c in seq_len(ncol(res))) {
      res[r, c] <- degree(x[r, c])
    }
  }
  return(res)
})
