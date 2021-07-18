# Title     : Gets coefficient matrix by using "[[" operator
# Created by: namezys
# Created on: 2020. 10. 16.

#' @describeIn polyMatrix get coefficient matrix by degree
#'
#' @param i the degree of the matrix of coefficient to be extracted
#' @examples
#'
#' # get coefficient matrix for degree 0
#' pm[[0]]
#' ##      [,1] [,2] [,3]
#' ## [1,]    0    1    0
#' ## [2 ]    1    1    0
#' # get coefficient matrix for degree 1
#' pm[[1]]
#' ##      [,1] [,2] [,3]
#' ## [1,]    1    0    3
#' ## [2 ]    0    0   -1
#'
#' @export
setMethod("[[", signature(x = PM, i = "numeric"), function(x, i) {
  if(length(i) != 1) {
    stop("Only one coefficient matrix can be gotten at once")
  }
  if(i > degree(x) || i < 0) {
    stop("degree out of bounds")
  }
  return(x@coef[, seq_len(ncol(x)) + ncol(x) * i])
})
#' @describeIn charpolynom get polynomial coefficient of characteristic polynomial
#'
#' @param i the degree of the polynomial coefficient to be extract
#' @export
setMethod("[[", signature(x = PMCP), function(x, i) {
  if(length(i) != 1) {
    stop("Only one coefficient matrix can be gotten at once")
  }
  if(i + 1 > ncol(x@coef) || i < 0) {
    stop("degree out of bounds")
  }
  return(x@coef[1, i + 1])
})
