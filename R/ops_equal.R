# Title     : equal and non equal
# Created by: namezys
# Created on: 2020. 10. 25.

#' @describeIn polyMatrix equal operator for two polinomial matrices, result is a boolean matrix
#'
#' @export
setMethod("==", signature(e1 = PM, e2 = PM), function(e1, e2) {
  if(nrow(e1) != nrow(e2) || ncol(e1) != ncol(e2)) {
    stop("non-conformable arrays")
  }
  if(degree(e1) < degree(e2)) {
    return(e2 == e1)
  }
  nr <- nrow(e1)
  nc <- ncol(e1)
  first_coef <- e1@coef
  second_coef <- cbind(e2@coef, matrix(0, nr, ncol(first_coef) - ncol(e2@coef)))
  res_per_degree <- (first_coef == second_coef)
  res <- res_per_degree[, 1:nc]
  for(d in 1:degree(e1)) {
    res <- res & res_per_degree[, (1:nc) + d * nc]
  }
  return(res)
})
#' @describeIn polyMatrix equal operator for polinomail matrix and polinomail, result is a matrix
#'
#' @export
setMethod("==", signature(e1 = PM, e2 = P), function(e1, e2) {
  if(degree(e2) > degree(e1)) {
    return(matrix(FALSE, nrow(e1), ncol(e1)))
  }
  nc <- ncol(e1)
  idx <- seq_len(ncol(e1))
  res <- (e1@coef[, idx] == e2[1])
  if(degree(e2) > 0) {
    for(d in 1:degree(e2)) {
      res <- res & (e1@coef[, idx + d * nc] == e2[d + 1])
    }
  }
  if(degree(e2) < degree(e1)) {
    for(d in (degree(e2) + 1):degree(e1)) {
      res <- res & (e1@coef[, idx + d * nc] == 0)
    }
  }
  return(res)
})
#' @describeIn polyMatrix equal operator for polinomial and numerical matrices
#'
#' @export
setMethod("==", signature(e1 = PM, e2 = M), function(e1, e2) { e1 == polyMatrix(e2, nrow(e2), ncol(e2)) })
#' @describeIn polyMatrix equal operator for polinomial matrix and number, result is a matrix
#'
#' @export
setMethod("==", signature(e1 = PM, e2 = NUM), function(e1, e2) {
  if(length(e2) != 1) {
    stop("Compare polyMatrix with sequence is unsupported")
  }
  return(e1 == polynom::polynomial(c(e2)))
})
#' @describeIn polyMatrix equal operator for aby object and polinomial matrix
#'
#' @export
setMethod("==", signature(e1 = "ANY", e2 = PM), function(e1, e2) { e2 == e1 })

#' @describeIn polyMatrix not equal operator
#'
#' @param e1 an left operand
#' @param e2 an right operand
#'
#' @export
setMethod("!=", signature(e1 = PM, e2 = "ANY"), function(e1, e2) { !(e1 == e2) })
#' @describeIn polyMatrix not equal operator
#'
#' @export
setMethod("!=", signature(e1 = "ANY", e2 = PM), function(e1, e2) { !(e2 == e1) })
