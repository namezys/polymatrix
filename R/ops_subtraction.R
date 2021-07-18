# Title     : substraction
# Objective : implemented using multiplication and addiotion
# Created by: namezys
# Created on: 2020. 10. 25.


#' @describeIn polyMatrix-Arith unary `-`
#' @return Unary `-` return a matrix with changed sign.
#'
#' @export
setMethod("-", signature(e1 = PM, e2 = PM), function(e1, e2) { e1 + (-e2) })

#' @rdname polyMatrix-Arith
#' @return Binary '-' of matrices or scalar operands returns matrix subtraction.
#'
#' @export
setMethod("-", signature(e1 = PM, e2 = "ANY"), function(e1, e2) {
  if(missing(e2)) {
    return((-1) * e1)
  }
  return(e1 + -e2)
})
#' @rdname polyMatrix-Arith
#'
#' @export
setMethod("-", signature(e1 = "ANY", e2 = PM), function(e1, e2) { e1 + -e2 })

