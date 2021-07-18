# Title     : addition
# Created by: namezys
# Created on: 2020. 10. 25.

#' @describeIn polyMatrix-Arith unary `+`
#' @return Unary `+` return same object.
#'
#' @export
setMethod("+", signature(e1 = PM, e2 = "missing"), function(e1, e2) { e1 })

#' @rdname polyMatrix-Arith
#' @return Binary `+` with two matrix operands returns elementwise summation.
#'
#' @export
setMethod("+", signature(e1 = PM, e2 = PM), function(e1, e2) {
  if(nrow(e1) != nrow(e2) || ncol(e1) != ncol(e2)) {
    stop("non-conformable arrays")
  }
  d <- degree(e1)
  if(d >= degree(e2)) {
    bc <- e1@coef
    sc <- e2@coef
  } else {
    d <- degree(e2)
    bc <- e2@coef
    sc <- e1@coef
  }
  bc[, seq_len(ncol(sc))] <- bc[, seq_len(ncol(sc))] + sc
  return(polyMatrix(bc, nrow(e1), ncol(e1), d))
})

#' @rdname polyMatrix-Arith
#' @return Binary `+` with matrix and scalar operands returns elementwise summation with scalar.
#'
#' @export
setMethod("+", signature(e1 = PM, e2 = P), function(e1, e2) {
  d <- degree(e2)
  res_d <- max(d, degree(e1))
  nc <- ncol(e1)
  coef <- cbind(e1@coef, matrix(0, nrow(e1), nc * (res_d - degree(e1))))
  src_c_idx <- seq_len(ncol(e1))
  for(i in 0:d) {
    c_idx <- src_c_idx + nc * i
    coef[, c_idx] <- coef[, c_idx] + e2[i + 1]
  }
  return(polyMatrix(coef, nrow(e1), nc, res_d))
})

#' @rdname polyMatrix-Arith
#'
#' @export
setMethod("+", signature(e1 = PM, e2 = "numeric"), function(e1, e2) {
  e1 + polynom::polynomial(e2)
})

#' @rdname polyMatrix-Arith
#'
#' @export
setMethod("+", signature(e1 = PM, e2 = "matrix"), function(e1, e2) { e1 + polyMatrix(e2, nrow(e2), ncol(e2)) })

#' @rdname polyMatrix-Arith
#'
#' @export
setMethod("+", signature(e1 = "ANY", e2 = PM), function(e1, e2) { e2 + e1 })
