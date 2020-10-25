# Title     : multiplication
# Created by: namezys
# Created on: 2020. 10. 16.

setMethod("*", signature(e1 = PM, e2 = "numeric"), function(e1, e2) {
  return(polyMatrix(e1@coef * e2, nrow(e1), ncol(e1), degree(e1)))
})
setMethod("*", signature(e1 = PM, e2 = P), function(e1, e2) {
  res_d <- degree(e1) + degree(e2)
  src_c_idx <- seq_len(ncol(e1@coef))
  nc <- ncol(e1)
  coef <- matrix(0, nrow(e1), nc * (res_d + 1))
  for(i in 0:degree(e2)) {
    column_idxs <- src_c_idx + nc * i
    coef[, column_idxs] <- coef[, column_idxs] + e1@coef * e2[i + 1]
  }
  return(polyMatrix(coef, nrow(e1), nc, res_d))
})
setMethod("*", signature(e1 = PM, e2 = PM), function(e1, e2) {
  stop("Per element multiplication for polyMatrix isn't supported")
})
setMethod("*", signature(e1 = "ANY", e2 = PM), function(e1, e2) { callGeneric(e2, e1) })

op <- parse.polyMatrix("1, 2 + x, 3 + x^2",
                       "x,   x^3, -6 + x^2")
op * parse.polynomial("1 - x")