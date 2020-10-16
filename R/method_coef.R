# Title     : Get coeffisient matrix by using "[[" operator
# Created by: namezys
# Created on: 2020. 10. 16.

setMethod("[[", signature(x = PM, i = "numeric"), function(x, i) {
  if(length(i) != 1) {
    stop("Only one coefficient matrix can be gotten")
  }
  if(i > degree(x) || i < 0) {
    stop("degree out of bounds")
  }
  return(x@coef[, seq_len(ncol(x)) + ncol(x) * i])
})
