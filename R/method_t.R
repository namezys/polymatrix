# Title     : transpose
# Created by: namezys
# Created on: 2020. 10. 25.

setGeneric("t")
setMethod("t", signature(x = PM), function(x) {
  nc <- ncol(x)
  nr <- nrow(x)
  d <- degree(x)
  col_idx <- seq_len(nc)
  row_idx <- seq_len(nr)
  c <- matrix(NA, nc, nr * (d + 1))
  for(i in 0:d) {
    c[, row_idx + i * nr] <- t(x@coef[, col_idx + i * nc])
  }
  return(polyMatrix(c, nc, nr, d))
})
