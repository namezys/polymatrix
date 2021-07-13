# Title     : Matrix multiplication
# Created by: namezys
# Created on: 2020. 10. 25.

#' Matrix multiplication
#'
#' Matrix multiplication accepts both polynomial and numerical matrices.
#'
#' @param x,y first and second operands
#'
#' @rdname polyMatrix-mmult
#' @export
setMethod("%*%", signature(x = PM, y = PM), function(x, y) {
  if(ncol(x) != nrow(y)) {
    stop("non-conformable arguments")
  }
  left_nc <- ncol(x)
  left_col_idx <- seq_len(left_nc)
  right_nc <- ncol(y)
  right_col_idx <- seq_len(right_nc)
  coef <- matrix(0, nrow(x), ncol(y) * (degree(x) + degree(y) + 1))
  for(i in 0:degree(x)) {
    for(j in 0:degree(y)) {
      rd <- i + j
      left <- x@coef[, left_col_idx + i * left_nc]
      right <- y@coef[, right_col_idx + j * right_nc]
      r_idx <- right_col_idx + rd * right_nc
      coef[, r_idx] <- coef[, r_idx] + (left %*% right)
    }
  }
  return(polyMatrix(coef, nrow(x), ncol(y), degree(x) + degree(y)))
})
#' @rdname polyMatrix-mmult
#'
#' @export
setMethod("%*%", signature(x = PM, y = "matrix"), function(x, y) { x %*% polyMatrix(y, nrow(y), ncol(y)) })
#' @rdname polyMatrix-mmult
#'
#' @export
setMethod("%*%", signature(x = "matrix", y = PM), function(x, y) { polyMatrix(x, nrow(x), ncol(x)) %*% y })
