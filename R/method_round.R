setMethod("round", signature(x = PM), function(x, digits = 0) {
  return(polyMatrix(round(x@coef, digits = digits), nrow(x), ncol(x), degree(x)))
})