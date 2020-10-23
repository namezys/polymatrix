# Title     : Get degree of polynomail matrix: max degree, degree matrix
# Created by: namezys
# Created on: 2020. 10. 17.

#' Get maximum degree
setGeneric("degree", function(x) {
  stop(paste("Unknown object type:", class(x)[1]))
})
setMethod("degree", signature(x = NUM), function(x) {
  if(length(x) != 1) {
    stop("Numeric suquence is unsupported")
  }
  return(0)
})
setMethod("degree", signature(x = M), function(x) { as.integer(0) })
setMethod("degree", signature(x = P), function(x) { length(x) - 1 })
setMethod("degree", signature(x = PM), function(x) { as.integer(ncol(x@coef) / x@ncol - 1) })
