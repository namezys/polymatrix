# Title     : Get dimenstions
# Created by: namezys
# Created on: 2020. 10. 17.

setGeneric("nrow", nrow)
setMethod("nrow", signature(x = PM), function(x) {
  return(nrow(x@coef))
})
setMethod("nrow", signature(x = P), function(...) { 1 })

setGeneric("ncol", ncol)
setMethod("ncol", signature(x = PM), function(x) {
  return(x@ncol)
})
setMethod("ncol", signature(x = P), function(x) { 1 })

setGeneric("dim")
setMethod("dim", signature(x = PM), function(x) { c(nrow(x), ncol(x)) })
setMethod("dim", signature(x = P), function(x) { c(1, 1) })
