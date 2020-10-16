# Title     : Get dimenstions
# Created by: namezys
# Created on: 2020. 10. 17.

setGeneric("nrow", nrow)
setMethod("nrow", signature(x = PM), function(x) {
  return(nrow(x@coef))
})

setGeneric("ncol", ncol)
setMethod("ncol", signature(x = PM), function(x) {
  return(x@ncol)
})

setGeneric("dim")
setMethod("dim", signature(x = PM), function(x) { c(nrow(x), ncol(x)) })
