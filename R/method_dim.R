# Title     : Get dimenstions
# Created by: namezys
# Created on: 2020. 10. 17.

#' @export
setGeneric("nrow", nrow)
setMethod("nrow", signature(x = PM), function(x) { return(nrow(x@coef)) })
#' @describeIn polyMatrix number of rows of a polynomial matrix
#'
#' @examples
#'
#' # dimensions
#' nrow(pm) ## 2
#'
#' @export
setMethod("nrow", signature(x = P), function(x) { 1 })

#' @export
setGeneric("ncol", ncol)
setMethod("ncol", signature(x = PM), function(x) { return(x@ncol) })
#' @describeIn polyMatrix number of column of a polynomial matrix
#'
#' @examples
#'
#' ncol(pm) ## 3
#'
#' @export
setMethod("ncol", signature(x = P), function(x) { 1 })

setGeneric("dim")
#' @describeIn polyMatrix dimension of a polynomial matrix
#'
#' @examples
#'
#' dim(pm) ## [1] 2 3
#'
#' @export
setMethod("dim", signature(x = PM), function(x) { c(nrow(x), ncol(x)) })
