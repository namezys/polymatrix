# Title     : Get dimenstions
# Created by: namezys
# Created on: 2020. 10. 17.

#' @export
setGeneric("nrow", nrow)
#' @describeIn polyMatrix the number of rows of a polynomial matrix
#'
#' @export
setMethod("nrow", signature(x = PM), function(x) { return(nrow(x@coef)) })
#' @describeIn polyMatrix a polynomial has only one row
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
#' @describeIn polyMatrix the number of columns of a polynomial matrix
#'
#' @export
setMethod("ncol", signature(x = PM), function(x) { return(x@ncol) })
#' @describeIn polyMatrix a polynomial has only one column
#'
#' @examples
#'
#' ncol(pm) ## 3
#'
#' @export
setMethod("ncol", signature(x = P), function(x) { 1 })

setGeneric("dim")
#' @describeIn polyMatrix the dimension of a polynomial matrix
#'
#' @examples
#'
#' dim(pm) ## [1] 2 3
#'
#' @export
setMethod("dim", signature(x = PM), function(x) { c(nrow(x), ncol(x)) })
