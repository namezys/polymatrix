# Title     : substraction
# Objective : implemented using multiplication and addiotion
# Created by: namezys
# Created on: 2020. 10. 25.


#' @describeIn polyMatrix substractioin
#'
#' @export
setMethod("-", signature(e1 = PM, e2 = PM), function(e1, e2) { e1 + (-e2) })
#' @describeIn polyMatrix substractioin
#'
#' @export
setMethod("-", signature(e1 = PM, e2 = "ANY"), function(e1, e2) {
  if(missing(e2)) {
    return((-1) * e1)
  }
  return(e1 + -e2)
})
#' @describeIn polyMatrix substractioin
#'
#' @export
setMethod("-", signature(e1 = "ANY", e2 = PM), function(e1, e2) { e1 + -e2 })

