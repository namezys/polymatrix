# Title     : transpose
# Created by: namezys
# Created on: 2020. 10. 25.

.predict.polyMatrix <- function(object, newdata) {
  if (!polynom::is.polynomial(newdata) && !is.numeric(newdata)) {
    stop("Only polynomail or numerical argument is supported")
  }
  d <- degree(object)
  if (newdata == 0) {
    return(object[[0]])
  }
  result <- object[[d]]
  if (polynom::is.polynomial(newdata)) {
    result <- polyMatrix(result, nrow(result), ncol(result))
  }
  if(d == 0) {
    return(result)
  }
  for(i in (d - 1):0) {
    result <- result * newdata + object[[i]]
  }
  return(result)
}

.predict.polyMatrix.charPoly <- function(object, newdata) {
  if (!polynom::is.polynomial(newdata) && !is.numeric(newdata)) {
    stop("Only polynomail or numerical argument is supported")
  }
  d <- degree(object)
  if (newdata == 0) {
    return(object[[0]])
  }
  result <- object[[d]]
  if(d == 0) {
    return(result)
  }
  for(i in (d - 1):0) {
    result <- result * newdata + object[[i]]
  }
  return(result)
}

#' @export
setGeneric("predict", predict)
#' @describeIn polyMatrix value of polynomial matrix in point
#'
#' @param newdata the value to evaluate
#' @export
setMethod("predict", signature(object = PM), .predict.polyMatrix)
#' @describeIn charpolynom value of char polynomail in polynomial point
#'
#' @param newdata the value to evaluate
#' @export
setMethod("predict", signature(object = PMCP), .predict.polyMatrix.charPoly)
