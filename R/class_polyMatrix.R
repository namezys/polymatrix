# Title     : Main class of polymatrix
# Objective : Definition of class
# Created by: namezys
# Created on: 2020. 10. 16.

PM <- "polyMatrix"
P <- "polynomial"
NUM <- "numeric"
M <- "matrix"

setOldClass(P)

.check.polyMatrix <- function(object) {
  if(object@ncol < 0) {
    return("Negative number of columns")
  }
  if(ncol(object@coef) %% object@ncol != 0) {
    return("Invalid coefficient matrix size")
  }
  return(TRUE)
}

#' An class to represent matrix of polinomails
#'
#' @slot coef A matrix of coefficientss which are joined into one matrix from lower degree to higher
#' @slot ncol Actuall number of columns in polynomail matrix
polyMatrixClass <- setClass(
  PM,
  representation(
    coef = "matrix",
    ncol = "integer"
  ),
  validity = .check.polyMatrix
)

.clean.coef <- function(coef, ncol) {
  # clean up coef matrix by removing zero tails
  stopifnot(ncol(coef) %% ncol == 0)
  d <- ncol(coef) / ncol - 1
  max_d <- 0
  for(i in d:0) {
    if(any(coef[, seq_len(ncol) + ncol * i] != 0)) {
      max_d <- i
      break
    }
  }
  if(max_d != d) {
    return(coef[, seq_len(ncol * (max_d + 1))])
  }
  return(coef)
}

.polyMatrix.raw <- function(data, ncol, .clean.up) {
  if(.clean.up) {
    data <- .clean.coef(data, ncol)
  }
  return(polyMatrixClass(coef = data, ncol = ncol))
}

polyMatrix <- function(data, nrow, ncol, degree = 0) {
  if(missing(data) || missing(nrow) || missing(ncol)) {
    stop("Not enough argument to create poly matrix")
  }
  if(length(nrow) > 1) {
    nrow <- nrow[1]
  }
  if(length(ncol) > 1) {
    ncol <- ncol[1]
  }
  if(nrow < 0) {
    stop("invalid 'nrow' value (< 0)")
  }
  if(ncol < 0) {
    stop("invalid 'ncol' value (< 0)")
  }
  if(degree < 0) {
    stop("invalid 'degree' value (< 0)")
  }
  if(!is.numeric(data)) {
    stop("'data' must be numeric type")
  }
  nrow <- as.integer(nrow)
  ncol <- as.integer(ncol)
  degree <- as.integer(degree)
  if(!is.matrix(data) ||
    nrow(data) != nrow ||
    ncol(data) != ncol * (degree + 1)) {
    data <- matrix(data, nrow, ncol * (degree + 1))
  }
  return(.polyMatrix.raw(data, ncol, .clean.up = TRUE))
}

is.polyMatrix <- function(x) {
  return(isS4(x) && class(x) == PM)
}