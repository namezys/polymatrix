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

#' A class to represent matrix of polinomials
#'
#' @slot coef A matrix of coefficients which are joined into one matrix from lower degree to higher
#' @slot ncol Actual number of columns in the polynomial matrix
#'
#' @examples
#'
#' # create a new polynomial matrix by parsing strings
#' pm <- parse.polyMatrix(
#'      "x; 1 + x^2; 3 x - x^2",
#'      "1; 1 + x^3; - x + x^3"
#' )
#' @export
#' @importFrom methods new
polyMatrixClass <- setClass(
  PM,
  slots = c(coef= "matrix", ncol = "integer"),
  validity = .check.polyMatrix
)

.clean.coef <- function(coef, ncol) {
  # clean up coef matrix by removing zero tails
  stopifnot(ncol(coef) %% ncol == 0)
  nr <- nrow(coef)
  d <- ncol(coef) / ncol - 1
  max_d <- 0
  for(i in d:0) {
    if(any(coef[, seq_len(ncol) + ncol * i] != 0)) {
      max_d <- i
      break
    }
  }
  if(max_d != d) {
    coef <- coef[, seq_len(ncol * (max_d + 1))]
    if (nr == 1 || max_d == 0 && ncol == 1) {
      coef <- matrix(coef, nr, ncol * (max_d + 1))
    }
  }
  if(!is.matrix(coef)) {
    coef <- matrix(coef)
  }
  return(coef)
}

.polyMatrix.raw <- function(data, ncol, .clean.up) {
  if(.clean.up) {
    data <- .clean.coef(data, ncol)
  }
  return(polyMatrixClass(coef = data, ncol = ncol))
}

#' Create polyMatrix object
#'
#' This function will create polynomial object fromm
#' coefficient matrix or signle value
#'
#' A coefficient matrix is a matrix which contains
#' matrices of coefficients from lower degree to higher
#' side-by-side
#'
#' @param data an matrix in case of creation from coefficient matrices or an numer/polynomial
#' @param nrow A numer of rows of matrix.
#'             If data is a matrix, default value is the number of rows of data matrix.
#'             In other case, it's a required parameter
#' @param ncol Must be positibe.
#'             If data is a matrix, default value is the number of columns of data matrix.
#'             In other ccase, it's a required parameter.
#' @param degree Degree of polynomials in coefficient matrix. Can't be negative.
#'        If data is polynomail, degree can be evaluated automatcal.
#'        In other case, default value is 0.
#' @return new polynomial matrix of polyMatrix class
#' @export
polyMatrix <- function(data, nrow, ncol, degree) {
  if(!missing(data) && is.polyMatrix(data)) {
    return(data)
  }
  if(polynom::is.polynomial(data)) {
    if(missing(nrow)) {
      nrow <- 1
    }
    if(missing(ncol)) {
      ncol <- 1
    }
    if(!missing(degree) && degree < length(data) - 1) {
      stop("Polynomail has higher degree than requested")
    }
    degree <- length(data) - 1
    nrow <- as.integer(nrow)
    ncol <- as.integer(ncol)
    degree <- as.integer(degree)
    coef_data <- matrix(0, nrow, ncol * (degree + 1))
    for(d in 0:degree) {
      coef_data[, d * ncol + 1:ncol] <- data[d + 1]
    }
    data <- coef_data
  }
  if(missing(degree)) {
    degree <- 0
  }
  if(missing(nrow) && !missing(data) && is.matrix(data)) {
    nrow <- nrow(data)
  }
  if(missing(ncol) && !missing(data) && is.matrix(data)) {
    ncol <- ncol(data)
  }
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
  if(!is.matrix(data) || nrow(data) != nrow || ncol(data) != ncol * (degree + 1)) {
    data <- matrix(data, nrow, ncol * (degree + 1))
  }
  return(.polyMatrix.raw(data, ncol, .clean.up = TRUE))
}

#' Check if object is polyMatrix
#'
#' @param x an \R object
#' @return TRUE if object is a polonial matrix
#' @examples
#' is.polyMatrix(c(1, 2, 3))
#' is.polyMatrix(polyMatrix(0, 2, 2))
#'
#' @export
is.polyMatrix <- function(x) {
  return(isS4(x) && class(x) == PM)
}

.as.list.polyMatrix <- function (pm) {
  result <- vector("list", length = nrow(pm) * ncol(pm))
  for(r in seq_len(nrow(pm))) {
    for(c in seq_len(ncol(pm))) {
      result[[c + ncol(pm) * (r - 1)]] <- pm[r, c]
    }
  }
  return(polynom::as.polylist(result))
}