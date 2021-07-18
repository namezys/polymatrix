# -----------------
# the characteristic polynomial of a matrix or polynomial matrix

.charpolynom.matrix <- function(x)
{
  if (ncol(x) != nrow(x)) {
    stop("Square matrix is expected")
  }
  if (ncol(x) == 1) {
    return(x[1, 1] + polynom::polynomial(c(0, -1)))
  }
  s <- nrow(x)
  d <- polyMatrix(cbind(matrix(0, s, s), diag(-1, s, s)), s, s, 1)
  return(det(x + d))
}

.char.inc.degree <- function (cp) {
  return(cbind(0, cp))
}

.char.add <- function(f, s) {
  if (ncol(f) > ncol(s)) {
    return(.char.add(s, f))
  }
  s[,seq_len(ncol(f))] <- s[,seq_len(ncol(f))] + f
  return(s)
}

.char.step <- function (x, l) {
  if(is.matrix(l) && nrow(l) == 1) {
    l <- l[1, 1]
  }
  if(!is.matrix(l)) {
    x <- polyMatrix(x, 1, 1)
    if (l == 0) {
      return(x)
    } else {
      xx <- polyMatrix(matrix(c(0, -1), 1, 2), 1, 2)
      return(.char.add(x, xx))
    }
  }
  stopifnot(nrow(x) == ncol(x) && nrow(l) == ncol(l) && nrow(x) == nrow(l))
  stopifnot(nrow(x) != 1)
  # using 1st row
  res <- polyMatrix(0, 1, 1)
  for(c in seq_len(ncol(x))) {
    xx <- x[-1, -c]
    ll <- l[-1, -c]
    rr <- .char.step(xx, ll)
    if (c %% 2 == 0  ) {
      rr <- -rr
    }
    res <- .char.add(res, rr * x[1, c])
    if (l[1, c] == 1) {
      res <- .char.add(res, -.char.inc.degree(rr))
    }
  }
  return(res)
}

.charpolynom.polyMatrix <- function (x) {
  if (ncol(x) != nrow(x)) {
    stop("Square matrix is expected")
  }
  return(polyMatrixCharClass(coef=.char.step(x, diag(1, nrow(x), ncol(x)))))
}

#' Characteristic polynomial of a matrix
#'
#' @param x an matrix
#'
#' @export
setGeneric("charpolynom", function(x) {
  stop("Matrix object is expected")
})
#' @describeIn charpolynom for numerical matrix it is a polynomial with numerical coefficients
#'
#' @return When the input is a numerical matrix of `matrix` class
#'         the value is a `polynomial` object.
#'
#' @examples
#'
#' # numerical matrices
#' m <- matrix(c(2, 1,
#'              -1, 0), 2, 2, byrow=TRUE)
#' charpolynom(m)
#' @export
setMethod("charpolynom", signature(x="matrix"), .charpolynom.matrix)
#' @describeIn charpolynom for polynomial it treats as a matrix 1x1
#'
#' @export
setMethod("charpolynom", signature(x=P), function (x) {
  return(charpolynom(polyMatrix(x)))
})
#' @describeIn charpolynom for polynomial matrix has polynomial coefficients
#'
#' @details
#' The characteristic polynom of a polynomial matrix is a polynom with polynomial coefficients.
#'
#' @return When the input is a `polyMatrix` object
#'         then the value is `polyMatrixCharClass` class object,
#'
#' @seealso [polyMatrixCharClass]
#' @export
setMethod("charpolynom", signature(x=PM), .charpolynom.polyMatrix)
