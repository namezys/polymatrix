# Title     : Subscribe to get element or part of matrix as polynom or numeric
# Created by: namezys
# Created on: 2020. 10. 17.


.rep.seq <- function(s, by, up_to, from = 0) {
  stopifnot(up_to >= from)
  res <- NULL
  for(d in from:up_to) {
    res <- c(res, s + by * d)
  }
  return(res)
}

.logic.index.to.integer.index <- function(logic_idx, limit) {
  if(length(logic_idx) > limit) {
    stop("(subscript) logical subscript too long")
  }
  logic_idx <- rep(logic_idx, length.out = limit)
  return(seq_len(length(logic_idx))[logic_idx])
}

.to.polynomail <- function(x) {
  p <- polynom::polynomial(x)
  return(if(length(p) == 1) p[1] else p)
}

#' Extract or Replace Parts of a polynomial matrix
#'
#' @param x a polynomial matrix
#' @param i row indeces
#' @param j column indeces
#' @param ... unused
#' @param drop unused
#'
#' @rdname polyMatrix-subscript
#' @export
setMethod("[", signature(x = PM, i = "missing", j = "missing", drop="missing"), function(x, ...) { x })
#' @describeIn polyMatrix-subscript get columns
#'
#' @export
setMethod("[", signature(x = PM, i = "missing", j = "ANY", drop="missing"), function(x, i, j) {
  x[seq_len(nrow(x)), j]
})
#' @describeIn polyMatrix-subscript gets rows
#'
#' @export
setMethod("[", signature(x = PM, i = "ANY", j = "missing", drop="missing"), function(x, i, j) {
  x[i, seq_len(ncol(x))]
})

#' @describeIn polyMatrix-subscript gets by logical index
#'
#' @export
setMethod("[", signature(x = PM, i = "logical", j = "logical", drop="missing"), function(x, i, j) {
  return(x[.logic.index.to.integer.index(i, nrow(x)), .logic.index.to.integer.index(j, ncol(x))])
})
#' @describeIn polyMatrix-subscript gets by logical index and numerical indices
#'
#' @export
setMethod("[", signature(x = PM, i = "logical", j = "numeric", drop="missing"), function(x, i, j) {
  return(x[.logic.index.to.integer.index(i, nrow(x)), j])
})
#' @describeIn polyMatrix-subscript gets by logical index and numerical indices
#'
#' @export
setMethod("[", signature(x = PM, i = "numeric", j = "logical", drop="missing"), function(x, i, j) {
  return(x[i, .logic.index.to.integer.index(j, ncol(x))])
})

.get.by.number.number <- function(x, i, j) {
  nr <- nrow(x)
  nc <- ncol(x)
  if(max(i) > nr || max(j) > nc) {
    stop("subscript out of bounds  OR RANGE...")
  }
  i <- i[i != 0]
  j <- j[j != 0]
  if(any(i < 0) || any(j < 0)) {
    if(any(i < 0)) {
      if(any(i > 0)) {
        stop("only 0's may be mixed with negative subscripts I DON'T UNDERSTAND")
      }
      i <- !(seq_len(nr) %in% -i)
    }
    if(any(j < 0)) {
      if(any(j > 0)) {
        stop("only 0's may be mixed with negative subscripts  SAME..")
      }
      j <- !(seq_len(nc) %in% -j)
    }
    return(x[i, j])
  }

  stopifnot(all(nr >= i) && all(i >= 1))
  stopifnot(all(nc >= j) && all(j >= 1))
  if(length(i) == 1 && length(j) == 1) {
    return(.to.polynomail(x@coef[i, j + nc * (0:degree(x))]))
  }
  c_idx <- .rep.seq(j, nc, degree(x))
  return(polyMatrix(x@coef[i, c_idx], length(i), length(j), degree(x)))
}
#' @describeIn polyMatrix-subscript gets by row and column indices
#'
#' @export
setMethod("[", signature(x = PM, i = "numeric", j = "numeric", drop="missing"),
          .get.by.number.number)

.set.numerical.or.matrix <- function(x, i, j, value) {
  if(any(i < 1) | any(nrow(x) < i)) {
    stop("subscript out of bounds")
  }
  if(any(j < 1) | any(ncol(x) < j)) {
    stop("subscript out of bounds   RANGE ... ")
  }
  if(degree(x) > 0) {
    x@coef[i, .rep.seq(j, ncol(x), up_to = degree(x), from = 1)] <- 0
  }
  x@coef[i, j] <- value
  x@coef <- .clean.coef(x@coef, ncol(x))
  return(x)
}

.set.polyMatrix <- function(x, i, j, value) {
  if(ncol(value) == 1 && nrow(value) == 1) {
    x[i, j] <- value[1, 1]
    return(x)
  }
  if(length(i) != nrow(value) || length(j) != ncol(value)) {
    stop("number of items to be replaced is not compatible with the replacement")
  }
  d <- degree(x)
  value_d <- degree(value)
  nc <- ncol(x)
  if(value_d > d) {
    x@coef <- cbind(x@coef, matrix(0, nrow(x), nc * (value_d - d)))
  } else if(value_d < d) {
    x@coef[i, .rep.seq(j, nc, up_to = d, from = value_d + 1)] <- 0
  }
  x@coef[i, .rep.seq(j, nc, up_to = value_d, from = 0)] <- value@coef
  x@coef <- .clean.coef(x@coef, nc)
  return(x)
}

.set.polynomial <- function(x, i, j, value) {
  if(length(value) == 1) {
    x[i, j] <- value[1]
    return(x)
  }
  d <- degree(x)
  value_d <- degree(value)
  nc <- ncol(x)
  if(value_d > d) {
    x@coef <- cbind(x@coef, matrix(0, nrow(x), nc * (value_d - d)))
  } else if(value_d < d) {
    x@coef[i, .rep.seq(j, nc, from = value_d + 1, up_to = d)] <- 0
  }
  value <- matrix(as.numeric(value), 1)
  r_dub <- rep(1, length(i))
  x@coef[i, .rep.seq(j, nc, up_to = value_d, from = 0)] <- value[r_dub, rep(seq_len(value_d + 1), each = length(j))]
  x@coef <- .clean.coef(x@coef, nc)
  return(x)
}

#' @describeIn polyMatrix-subscript replace o matrix by a new one
#'
#' @param value new value
#'
#' @export
setMethod("[<-", signature(x = PM, i = "missing", j = "missing"), function(x, i, j, value) {
  x[seq_len(nrow(x)), seq_len(ncol(x))] <- value
  return(x)
})
#' @describeIn polyMatrix-subscript assigns rows
#'
#' @export
setMethod("[<-", signature(x = PM, i = "missing"), function(x, i, j, value) {
  x[seq_len(nrow(x)), j] <- value
  return(x)
})
#' @describeIn polyMatrix-subscript assigns columns
#'
#' @export
setMethod("[<-", signature(x = PM, j = "missing"), function(x, i, j, value) {
  x[i, seq_len(ncol(x))] <- value
  return(x)
})

#' @describeIn polyMatrix-subscript replace part of matrix by one number
#'
#' @export
setMethod("[<-", signature(x = PM, i = "numeric", j = "numeric", value = "numeric"), .set.numerical.or.matrix)
#' @describeIn polyMatrix-subscript replace part of matrix by another numerical matrix.
#' Size of the new matrix should be same as replaced part
#'
#' @export
setMethod("[<-", signature(x = PM, i = "numeric", j = "numeric", value = "matrix"), .set.numerical.or.matrix)
#' @describeIn polyMatrix-subscript replace part of matrix by one polynomail
#'
#' @export
setMethod("[<-", signature(x = PM, i = "numeric", j = "numeric", value = P), .set.polynomial)
#' @describeIn polyMatrix-subscript replace part of matrix by another polunomial matrix.
#' Size of the new matrix should be same as replaced part
#'
#' @export
setMethod("[<-", signature(x = PM, i = "numeric", j = "numeric", value = PM), .set.polyMatrix)

.subsript.matrix.polynom <- function (x, i, j, value)
{
  stopifnot(is.matrix(x) && polynom::is.polynomial(value))
  result <- polyMatrix(x, nrow(x), ncol(x), 0)
  result[i, j] <- value
  return(result)
}
