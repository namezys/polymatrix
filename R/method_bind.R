# Title     : row and column binding
# Objective : because core functions use only "..." we have to redefine the method
#             in order to allow us to use with polyMatrix in any poisition  of argument liist
# Created by: namezys
# Created on: 2020. 10. 23.

#' Combine polynomial matrices by rows or columns
#'
#' @param ... (generalized) vectors or matrices.
#'   If any of the objects is a polynomail matrix
#' @param deparse.level details in the base function, polynomial matrices doesn't use this argument
#'
#' @return if at least one argument is a polynomial matrix,
#'   the result will be a combined polynomial matrix.
#'   Otherwise, the base package implementation [base::cbind()] or [base::rbind()]
#'   will be called.
#'
#' @seealso [base::cbind()]
#'
#' @export
cbind <- function(..., deparse.level = 1) {
  vargs <- list(...)
  if(length(vargs) == 0) {
    return(NULL)
  }
  if(!any(sapply(vargs, is.polyMatrix))) {
    return(base::cbind(deparse.level = deparse.level, ...))
  }
  if(length(vargs) == 1) {
    return(vargs[[1]])
  }
  nc <- 0
  nr <- NULL
  for(i in vargs) {
    if(is.null(i)) {
      next
    }
    vnr <- nrow(i)
    if(!is.null(vnr) && vnr == 1) {
      vnr <- NULL
    }
    if(is.null(nr)) {
      nr <- vnr
    } else if(!is.null(vnr) && nr != vnr) {
      stop("the number of rows of the matrices must match")
    }
    vnc <- ncol(i)
    if(is.null(vnc)) {
      vnc <- 1
    }
    nc <- nc + vnc
  }
  if(is.null(nr)) {
    nr <- 1
  }
  res <- polyMatrix(0, nr, nc, 0)
  c <- 1
  for(i in vargs) {
    if(is.null(i)) {
      next
    }
    vnc <- ncol(i)
    if(is.null(vnc)) {
      vnc <- 1
    }
    res[, c:(c + vnc - 1)] <- i
    c <- c + vnc
  }
  return(res)
}

#' @describeIn cbind row based bind  DON'T UNDERSTAND.. !!!
#'
#' @export
rbind <- function(..., deparse.level = 1) {
  vargs <- list(...)
  if(length(vargs) == 0) {
    return(NULL)
  }
  if(!any(sapply(vargs, is.polyMatrix))) {
    return(base::rbind(deparse.level = deparse.level, ...))
  }
  if(length(vargs) == 1) {
    return(list(v = vargs[[1]]))
  }

  nc <- NULL
  nr <- 0
  for(i in vargs) {
    if(is.null(i)) {
      next
    }
    vnc <- ncol(i)
    if(!is.null(vnc) && vnc == 1) {
      vnc <- NULL
    }
    if(is.null(nc)) {
      nc <- vnc
    } else if(!is.null(vnc) && nc != vnc) {
      stop("the number of columns of the matrices must match")
    }
    vnr <- nrow(i)
    if(is.null(vnr)) {
      vnr <- 1
    }
    nr <- nr + vnr
  }
  if(is.null(nc)) {
    nc <- 1
  }
  res <- polyMatrix(0, nr, nc, 0)
  r <- 1
  for(i in vargs) {
    if(is.null(i)) {
      next
    }
    vnr <- nrow(i)
    if(is.null(vnr)) {
      vnr <- 1
    }
    res[r:(r + vnr - 1),] <- i
    r <- r + vnr
  }
  return(res)
}
