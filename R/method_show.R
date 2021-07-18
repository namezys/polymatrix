# Title     : Print polyMatrix object
# Created by: namezys
# Created on: 2020. 10. 16.

.format_polynom <- function(p, s = "x")
{
  res <- NULL
  for(i in seq_along(p)) {
    d <- i - 1
    cc <- p[i]
    if(cc != 0) {
      if(d == 0) {
        res <- format(cc)
      } else {
        if(d == 1) {
          ss <- s
        } else {
          ss <- paste0(s, "^", d)
        }

        if(cc == 1) {
          if(length(res) == 0) {
            res <- c(res, ss)
          } else {
            res <- c(res, " + ", ss)
          }
        } else if(cc == -1) {
          res <- c(res, " - ", ss)
        } else if(cc < 0) {
          res <- c(res, " - ", -cc, ss)
        } else {
          if(is.null(res)) {
            res <- c(res, cc, ss)
          } else {
            res <- c(res, " + ", cc, ss)
          }
        }
      }
    }
  }
  if(is.null(res)) {
    return("0")
  }
  return(paste(res, collapse = ""))
}

.show.polyMarix <- function(object) {
  res <- matrix("+", nrow(object) + 1, ncol(object) + 1)
  res[1, 1] <- " "
  for(j in seq_len(ncol(object))) {
    res[1, j + 1] <- paste0("[,", j, "]")
  }
  for(i in seq_len(nrow(object))) {
    res[i + 1, 1] <- paste0("[", i, ",]")
    for(j in seq_len(ncol(object))) {
      res[i + 1, j + 1] <- .format_polynom(object[i, j])
    }
  }
  col_sizes <- apply(res, 2, function(x) { max(sapply(x, nchar)) })
  for(i in seq_len(nrow(res))) {
    s <- NULL
    for(j in seq_len(ncol(res))) {
      s <- c(s, format(res[i, j], justify = "right", width = col_sizes[j]))
    }
    cat(s, sep = "   ")
    cat("\n")
  }
}

.show.polyMarixCharPolynomial <- function (cp) {
  res <- NULL
  for(i in seq_len(ncol(cp@coef))) {
    pc <- cp@coef[1, i]
    if (pc != 0) {
      str_p <- .format_polynom(pc)
      if (sum(c(pc) != 0) == 1) {
        if (sum(c(pc) > 0) == 1) {
          str_p <- paste(c("+ ", str_p), collapse = "")
        }
        if (!is.null(res)) {
          res <- c(res, str_p)
        } else {
          res <- str_p
        }
      } else {
        if (!is.null(res)) {
          stopifnot(i != 1)
          res <- c(res, "+", paste("(", str_p, ")", collapse = ""))
        } else {
          if (i == 1) {
            res <- str_p
          } else {
            res <- paste("(", str_p, ")", collapse = "")
          }
        }
      }
      if (i == 2) {
        res <- c(res, "l")
      } else if (i > 2){
        res <- c(res, paste(c("l^", i - 1), collapse = ""))
      }
    }
  }
  cat(paste(res, collapse = " "))
  cat("\n")
}

#' @describeIn polyMatrix prints out a text representation of a polynomial matrix
#'
#' @param object an \R object
#' @examples
#'
#' # print out a polynomial matrix
#' show(parse.polyMatrix(
#'   "      1.0001 - x,          1 - x^2, 1 + 2.0003*x + x^2",
#'   "0.0001 + x - x^2,            1 + x, 1 - 2*x + x^2",
#'   "        12.3 x^3,  2 + 3.5 x + x^4, -0.7 + 1.6e-3 x^3"
#' ))
#' ##                   [,1]             [,2]                [,3]
#' ## [1,]        1.0001 - x          1 - x^2   1 + 2.0003x + x^2
#' ## [2,]   1e-04 + x - x^2            1 + x        1 - 2x + x^2
#' ## [3,]           12.3x^3   2 + 3.5x + x^4    -0.7 + 0.0016x^3
#'
#' @export
#' @importFrom methods show
setMethod("show", signature(object = PM), .show.polyMarix)
#' @describeIn charpolynom prints out a text representation of a characteristic polynomial of
#' a polynomial matrix
#'
#' @param object an \R object
#'
#' @export
setMethod("show", signature(object = PMCP), function (object) {
  .show.polyMarixCharPolynomial(object)
})
