# Title     : Print polyMatrix object
# Created by: namezys
# Created on: 2020. 10. 16.

.format_polynom <- function(p, s = "x")
{
  res <- c()
  for(i in seq_along(p)) {
    d <- i - 1
    cc <- p[i]
    if (cc != 0) {
      if (d == 0) {
        res <- format(cc)
      } else {
        if (d == 1) {
          ss <- s
        } else {
          ss <- paste0(s, "^", d)
        }

        if (cc == 1) {
          if (length(res) == 0) {
            res <- c(res, ss)
          } else {
            res <- c(res, "+", ss)
          }
        } else if (cc == -1) {
          res <- c(res, "-", ss)
        } else if (cc < 0) {
          res <- c(res, "-", -cc, ss)
        } else {
          if (is.null(res)) {
            res <- c(res, cc, ss)
          } else {
            res <- c(res, "+", cc, ss)
          }
        }
      }
    }
  }
  if (is.null(res)) {
    return("0")
  }
  return(paste(res, collapse=" "))
}

setMethod("show", signature(object=PM), function(object) {
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
  col_sizes <- apply(res, 2, function(x) {max(sapply(x, nchar))})
  for(i in seq_len(nrow(res))) {
    s <- NULL
    for(j in seq_len(ncol(res))) {
      s <- c(s, format(res[i, j], justify="right", width=col_sizes[j]))
    }
    cat(s, sep="   ")
    cat("\n")
  }
})
