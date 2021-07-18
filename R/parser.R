# Title     : Parse string to polynomial or matrix
# Created by: namezys
# Created on: 2020. 10. 21.


.read.coef <- function(s) {
  res <- regexpr("^[+-]?\\d*\\.?\\d*([eE][+-]?\\d+)?", s, perl = TRUE)
  l <- attr(res, "match.length")
  if(l == 0) {
    return(list(v = NULL, s = 1, l = 0))
  }
  stopifnot(res == 1)
  c <- substr(s, 1, l)
  if(c == "+") {
    return(list(v = NULL, s = +1, l = 1))
  }
  if(c == "-") {
    return(list(v = NULL, s = -1, l = 1))
  }
  return(list(v = as.numeric(c), l = l))
}

.read.degree <- function(s, var) {
  re <- sprintf("^(?<v>%s)(\\^(?<d>\\d+))?", var)
  res <- regexpr(re, s, perl = TRUE)
  if(res == -1) {
    return(list(v = NULL, l = 0))
  }
  l <- attr(res, "match.length")
  starts <- attr(res, "capture.start")
  lengths <- attr(res, "capture.length")
  v_start <- starts[1, "v"]
  v_length <- lengths[1, "v"]
  d_start <- starts[1, "d"]
  d_length <- lengths[1, "d"]
  stopifnot(v_start == 1)
  stopifnot(substr(s, 1, v_length) == var)
  if(d_length == 0) {
    return(list(v = 1, l = l))
  }
  d <- as.integer(substr(s, d_start, d_start + d_length - 1))
  return(list(v = d, l = l))
}

.parse.polynomial <- function(s, var) {
  term_idx <- 0
  p <- 0
  while(s != "") {
    term_idx <- term_idx + 1
    coef_term <- .read.coef(s)
    s <- substr(s, 1 + coef_term$l, nchar(s))
    if(coef_term$l > 0 && substr(s, 1, 1) == "*") {
      s <- substr(s, 2, nchar(s))
    }
    d_term <- .read.degree(s, var)
    s <- substr(s, 1 + d_term$l, nchar(s))
    c <- coef_term$v
    d <- d_term$v
    if(is.null(c) && is.null(d)) {
      return(list(v = NULL, e = paste("invalid term at position", term_idx)))
    }
    if(is.null(c)) {
      c <- coef_term$s
    }
    if(is.null(d)) {
      d <- 0
    }
    if(d + 1 > length(p)) {
      p <- c(p, rep(0, d + 1 - length(p)))
    }
    p[d + 1] <- p[d + 1] + c
    if(s != "") {
      next_c <- substr(s, 1, 1)
      if(next_c != "+" && next_c != "-") {
        return(list(e = paste("expect '+' or '-' after term at position", term_idx)))
      }
    }
  }
  return(list(v = polynom::polynomial(p)))
}

.parse.polyMatrix.row <- function(s, var, row_idx = 0) {
  splitter_idxs <- as.integer(gregexpr("[,;&]", s)[[1]])
  if(length(splitter_idxs) == 1 && splitter_idxs == -1) {
    splitter_idxs <- NULL
  }
  splitter_idxs <- c(0, splitter_idxs, nchar(s) + 1)
  nc <- length(splitter_idxs) - 1
  res <- polyMatrix(0, 1, nc)
  for(i in seq_len(length(splitter_idxs) - 1)) {
    s_item <- substr(s, splitter_idxs[i] + 1, splitter_idxs[i + 1] - 1)
    item <- .parse.polynomial(s_item, var)
    if(!is.null(item$e)) {
      return(list(e = paste0(item$e, " in item [", row_idx, ", ", i, "]")))
    }
    res[1, i] <- item$v
  }
  return(list(v = res))
}

#' Parse polynomial from string
#'
#' Parse string representation of polynomial into a polynomial object.
#'
#' @param s an string for parsing
#' @param var an variable name
#'
#' @return new polynomial as `polynom::polynomial` object
#'
#' @seealso [parse.polyMatrix()]
#' @export
parse.polynomial <- function(s, var = "x") {
  if(!grepl("^[a-z]$", var) || var == "e") {
    stop(e = "invalid variable name")
  }
  s <- gsub("\\s", "", s, perl = TRUE)
  if(s == "") {
    stop("Fail to parse empty string")
  }
  res <- .parse.polynomial(s, var = var)
  if(!is.null(res$e)) {
    stop(paste("Fail to parse polynomial:", res$e))
  }
  return(res$v)
}

parse.polyMatrix.prepare <- function(...) {
  s <- paste(..., sep = "\\")
  rows <- strsplit(s, "\n|\\\\")
  rows <- sapply(rows, function(x) { gsub("\\s", "", x, perl = TRUE) })
  rows <- rows[rows != ""]
  return(rows)
}

#' Parse polynomial matrix from strings
#'
#' This is a convenient way to input a polynomial matrix.
#'
#' Space and tabulation characters are ignored.
#'
#' Row should be divided by new line "\code{\\n}" or backslash "\code{\\}" (TeX style).
#'
#' Elements in each row can be divided by "\code{,}", "\code{;}" or "\code{&}" (TeX style)
#'
#' For convenience, this function can accept multiple string.
#' In this case each string will be treated as a new row.
#'
#' This function accepts TeX matrix format.
#'
#' @param ... string or strings to parse
#' @param var variable character.
#'            Only lower latin characters are allowed except 'e' which is reseved for numbers
#'
#' @return new polynomial matrix of polyMatrix class
#'
#' @examples
#' parse.polyMatrix("       1, 2 + x",
#'                  "2 + 2x^2,    x^3")
#'
#' # The function can suggest mistake position in case of invalid format
#' \dontrun{
#' parse.polyMatrix(
#'     "1 + y &    2\\
#'         -2 &  x^2"
#' )
#' ## Fail to parse polyMatrix: invalid term at position 2 in item [1, 1]
#' }
#'
#' @seealso [parse.polynomial()]
#' @export
parse.polyMatrix <- function(..., var = "x") {
  # use "..." to suppress name hints in PyCharm
  if(!grepl("^[a-z]$", var) || var == "e") {
    stop(e = "invalid variable name")
  }
  rows <- parse.polyMatrix.prepare(...)
  res <- polyMatrix(0, length(rows), 1)
  for(i in seq_len(length(rows))) {
    p_r <- .parse.polyMatrix.row(rows[i], var, i)
    if(!is.null(p_r$e)) {
      stop(paste("Fail to parse polyMatrix:", p_r$e))
    }
    nc <- ncol(p_r$v)
    if(ncol(res) < nc) {
      res <- cbind(res, matrix(0, 1, nc - ncol(res)))
    }
    for(j in seq_len(ncol(p_r$v))) {
      res[i, j] <- p_r$v[1, j]
    }
  }
  return(res)
}
