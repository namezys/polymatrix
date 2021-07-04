# Title     : Euclidean algorithm of triangularization
# Created by: namezys
# Created on: 2020. 10. 16.

# check if column after c-th row has only zero
.lq.eucl.is.cleaned <- function(pm, c, eps) {
  if (ncol(pm) == c) {
    return(TRUE)
  }
  return(all(is.zero(pm[(c + 1):nrow(pm), c], eps=eps)))
}

# perform Gausian elimination in column c starting from row c
.lq.eucl.col <- function(pm, c, eps) {
  stopifnot(nrow(pm) == ncol(pm) && c <= ncol(pm))
  selected_r <- NA
  selected_d <- NA
  for(r in c:nrow(pm)) {
    p <- pm[r, c]
    if (!is.zero(p, eps=eps)) {
      if (is.na(selected_d) || selected_d > degree(p)) {
        selected_d <- degree(p)
        selected_r <- r
      }
    }
  }
  stopifnot(!is.na(selected_r))
  q <- diag(1, nrow(pm), ncol(pm))
  if (selected_r != c) {
    # exchange columns
    q[c, c] <- 0
    q[selected_r, selected_r] <- 0
    q[selected_r, c] <- 1
    q[c, selected_r] <- 1
  }
  pm <- q %*% pm
  qq <- diag(polynom::polynomial(c(1)), nrow(pm), ncol(pm))
  p <- pm[c, c]
  for(r in (c + 1):nrow(pm)) {
    coef <- pm[r, c] / p
    qq[r, c] <- -coef
  }
  q <- qq %*% q
  pm <-  qq %*% pm
  for(r in (c + 1):nrow(pm)) {
    pm[r, c] <- 0
  }
  return(list(inv_q=q, r=pm))
}

#' Triangularization of a polynomial matrix by Euclidean division method
#'
#' @param pm matrix to triangularize
#' @param eps threshold of non zero coefficients
#' @return Upper tringular matrix `R`, transformation matrix `Q` and iverse `INV_Q` of matrix `Q`
#'
#' @details
#' The method use the for polynomials extended Euclidean algorithm.
#'
#' @seealso [triang_Sylvester()]
#' @export
triang.euclidean <- function (pm, eps=ZERO_EPS, iteration_limit=nrow(pm) * 100) {
  if (!is.polyMatrix(pm)) {
    stop("Only polynomial matrices are supported")
  }
  if (nrow(pm) != ncol(pm)) {
    stop("Only square matrices can be triangulirized")
  }
  r <- pm
  inv_q <- diag(polynom::polynomial(1), nrow(pm), ncol(pm))
  for(c in seq_len(ncol(pm) - 1)) {
    col_count_limit <- iteration_limit
    while(!.lq.eucl.is.cleaned(r, c, eps=eps)) {
      if (col_count_limit == 0) {
        stop("Iteration limit was reached")
      }
      col_count_limit <- col_count_limit - 1
      step <- .lq.eucl.col(r, c, eps=eps)
      inv_q <- step$inv_q %*% inv_q
      r <- step$r
    }
  }
  return(list(R=r, Q=inv(inv_q), INV_Q=inv_q))
}
