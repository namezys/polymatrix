ZERO_EPS = 1e-5

is.zero <- function(x, eps=ZERO_EPS)
{
  #' Test if an something is zero
  #'
  #' Diffent types of objects can be treated as a zero in different way.
  #' Numerical type can be compare by absolute value with eps.
  #' Customer types should define an customer method.
  #' @param x The checked object
  #' @param eps Minimal numerical value which will not treat as zero
  #' @return TRUE if object can be treat as zero
  #'
  #' @examples
  #' is.zero(0) # TRUE
  #' is.zero(0.0001, eps=0.01) # TRUE
  #' is.zero(c(0, 1, 0)) # TRUE, FALSE, TRUE
  #' is.zero(matrix(c(1, 9, 0, 0), 2, 2))
  #' # FALSE TRUE
  #' # FALSE TRUE
  UseMethod("is.zero")
}

is.zero.default <- function(x, eps=ZERO_EPS)
{
  return(abs(x) < eps)
}

is.zero.polynomial <- function(x, eps=ZERO_EPS)
{
  return(all(abs(as.numeric(x)) < eps))
}

is.zero.polyMatrix <- function(x, eps=ZERO_EPS)
{
  pm <- polyMconvert.dlist(x)
  result <- matrix(NA, nrow(pm), ncol(pm))
  for(r in 1:nrow(pm)) {
    result[r, ] = sapply(pm$dlist[[r]], function(x) {is.zero(x, eps=eps)})
  }
  return(result)
}
