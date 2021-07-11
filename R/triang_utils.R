
zero_lead_rows <- function(M, eps=ZERO_EPS)
{
  #' Get zero lead rows of matrix M
  #'
  #' @param M Numerical matrix
  #' @param eps Machine epsilon to determinate zeros
  #' @return vector of idx (length is equal to columm number), NULL in case of error
  result <- apply(M, 2, function(x) {which(!is.zero(x, eps=eps))[1]})
  if (any(is.na(result))) {
    return(NULL)
  }
  return(result)
}

zero_lead_hyp_rows <- function(M, sub_nrow, esp=ZERO_EPS)
{
  #' Get zero lead hyper rows of size sub_nrow of matrix M
  #' @param M Numerical matrix
  #' @param sub_nrow Size of hyper row
  #' @param esp Machine epsilon to determinate zeros
  #' @return vector of idx of hyperrows, NaN for columns without zeros
  lead_rows <- zero_lead_rows(M, eps=esp)
  if (is.null(lead_rows)) {
    return(NULL)
  }
  return((lead_rows - 1) %/% sub_nrow + 1)
}
