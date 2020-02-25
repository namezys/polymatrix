dim.polyMatrix <- function(x) {
  #' The dim of a polynomial matrix
  check.is.polyMatrix(x);
  return(x$dim)
}
