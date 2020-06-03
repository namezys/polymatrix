dim.polyMatrix <- function(x) {
  #' The dimension of a polynomial matrix
  #'
  #' Retuns the value of the $dim element of the given \code{polyMatrix} object.
  #' Does not check the validity of the $dim element.
  #'
  #' @param x a `polyMatrix` class object
  #' @return A 2 element vector which contains the dimension parameters
  #'
  #' @examples
  #' dim(matrix(1:12,3,4)) # dim of a real matrix
  #'
  #' A <- polyMgen.a()
  #' class(A)# polyMarray
  #' dim(A) # dim of a polyMatrix
  #'
  #' @aliases dim
  check.is.polyMatrix(x);
  return(x$dim)
}
