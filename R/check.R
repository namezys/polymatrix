is.polyMatrix <- function(x)
{
  return(inherits(x, CLASS_MATRIX))
}

is.polyMatrix.polyMarray <- function(x)
{
  return(inherits(x, CLASS_MARRAY))
}

is.polyMatrix.polyMbroad <- function(x)
{
  return(inherits(x, CLASS_MBOARD))
}

is.polyMatrix.polyMcells <- function(x)
{
  return(inherits(x, CLASS_MCELSS))
}

is.polyMatrix.polyMdlist <- function(x)
{
  return(inherits(x, CLASS_MDLIST))
}

is.square <- function(x)
{
  check.is.polyMatrix(x)
  return(dim(x)[1] == dim(x)[2])
}

check.is.matrix <- function(v)
{
  #' Check if v is a matrix and stop in other case
  if (!is.matrix(v)) {
    stop(paste("Expect a matrix, got", class(v)))
  }
}

check.is.polynomial <- function(v)
{
  #' Check if v is a polynomail object and stop in other case
  if (!polynom::is.polynomial(v)) {
    stop(paste("Expect a polynomail, got", class(v)))
  }
}

check.is.polyMatrix <- function(v)
{
  #' Check if v is a polynomail matrix and stop in other case
  if (!is.polyMatrix(v)) {
    stop(paste("Expect a polyMatrix, got", class(v)))
  }
}

check.is.polyMatrix.polyMdlist <- function(v)
{
  #' Check if v is a polynomail matrix in mdlist format and stop in other case
  if (!is.polyMatrix.polyMdlist(v)) {
    stop(paste("Expect a polyMatrix in dlist format, got", class(v)))
  }
}
