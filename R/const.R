const <-function(p) {
  #' the constant of a polynom or polynomial matrix
  UseMethod("const")
}

const.polynomial <- function(p) {
  return(as.numeric(p)[1])
}

const.polyMatrix <- function(p)
{
  if (is.polyMatrix.polyMarray(p)) {
    return(p$const)
  }
  if (is.polyMatrix.polyMbroad(p)) {
    return(p$broad[, 1:dim(p)[2]])
  }
  if (is.polyMatrix.polyMcells(p)) {
    return(p$cells[[1]])
  }
  if (is.polyMatrix.polyMdlist(p)) {
    result <- matrix(NA, nrow(p), ncol(p))
    for(r in 1:nrow(p)) {
      for(c in 1:ncol(p)) {
        result[r, c] <- as.numeric(p$dlist[[r]][[c]])[1]
      }
    }
    return(result)
  }
  stop("Not a regular 'polyMatrix' class object!")
}
