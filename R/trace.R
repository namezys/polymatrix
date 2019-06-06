# ----
# the matrix trace for 'matrix' and 'polyMatrix' class objects

tr <- function(matrixObject) {
  UseMethod("tr")
}

tr.matrix <- function(matrixObject)
{
  if (nrow(matrixObject) > ncol(matrixObject)) {
    matrixObject <- t(matrixObject)
  }

  minDim <- min(dim(matrixObject))
  numericSum <- sum(as.numeric(matrixObject)[seq(1, by = minDim + 1, l = minDim)])

  return(numericSum)
}

tr.polyMatrix <- function(matrixObject)
{
  pList <- polyMconvert(matrixObject, "polyMdlist")
  polynomialObject <- polynomial(0)

  if (dim(pList)[1] > dim(pList)[2]) {
    pList <- t(pList)
  }

  minDim <- min(dim(pList))

  for(i in 1:minDim) {
    polynomialObject <- polynomialObject + pList$dlist[[i]][[i]]
  }

  return(polynomialObject)
}


# ----
# fine
