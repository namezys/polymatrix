pMdet <- function(pm)
{
  if (!is.square(pm)) {
    stop("a square matrix is expected")
  }
  pd <- polyMconvert.dlist(pm)
  m <- dim(pd)[1]

  if (m == 1) {
    return(pd$dlist[[1]][[1]])
  }
  t <- m # before the first step: the perm.length; after: the actual permutation
  p <- polynom::polynomial(0) # the determinant
  for(k in 1:factorial(m)) {
    t <- permNext(t)
    p <- p + permSign(t) * pMprod(pMbas(pd, t, byrow=FALSE))
  }
  return(p)
}


# ----
# fine
