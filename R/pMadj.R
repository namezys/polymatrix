# the adjugate of a given polynomial matrix
pMadj <- function(pm)
{ 
  if (dim(pm)[1] != dim(pm)[2]) {
    stop("'pm' must be a square matrix")
  }
  
  k <- dim(pm)[1]
  pd <- polyMconvert(pm, "polyMdlist") # convert to a 'polyMdlist' class object
  padj <- pd # skeleton of the result
  for(i in 1:k) {
    for(j in 1:k) {
      p <- pMdet(pMsub(pd, -i, -j)) * (-1) ^ (i + j)
      padj$dlist[[j]][[i]] <- p
      padj$degree[j, i] <- degree(p) 
    }
  }
  
  return(padj)
}


  
# ----
# fine
  
