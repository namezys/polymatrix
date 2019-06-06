# ---
# the transposed polyMatrix


t.polyMatrix <- function(pm)  
{ 
  pd <- polyMconvert(pm, "polyMdlist")
  tpd <- pd
  tpd$dim <- rev(tpd$dim)
  tpd$degree <- t(tpd$degree)
  j <- tpd$dim[1]
  k <- tpd$dim[2]
  tpd$dlist <- vector("list", j)
  
  for (i1 in 1:j) {
    tpd$dlist[[i1]] <- vector("list", k)
  }
  
  for (i1 in 1:j) {
    for(i2 in 1:k) {
      tpd$dlist[[i1]][[i2]] <- pd$dlist[[i2]][[i1]]
    }
  }
  
  return(tpd)
}


# ----
# fine




