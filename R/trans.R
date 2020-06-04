t.polyMatrix <- function(x)
{
  #' Matrix transpose of a \code{polyMatrix} class object
  #'
  #' The method first convert the storage method of
  #' the given \code{polyMatrix} object to \code{polyMdlist} class interpretation,
  #' then flips the \code{polyMatrix} over its diagonal.
  #' @param x \code{polyMatrix} class object
  #' @return A '\code{polyMdlist}, \code{polyMatrix}' class object,
  #' the transposed version of the given \code{x} polynomial matrix.
  #'
  #' @examples
  #' m <- matrix(1:12,3,4)
  #' t(m) # the \code{base::t()} function
  #'
  #' pa <- polyMgen.a()
  #' pm <- t(pa)
  #' dim(pm) # 3 x 2
  #' class(pm) #  "polyMdlist" "polyMatrix"
  #'
  #' @seealso The \sQuote{\code{t}} in the \code{base} package.

  pd <- polyMconvert(x, "polyMdlist")
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
