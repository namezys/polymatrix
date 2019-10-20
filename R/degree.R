# ---
# degree of a polynom or polynomial matrix

degree <- function(p,...) UseMethod("degree")

degree.polynomial <- function(p,...)  return(length(p)-1)

degree.polyMatrix <- function(p, method=c("default","matrix","column","row"), review=FALSE, level=2,...)
{
  if(!review) {
    d <- switch(substr(method[1],1,1),
                "d"=max(p$degree),     # default
                "m"=p$degree,          # matrix
                "c"=colMax(p$degree),  # column
                "r"=rowMax(p$degree))  # row
    return(d)
  } else {
    # level=2 revise only
    # level=1 revise and check
    # level=0 check only
    degree.orig <- p$degree
    degree.orig[is.na(degree.orig)] <- -1
    dim.orig <- p$dim
    p.rev <- p
    p <- polyMconvert(p,"polyMdlist")
    n <- length(p$dlist)
    m <- length(p$dlist[[1]])
    degree <- matrix(0,n,m)
    for(i in 1:n) for(j in 1:m) degree[i,j] <- degree(p$dlist[[i]][[j]])
    if(all(dim.orig!=c(n,m))) stop("A 'dim' inconsistency in the given polyMatrix")
    ret <- if(all(degree == degree.orig)) TRUE else FALSE
    if (level>0) p.rev$degree <- degree
    switch(paste(level),
           "0" = return(degree_matrix_OK=ret),
           "1" = return(list(revised=p.rev,degree.orig=degree.orig,degree.act=degree,degree_matrix_OK=ret)),
           "2" = return(revised=p.rev)
    )
  }
}



# ----
# fine
