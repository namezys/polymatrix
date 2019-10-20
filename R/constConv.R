# ---
# conversion between the 'matrix' and 'polyMatrix' representation of constant matrices matrix

M2pM <- function(m, class=c("polyMarray","polyMbroad","polyMcells","polyMdlist"))
{
  osztaly <- class[1]
  nrow <- dim(m)[1]
  ncol <- dim(m)[2]
  m<-as.numeric(m)

  check_class(osztaly)
  if (osztaly == CLASS_MARRAY) {
    return(polyMgen.a(nrow, ncol, rawData=m, degree=0))
  }
  if (osztaly == CLASS_MBOARD) {
    return(polyMgen.b(nrow, ncol, rawData=m, degree=0))
  }
  if (osztaly == CLASS_MDLIST) {
    return(polyMgen.c(nrow, ncol, rawData=m, degree=0))
  }
  stopifnot(osztaly == CLASS_MDLIST)
  p <- polynom::polynomial(1)
  ml <- lapply(as.list(m), function(x) x*p)
  return(polyMgen.d(nrow, ncol, rawData=ml))
}


pM2M <- function(pM)
{
  if (max(degree(pM))) {
    stop("The 'pM' is not a zero degree polynomial matrix")
  }
  return(const(pM))
}


# ----
# fine
