# ---
# conversion between the 'matrix' and 'polyMatrix' representation

M2pM <- function(m, class=CLASS_MARRAY)
{
  nrow <- dim(m)[1]
  ncol <- dim(m)[2]
  m <- as.numeric(m)

  check_class(class)
  if (class == CLASS_MARRAY) {
    return(polyMgen.a(nrow, ncol, rawData=m, degree=0))
  }
  if (class == CLASS_MBOARD) {
    return(polyMgen.b(nrow, ncol, rawData=m, degree=0))
  }
  if (class == CLASS_MCELSS) {
    return(polyMgen.c(nrow, ncol, rawData=m, degree=0))
  }
  if (class == CLASS_MDLIST) {
    p <- polynom::polynomial(1)
    ml <- lapply(as.list(m), function(x) x * p)
    return(polyMgen.d(nrow, ncol, rawData=ml))
  }
  stop('Unknonw polymatrix class')
}


pM2M <- function(pM)
{
  if (max(degree(pM)) != 0) {
    stop("The 'pM' is not a zero degree polynomial matrix")
  }
  return(const(pM))
}


# ----
# fine
