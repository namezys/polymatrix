# ----
# a diagonal polynomial matrix

diag <- function(x, nrow=NULL, ncol=NULL, names=NULL, type=CLASS_LIST, ...) {
  UseMethod('diag')
}

diag.default <- function(x, ...)
{
  return(base::diag(x, ...));
}

diag.list <- function(x, nrow=NULL, ncol=NULL, names=NULL, type=CLASS_LIST, ...)
{
  if (is.null(nrow))  {
    nrow<-length(x)
  }
  if (is.null(ncol)) {
    ncol<-nrow
  }
  pm <- polyMgen.d(nrow, ncol, rawData=list(polynom::polynomial(0)))
  m <- min(nrow, ncol)
  if (length(x) < m) {
    x <- cycFill(x, m)
  }
  for(i in 1:m) {
    pm$dlist[[i]][[i]] <- x[[i]]
    pm$degree[i, i] <- degree(x[[i]])
  }
  return(pm)
}

diag.polyMatrix <- function(x, nrow=NULL, ncol=NULL, names=NULL, type=CLASS_LIST, ...)
{
  pd <- polyMconvert.dlist(x)
  if (is.null(nrow)) {
    nrow <- dim(x)[1]
  }
  if (is.null(ncol)) {
    ncol <- dim(x)[2]
  }
  m <- min(dim(pd))
  v <- vector("list", m)
  for(i in 1:m) {
    v[[i]] <- pd$dlist[[i]][[i]]
  }
  if (type == CLASS_MATRIX) {
      return(diag(v, nrow, ncol));
  }
  return(v);
}

# ----
# fine
