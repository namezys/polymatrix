# ----
# a diagonal polynomial matrix

diag<-
function(x,nrow=NULL,ncol=nrow,type=c("vector","matrix"),...)
  { switch(tail(class(x),1),
          "list"=diag.list(x,nrow=nrow,ncol=ncol),
          "polyMatrix"=diag.polyMatrix(x,nrow=nrow,ncol=ncol,type),
          "matrix"=base::diag(x),
           { if (is.null(nrow)&is.null(ncol)) base::diag(x)
               else if (is.null(nrow)) base::diag(x,ncol=ncol)
               else if (is.null(ncol)) base::diag(x,nrow=nrow)
               else base::diag(x,nrow=nrow,ncol=ncol) }
          )
  }

diag.list <- function(v, nrow=NULL, ncol=NULL)
{
  if (is.null(nrow)) {
    nrow<-length(v)
  }
  if (is.null(ncol)) {
    ncol<-nrow
  }
  pm <- polyMgen.d(nrow, ncol, rawData=list(polynom::polynomial(0)))
  m <- min(nrow, ncol)
  if (length(v) < m) {
    v <- cycFill(v,m)
  }
  for(i in 1:m) {
    pm$dlist[[i]][[i]] <- v[[i]]
    pm$degree[i, i] <- degree(v[[i]])
  }
  return(pm)
}

diag.polyMatrix <-
function(pm,nrow=dim(pm)[1],ncol=dim(pm)[2],type=c("list","polyMatrix"))
  { tipus<-substr(type[1],1,2)
    pd<-polyMconvert(pm,"polyMdlist")
    m<-min(dim(pd))
    v<-vector("list",m)
    for(i4 in 1:m) v[[i4]]<-pd$dlist[[i4]][[i4]]
    return( if(tipus=="pM") {diag(v,nrow,ncol)} else v)

   }

# ----
# fine
