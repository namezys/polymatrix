# ----
# a diagonal polynomial matrix

diag<-
function(x,nrow=NULL,ncol=nrow,type=c("vector","matrix"),...)
  { switch(tail(class(x),1),
          "list"=diag.list(x,k=nrow,j=ncol),
          "polyMatrix"=diag.polyMatrix(x,k=nrow,j=ncol,type),
          "matrix"=base::diag(x),
           { if (is.null(nrow)&is.null(ncol)) base::diag(x)
               else if (is.null(nrow)) base::diag(x,ncol=ncol)
               else if (is.null(ncol)) base::diag(x,nrow=nrow)
               else base::diag(x,nrow=nrow,ncol=ncol) }
          )
  }

diag.list <-
function(v,k=length(v),j=k) 
  { if(is.null(k)) k<-length(v)
    if(is.null(j)) j<-k
    pm <- polyMgen.d(k=k,j=j,rawData=list(polynom::polynomial(0)))
    m <- min(k,j)
    if(length(v) < m) v<-cycFill(v,m)
    for(i4 in 1:m) 
      { pm$dlist[[i4]][[i4]] <- v[[i4]]
        pm$degree[i4,i4] <- degree(v[[i4]])
       }
    return(pm)
   }

diag.polyMatrix <-
function(pm,k=dim(pm)[1],j=dim(pm)[2],type=c("list","polyMatrix")) 
  { tipus<-substr(type[1],1,2)
    pd<-polyMconvert(pm,"polyMdlist") 
    m<-min(dim(pd))
    v<-vector("list",m)
    for(i4 in 1:m) v[[i4]]<-pd$dlist[[i4]][[i4]]
    return( if(tipus=="pM") {diag(v,k,j)} else v)
              
   }
   
# ----
# fine
