# ----
# the matrix trace for 'matrix' and 'pMatrix' class objects

tr <-
function(M) UseMethod("tr")

tr.matrix <-
function(M)  
  { if(nrow(M)>ncol(M))M<-t(M);
    n<-min(dim(M))
    numic<-sum(as.numeric(M)[seq(1,by=n+1,l=n)])
    return(numic)}

tr.pMatrix <-
function(M)  
  { pd<-pMconvert(M,"pMdlist")
    ponom<-polynomial(0)
    if(dim(pd)[1]>dim(pd)[2]) pd<-t(pd);
    m<-min(dim(pd))
    for(k in 1:m)
    ponom<-ponom+pd$dlist[[k]][[k]]
    return(ponom) }

   
# ----
# fine