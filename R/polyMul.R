# multiplication of two polynomial by matrix multiplication
# -----------------

polyMul <-
function(p,q)
 { # p<-5:1;q<-4:1;
   p<-rev(p);n<-length(p)
   q<-rev(q);m<-length(q)
   k<-n+m-1
   raw<-c(p,rep(c(rep(0,n+m-1),p),n+m-2))
   P<-matrix(raw,2*n+m-2)[1:(n+m-1),]
   Q<-matrix(c(q,rep(0,n-1)),ncol=1)
   P
   Q
   return(rev(P%*%Q)) 
 }

# ----
# fine
