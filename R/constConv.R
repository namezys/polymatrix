# ---
# conversion between the 'matrix' and 'pMatrix' representation of constant matrices matrix

M2pM <-
function(m,class=c("pMarray","pMbroad","pMcells","pMdlist"))  
 { osztaly <-class[1]
   dim<-dim(m)
   k<-dim[1]
   j<-dim[2]
   d<-0
   m<-as.numeric(m)
   pm<-
   switch( osztaly,
          "pMarray" =   pMgen.a(k,j,rawData=m,degree=d),
          "pMbroad" =   pMgen.b(k,j,rawData=m,degree=d),
          "pMcells" =   pMgen.c(k,j,rawData=m,degree=d),
          "pMdlist" = { p<-polynom::polynomial(1)
                        ml<-lapply(as.list(m),function(x) x*p)
                        pMgen.d(k,j,rawData=ml)}
          )
   return(pm) 
  }


pM2M <-
function(pM)  
 { if(max(degree(pM)))  stop("The 'pM' is not a zero degree polynomial matrix")
   const(pM)
 }


# ----
# fine