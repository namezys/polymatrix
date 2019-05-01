# ---
# conversion between the 'matrix' and 'polyMatrix' representation of constant matrices matrix

M2pM <-
function(m,class=c("polyMarray","polyMbroad","polyMcells","polyMdlist"))  
 { osztaly <-class[1]
   dim<-dim(m)
   k<-dim[1]
   j<-dim[2]
   d<-0
   m<-as.numeric(m)
   pm<-
   switch( osztaly,
          "polyMarray" =   polyMgen.a(k,j,rawData=m,degree=d),
          "polyMbroad" =   polyMgen.b(k,j,rawData=m,degree=d),
          "polyMcells" =   polyMgen.c(k,j,rawData=m,degree=d),
          "polyMdlist" = { p<-polynom::polynomial(1)
                        ml<-lapply(as.list(m),function(x) x*p)
                        polyMgen.d(k,j,rawData=ml)}
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
