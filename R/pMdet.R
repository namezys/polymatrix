pMdet <- 
function(pm)
  { if(dim(pm)[1]!=dim(pm)[2]) stop("'pm' must be a square matrix")
    pd<-pMconvert(pm,"pMdlist")
    m<-dim(pd)[1]
    t<-m # before the first step: the perm.length; after: the actual permutation
    if(m==1) p<-pd$dlist[[1]][[1]]
      else 
        { p<-polynom::polynomial(0) # the determinant
          for(k in 1:factorial(m))
            { t<-permNext(t)
              p<-p+permSign(t)*pMprod(pMbas(pd,t,byrow=FALSE)) } }
    return(p)
   }


# ----
# fine
