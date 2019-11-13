# -----------------
# the characteristic polynomial of a matrix or polynomial matrix

charpn <-
function(M) UseMethod("charpn")

charpn.matrix <-
function(M)  
   { n<-dim(M)[1]
     pm <- polyMgen.a(dim(M)[1],dim(M)[2],rawData=as.numeric(M),degree=0)
     if (!n==dim(pm)[2]) stop(" 'M' is not a square matrix")
     xI<-pMdiag(ch2pn("x"),n)
     chpn<-pMdet(xI-pm)
     return(chpn) }

charpn.polyMatrix <-
function(M)  
  { pM<-M 
    vecszor <- function(v,i) 
      { # v list of polynomials, i diagonal subset indicator
        n<-length(v$dlist) 
        if(length(i)!=n) 
              stop("The length of the polynomial list is not equal by the length of the subset indicator!")

         dia<-(1:n)[i]  # the index of diagonal terms with x
         out<-(1:n)[!i] # the index of out-diagonal terms without x

         k<-sum(i)# size of the diagonal subset
         prod<-vector("list",k+1)
         for(j in 0:k) prod[[j+1]]<-polynom::polynomial(0)            
         names(prod)<-paste0("[t^",0:k,"]");names(prod)[1]<-"[1]";

         if(k==0) # no x-term
           { prod[[1]]<-prod[[1]]+pMprod(v) } 
          else
           { xssi<-k # init the x-term subset indicator
             for(j in 1:2^k)
              { xssi<-ssetNext(xssi)
                out.ss<-c(out,dia[xssi==0])
                prod[[sum(xssi)+1]] <- prod[[sum(xssi)+1]]+
                     if(length(out.ss)==0) polynom::polynomial(1) 
                       else pMprod(pMbas(v,out.ss,byrow=TRUE))}
            }
         return(prod)}

    n<-dim(pM)[1]
      pd<- -polyMconvert(M,"polyMdlist") # minus !!! mert(lambdaI-polyMatrix) kell

      pcch <- vector("list",n+1) # list of polynomial coefs of the char.polynom
      for(i in 1:(n+1)) pcch[[i]]<-polynom::polynomial(0)
      names(pcch)<-paste0("[M^",0:n,"]");# names(pcch)[1]<-"[1]";

      t<-n # the perm.length; after: the actual permutation
      if(n==1) {pcch[[1]] <- pd$dlist[[1]][[1]];pcch[[2]]<-polynom::polynomial(1)}
        else 
          { 
            for(i in 1:factorial(n))
              { t<-permNext(t)                # the permutation
                s<-permSign(t)                # the permutations sign 
                Lis<-pMbas(pd,t,byrow=TRUE)   # list of selected elements  
                Dia<-1:n==t                   # the diagonal element indicator
                pr<-vecszor(Lis,Dia)          # form the polynom product of the selected elements
                for(j in 1:length(pr)) pcch[[j]]<-pcch[[j]]+s*pr[[j]] # collect the result
              }  
          }
    class(pcch)<-"charpn"            
    return(pcch)  
   }

# ----
# fine
