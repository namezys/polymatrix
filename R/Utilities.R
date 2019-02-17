# -----------------
# Matrix utilities
#
#  1. # pMcol     - a column of a polynomial matrix
#  2. # pMrow     - a row of a polynomial matrix 
#  3. # pMbas     - bastion vector, the permutation: 'ki' (calc of det)
#  4. # pMsub     - sub-matrix of a polynomial-matrix
#  5. # pMprod    - product of the elements of a polynomial matrix
#  6. # pVsk      - scalar product of two polynomial vector
#  7. # pMsgn     - sign-change of a polynomial matrix
#  8. # ssetNext  - next subset of a set
#  9. # permNext  - lexicographical next permutation
# 10. # permSign  - the sign of a permutation
# 11. # colMax    - column maximum values of a matrix
#     # rowMax    - row maximum values of a matrix
# 12. # colMin    - column minimum values of a matrix
#     # rowMin    - row minimum values of a matrix
# 13. # cycFill   - cyclic fill by a given material
# 14. # pMdiag    - diagonal polynomial matrix
# --- # -----------



# -----------------
#  1. # pMcol     - a column of a polynomial matrix

pMcol <-
function(pm,which=1)
  { if(class(pm)[2]!="pMatrix") stop("The 'pm' parameter must be a 'pMatrix' object") 
    pm<-pMconvert(pm,"pMdlist")
    k<-dim(pm)[1]
    dlist<-vector("list",k)
    degree<-matrix(NA,k,1)
    for (i in 1:k) 
      { dlist[[i]]<-list(pm$dlist[[i]][[which]])
        degree[i,1]<-degree(dlist[[i]][[1]])    }
    return(
    structure(list(dim = c(k,1),
                   degree = degree,
                   symb = "x",   
                   dlist = dlist),
              class = c("pMdlist","pMatrix")))
   }


# -----------------
#  2. # pMrow     - a row of a polynomial matrix 

pMrow <-
function(pm,which=1)
  { if(class(pm)[2]!="pMatrix") stop("The 'pm' parameter must be a 'pMatrix' object") 
    pd<-pMconvert(pm,"pMdlist")
    j<-dim(pd)[2]
    dlist<-vector("list",1)
    dlist[[1]]<-vector("list",j)
    degree<-matrix(NA,1,j)
    for (i in 1:j) 
      { dlist[[1]][[i]]<-pd$dlist[[which]][[i]]
        degree[1,i]<-degree(dlist[[1]][[i]])    }
    return(
    structure(list(dim = c(1,j),
                   degree = degree,
                   symb = "x",   
                   dlist = dlist),
              class = c("pMdlist","pMatrix")))
   }

# -----------------
#  3. # pMbas     - bastion vector, the permutation: 'ki'

pMbas <-   # bastion vector of a matrix or a subvector by the index: 'ki'
function(pm,ki,byrow)
  { n <- length(ki)
    if(byrow & n>dim(pm)[1]|(!byrow & n>dim(pm)[2])) stop("Index vector too long!")
    pd <- pMconvert(pm,"pMdlist")
    if(dim(pd)[1]==1) pd<-t(pd) # if vector let it a column
    if(dim(pd)[2]==1)
          {v<-pd$dlist[ki]
           degree<-degree(pm,"m")[ki,1,drop=FALSE]}          
     else {
            v <- vector("list",n)
            if(byrow)
              for (k in 1:n) v[[k]][1] <- list(pd$dlist[[k]][[ki[k]]])
             else
              for (k in 1:n) v[[k]][1] <- list(pd$dlist[[ki[k]]][[k]])
            degree <- vector("numeric",n)
            for (k in 1:n) degree[k] <- degree(v[[k]][[1]])
           }
    return(
    structure(list(dim = c(n,1),
                   degree = matrix(degree,n,1),
                   symb = "x",   
                   dlist = v),
              class = c("pMdlist","pMatrix")))
   }


# -----------------
#  4. # pMsub     - sub-matrix of a polynomial-matrix

pMsub <-    
function(pm,i,j=i) 
  { pd<-pMconvert(pm,"pMdlist")
    
    if(length(i)>0) 
      { i<-i[i!=0];i<-i[i<=pd$dim[1]] 
	    if(length(i)>0) {
        if((!all(i<0))&(!all(i>0))) stop("The row selection indices have different signs!")
        pd$dlist<-pd$dlist[i] # retain or delet the i.th row(s)
        pd$dim[1] <- if(i[1]<0) pd$dim[1]-length(i) else length(i)  } }
    if(length(j)>0) 
      { j<-j[j!=0];j<-j[j<=pd$dim[2]] 
	    if(length(j)>0) {
        if((!all(j<0))&(!all(j>0))) stop("The column selection indices have different signs!")
        for(k in 1:length(pd$dlist)) 
          pd$dlist[[k]]<-pd$dlist[[k]][j]# retain or delet  the j.th column from the k.th row
        pd$dim[2] <- if(j[1]<0) pd$dim[2]-length(j) else length(j)  } }
    pd$degree<-pd$degree[if(length(i)) i else 1:pd$dim[1],if(length(j)) j else 1:pd$dim[2],drop=FALSE] 
    pm<-pMconvert(pd,class(pm)[1])		
    return(pm) 
   }
  
# -----------------
#  5. # pMprod    - a product of the elements of a polynomial vector

pMprod <-
function(pm)
  { 
    pm <- pMconvert(pm,"pMdlist")
    if(min(dim(pm)) != 1) stop("The input must be a vector!")
    m <- max(dim(pm))
    pr <- polynom::polynomial(1)
    for (k in 1:m) 
      pr <- pr*(if(m==dim(pm)[1]) pm$dlist[[k]][[1]] else pm$dlist[[1]][[k]])
    return(pr)
   }


# -----------------
#  6. # pVsk      - scalar product of two polynomial vectors

pVsk <-
function(pMx,pMy=NULL)
  { if(is.null(pMy)) pMy <- pMx
    if(max(min(dim(pMx)),min(dim(pMy)))!=1)
       stop("The scalar product works only for two vector 'pMatrix' object!")
    if(max(dim(pMx))!=max(dim(pMy)))
       stop("The scalar product works only for two equal length vectors!")
    m<-max(dim(pMx))  
    pdx <- pMconvert(pMx,"pMdlist")    
    pdy <- pMconvert(pMy,"pMdlist")    
    if(m==dim(pdx)[2]) pdx <- t(pdx)
    if(m==dim(pdy)[2]) pdy <- t(pdy)
    p<-polynom::polynomial(0)
    for (i in 1:m) 
       p<-p+pdx$dlist[[i]][[1]]*pdy$dlist[[i]][[1]]
       
    return(p)
   }


# -----------------
#  7. # pMsgn     - sign-change of a polynomial matrix

pMsgn <-   # change of the sign
function(pm) 
  {   mpm <- switch(class(pm)[1],
             pMarray = { pm$const<- -pm$const; pm$array<- -pm$array; pm },
             pMbroad = { pm$broad<- -pm$broad; pm },
             pMcells = { for(i3 in 0:degree(pm)) pm$cells[[i3+1]]<- -pm$cells[[i3+1]]; pm },
             pMdlist = { for(i1 in 1:dim(pm)[1]) 
                            for(i2 in 1:dim(pm)[2]) 
                              pm$dlist[[i1]][[i2]] <- -pm$dlist[[i1]][[i2]]
                         pm },
              stop("A not regular 'pMatrix' class object!"))    
      return(mpm)
   }


# -----------------
#  8. # ssetNext  - next subset of a set

ssetNext<-
function(ss)
 { m <- length(ss)
   if(m==1) 
     return(if(ss==0) m else rep(0,ss)) 
   if (all(ss!=0)) return(m)
    else 
       { k<-tail(which(ss==0),1)
         ss[k]<-1
         if(k<m) ss[(k+1):m]<-0 
         return(ss) }
   }


# -----------------
#  9. # permNext  - lexicographical next permutation

permNext <-
function(r)
 { if(length(r)==1) return(1:r)
   if(!all(sort(r)==1:length(r))) stop("The given 'r' is not a permutation")
   p<-tail(which(diff(r)>0),1)
   if(length(p)==0) return(length(r))
   a<-r[p]
   b<-r[(p+1):length(r)]
   c<-min(b[b>a])
   b<-b[b!=c]# b\c
   r[p]<-c
   r[(p+1):length(r)]<-sort(c(a,b))
   return(r)
  }

  
# -----------------
# 10. # permSign  - the sign of a permutation

permSign <-
function(r)
 { if(!all(sort(r)==1:length(r))) stop("The given 'r' is not a permutation")
   n<-length(r)
   s<-1
   for(j in (n-1):1)
    for(k in 1:j) 
     if(r[k]>r[k+1]) 
       { r[k+0:1]<-r[k+1:0]
         s<- -s }
   return(s)
  }


# -----------------
# 11. # colMax    - column maximum values of a matrix
      # rowMax    - row maximum values of a matrix

colMax <-
function(m)  
 { if(class(m)!="matrix") 
      stop("The 'colMax' function require a 'matrix' class argument!")
   k<-dim(m)[2]
   v<-vector("numeric",k)
   for(i2 in 1:k) v[i2]<-max(m[,i2])
   return(v)
  }

rowMax <-
function(m)  
 { if(class(m)!="matrix") 
      stop("The 'rowMax' function require a 'matrix' class argument!")
   k<-dim(m)[1]
   v<-vector("numeric",k)
   for(i1 in 1:k) v[i1]<-max(m[i1,])
   return(v)
  }

# -----------------
# 12. # colMin    - column minimum values of a matrix
      # rowMin    - row minimum values of a matrix

colMin <-
function(m)  
 { if(class(m)!="matrix") 
      stop("The 'colMin' function require a 'matrix' class argument!")
   k<-dim(m)[2]
   v<-vector("numeric",k)
   for(i2 in 1:k) v[i2]<-min(m[,i2])
   return(v)
  }

rowMin <-
function(m)  
 { if(class(m)!="matrix") 
      stop("The 'rowMin' function require a 'matrix' class argument!")
   k<-dim(m)[1]
   v<-vector("numeric",k)
   for(i1 in 1:k) v[i1]<-min(m[i1,])
   return(v)
  }
 
# -----------------
# 13. # cycFill   - cyclic fill a vector//list of given length 

cycFill <-
function(u,m)
  { n<-length(u)
    if(m!=n) 
      { v<- if(class(u)=="list") list() else NULL
        if(m%/%n>=1) for(i in 1:(m%/%n)) v[(i-1)*n+(1:n)]<-u[1:n]
        if(m%%n) v[n*m%/%n+(1:(m%%n))]<-u[1:(m%%n)]
      } else
      v<-u
   return(v)
  }


# -----------------
# 14. # pMdiag    - diagonal polynomial matrix

pMdiag <-
function(p,k,symb="x")
  { 
    mclass<-if (class(p)=="polynomial") "OK" else 
     if (class(p)=="list") if(all(sapply(p,class)=="polynomial")) "OK" else "BAD" else "BAD"
    if(mclass=="BAD") stop("The input not a 'polynomial' or a 'list' class object of 'polynomial's!")
	if(length(k)>2|(length(k)==2&(k[1]!=k[2]))) stop("We for only square matrices!")
	if(length(k)==2) k<-k[1]
    p <- if (class(p)=="polynomial") list(p) else p
    K<-length(p)
    rD <- list()
    if(k%/%K>0) for(i in 1:(k%/%K)) rD <- c(rD,p)
    if(k%%K!=0) rD <- c(rD,p[1:(k%%K)])
    rawData<-rD[1]
    if(k>1) 
      for(i1 in 2:k)
        { for(i2 in 1:k)
            rawData <- c(rawData,list(ch2pn("0")))
          rawData <- c(rawData,rD[i1])  	    }
    return(pMgen.d(k,k,rawData=rawData,symb=symb))
  }

  
# -----------------
# fine

