# -----
# Generators for pMatrix class polynomial matrices 

#  0. # pMgen          - call the generators

#  1. # pMgen.a       - pMatrix in pMarray form
#  2. # pMgen.b       - pMatrix in pMbroad form
#  3. # pMgen.c       - pMatrix in pMcells form
#  4. # pMgen.d       - pMatrix in pMdlist form

#  5. # pMgen.varma   - Generate a pMvarma class polynomial matrix pair

# ----------------------
#  0. # pMgen          - call the generators

pMgen <- 
function(k,j,rawData,symb,rand,degree,byrow,sm="pMarray",...) 
{ if(!substr(sm,3,3)%in%c("a","b","c","d")) 
    stop("the admissible storage methods are: 'pMarray','pMbroad','pMcells' and 'pMdlist'!")
  degree <- if(!missing("degree")) degree else 2
  k <- if(!missing("k")) k else 2
  j <- if(!missing("j")) j else 3
  symb <- if(!missing("symb")) symb else "x"
  rand <- if(!missing("rand")) rand else FALSE
  byrow  <- if(!missing("byrow")) byrow else  FALSE
  pM <- switch(substr(sm,3,3),
   "a"= { rawData <- if(!missing("rawData")) rawData else 1:(k*j*(max(degree)+1))
          pMgen.a(k=k,j=j,rawData=rawData,symb=symb,rand=rand,degree=degree,byrow=byrow)}, 
   "b"= { rawData <- if(!missing("rawData")) rawData else 1:(k*j*(degree+1))
          pMgen.b(k=k,j=j,rawData=rawData,symb=symb,rand=rand,degree=degree,byrow=byrow)}, 
   "c"= { rawData <- if(!missing("rawData")) rawData else 1:(k*j*(max(degree)+1))
          pMgen.c(k=k,j=j,rawData=rawData,symb=symb,rand=rand,degree=degree,byrow=byrow)},
   "d"= { rawData <- if(!missing("rawData")) rawData else list(polynom::polynomial(1:3))
          pMgen.d(k=k,j=j,rawData=rawData,symb=symb,rand=rand,degree=degree,byrow=byrow)})
  return(pM)	  
}


# ----------------------
#  1. # pMgen.a       - pMatrix in pMarray form

pMgen.a <- 
function(k = 2, j = 3, rawData = 1:(k * j * (max(degree) + 1)),
         symb = "x",rand = FALSE, degree = 2, byrow = FALSE) 
{
    if (missing("degree")) 
      degree <- matrix(sample(0:3, k * j, c(1, 2, 3, 2), replace = TRUE), k, j)
  degree <- if(!is.matrix(degree)) 
      matrix(degree,k,j) else degree
  d <- max(degree)
  n <- k * j + sum(degree)
  if(is.function(rand)) rawData <- rand(n)
  if(is.logical(rand)) if(rand) rawData <- rnorm(n)
  rawData <- cycFill(rawData,n)
  ip <- 1;op <- 1
  rawDataSupp <- rep(0,k*j*(d+1))
  for(ij in 1:j) for(ik in 1:k)  
    { rawDataSupp[op:(op+degree[ik,ij])] <- rawData[ip:(ip+degree[ik,ij])];
	  ip <- ip+degree[ik,ij]+1; op <- op+d+1}
  ci <- seq(1,by=d+1,length=k*j)
  const <- matrix(rawDataSupp[ci],k,j,
                      dimnames=list(paste0("x",1:k),paste0("y",1:j)),byrow=byrow)
  array <- if(d==0) NULL else
            array(as.vector(t(matrix(rawDataSupp[-ci],d))),dim=c(k,j,d),
               dimnames=list(paste0("x",1:k),paste0("y",1:j),
                             if(d==1) symb else c(symb,paste0(symb,"^",2:d))))
  pm <- list( dim=c(k,j),degree=degree, symb=symb, const=const, array=array)
  class(pm)<-c("pMarray","pMatrix")
  return(pm) }

# ----------------------
#  2. # pMgen.b       - pMatrix in pMbroad form

pMgen.b <- 
function(k=2,j=3,rawData=1:(k*j*(degree+1)),symb="x",rand=FALSE,degree=2,byrow=FALSE)   
{ pa<-pMgen.a(k=k,j=j,degree=degree,rawData=rawData,symb=symb,rand=rand)
  pb<-pMconvert(pa,"pMbroad")
  return(pb)
  }

# ----------------------
#  3. # pMgen.c       - pMatrix in pMcells form

pMgen.c <- 
function(k=2,j=3,rawData=1:(k*j*(max(degree)+1)),symb="x",rand=FALSE, degree = 2, byrow = FALSE)   
{ pa<-pMgen.a(k=k,j=j,degree=degree,rawData=rawData,symb=symb,rand=rand)
  pc<-pMconvert(pa,"pMcells")
  return(pc)
}
   
# ----------------------
#  4. # pMgen.d       - pMatrix in pMdlist form

pMgen.d <- 
function(k=2,j=3,rawData=list(polynomial(1:3)),symb="x",rand=FALSE,degree,byrow=FALSE)   
{ n <- k*j
  if(missing(degree)) 
      degree<-matrix(sample(0:3,k*j,c(1,2,3,2),replace=TRUE),k,j)
  if(!missing(degree)) if(is.null(degree))
      degree<-matrix(sample(0:3,k*j,c(1,2,3,2),replace=TRUE),k,j)
  degree <- if(!is.matrix(degree)) matrix(degree,k,j) else degree
  dgm<-if(is.logical(rand)) function() rgeom(1,.33)+1 else rand
  if(is.function(rand)) rand<-TRUE
  if(rand) 
   { rawData<-vector("list",n)
     deg<-as.numeric(if(byrow) t(degree) else degree)
      for(i1 in 1:n)
        { rd<-vector("numeric",deg[i1]+1)
		  for(i2 in 0:deg[i1]) rd[i2+1]<-dgm()
         rawData[[i1]] <- polynom::polynomial(rd) }}  
  m <- length(rawData)
  rawData_class_good<-vector("logical",m)
  for(i in 1:m) rawData_class_good[i]<-polynom::is.polynomial(rawData[[i]])
  if(!all(rawData_class_good)) stop("rawData class error!!!\n")
  rawData<-cycFill(rawData,n)
  
  dlist <- vector("list",k)
  if(!byrow)  
    for (i1 in 1:k) 
       dlist[[i1]]<-rawData[(1:j-1)*k+i1] # by col data
  if(byrow) 
    {
      for (i1 in 1:k) dlist[[i1]]            <-vector("list",j)
      for (i in 1:n-1) dlist[[i%/%j+1]][[i%%j+1]]<-rawData[[i+1]]
     } 
  
  d<-matrix(0,k,j); 
    for (i1 in 1:k) for(i2 in 1:j) d[i1,i2] <- degree(dlist[[i1]][[i2]])
  pd<-list(dim=c(k,j),degree=d,symb=symb,dlist=dlist)
  class(pd)<-c("pMdlist","pMatrix")
  return(pd) 
}


# ----------------------
#  5. # pMgen.varma       - Generate a pMvarma class polynomial matrix pair

pMgen.varma <- 
function(k=3,j=3,rawData=list(polynomial(1:3)),symb="x",rand=FALSE,degree=c(1,1), byrow = FALSE)   
{ 
  if(length(degree)!=2) stop("Use two degree parameters (p,q)!")
  AR <- if(!is.na(degree[1])) pMgen.d(k,j,rand=TRUE,degree=degree[1]) else NA
  MA <- if(!is.na(degree[2])) pMgen.d(k,j,rand=TRUE,degree=degree[2]) else NA
  varma<-list(degree=c(ar=degree[1],ma=degree[2]),AR=AR,MA=MA)
  class(varma)<-"pMvarma"
  return(varma) 
}



# -----
# fine  

