#  1. # pMconvert - representation converter for "pMatrix" class objects
#
#    + pMconvert.a2b  array => broad
#    + pMconvert.a2c  array => cells
#    + pMconvert.a2d  array => dlist
#    + pMconvert.b2a  broad => array
#    + pMconvert.b2c  broad => cells
#    + pMconvert.b2d  broad => dlist
#    + pMconvert.c2a  cells => array
#    + pMconvert.c2b  cells => broad
#    + pMconvert.c2d  cells => dlist 
#    + pMconvert.d2a  dlist => array
#    + pMconvert.d2b  dlist => broad
#    + pMconvert.d2c  dlist => cells
#
#  2. # MTS2pM     - MTS model list to "pMatrix" converter

# -----
#  1. # pMconvert - representation converter for "pMatrix" class objects
#
pMconvert <- 
function(pM,newclass)
 {
  if((length(class(pM))==2)&(class(pM)[2]=="pMatrix"))
   {
    old.c <- substr(class(pM)[1],3,3)
    new.c <- substr(newclass[1],3,3)
    pN <-
      if(old.c==new.c) pM 
        else eval(parse(text=paste0("pMconvert.",old.c,"2",new.c,"(pM)")))
	  
	return(pN)  
	}
	
  }

# -------------------------------------------------------------------
# polynom matrix dim=c(k,j), max degree=d

# pMarray structure
#    $ dim    : c(k,j)
#    $ degree : d
#    $ symb   : 'z' 
#    $ const  : matrix dim=dim
#    $ coefs  : array dim=c(dim,d)

# pMbroad structure
#    $ dim    : c(k,j)
#    $ degree : d 
#    $ symb   : 'z' 
#    $ broad  : matrix dim=dim[1] x dim[2]*(degree+1)

# pMcells structure
#    $ dim    : c(k,j)
#    $ degree : d
#    $ symb   : 'z' 
#    $ cells  : list of matrices dim==dim, length==degree+1

# pMdlist structure
#    $ dim    : c(k,j)
#    $ degree : d 
#    $ symb   : 'z' 
#    $ dlist  : list of lists length==k length of lists==j

# -------------------------------------------------------------------
pMconvert.a2b <- # array => broad: one matrix of dim=c(k,j*(d+1))
function(pa)
{
    dim<-dim(pa)
    k<-dim[1]
    j<-dim[2]
    dmax<-degree(pa)
    broad<-matrix(NA,k,j*(dmax+1))
    broad[,1:j]<-const(pa)
    if(dmax)
      for(i in 1:dmax)
        broad[,i*j+1:j]<-pa$array[,,i]
    pb<-list(dim=dim,degree=degree(pa,"m"),symb=pa$symb,broad=broad)
    class(pb) <- c("pMbroad","pMatrix") 
    return(pb)
}

# ----
pMconvert.a2c <- # array => cells: list of matrices dim=c(k,j), length=d+1
function(pa)
{
    dim<-dim(pa)
    cells<-list(const(pa))
    a.coefs<-pa$array    
    if(degree(pa))
      for(i in 1:dim(a.coefs)[3]) 
        cells <- c(cells,list(a.coefs[,,i]))
    pc<-list(dim=dim,degree=degree(pa,"m"),symb=pa$symb,cells=cells)
    class(pc)<-c("pMcells","pMatrix")
    return(pc)
}

# ----
pMconvert.a2d <- # array => dlist: list of lists length=k, lengnth of lists=j
function(pa)
  {
    dim<-dim(pa)
    k<-dim(pa)[1]
    j<-dim(pa)[2]
    const<-pa$const
    a.coefs<-pa$array
    dlist<-vector("list", k)
    for (i1 in 1:k) dlist[[i1]]<-vector("list", j)
    for (i1 in 1:k) for (i2 in 1:j) 
      dlist[[i1]][[i2]] <- polynomial(c(const[i1,i2],a.coefs[i1,i2,]))
    pd<-list(dim=dim,degree=degree(pa,"m"),symb=pa$symb,dlist=dlist)
    class(pd) <- c("pMdlist","pMatrix") 
    return(pd) 
  }


# ----
pMconvert.b2a <- # broad=>array:$dim=c(k,j), $degree=d, $const=matrix, $coefs=array(j,k,dmax)
function(pb)
{
    dim<-dim(pb)
    d<-degree(pb)
    const<-const(pb)
    coefs<-coefs(pb)
    a.coefs<-if(d) array(NA,dim=c(dim,d)) else NULL
    if(d) for (i3 in 1:d) a.coefs[,,i3]<-coefs[[i3]]
    pa<-list(dim=dim,degree=degree(pb,"m"),symb=pb$symb,const=const,array=a.coefs)
    class(pa)<-c("pMarray","pMatrix")
    return(pa)
}

# ----
pMconvert.b2c <- # broad => cells: list of matrices dim=c(k,j), length=dmax+1
function(pb)
{
    dim<-dim(pb)
    d<-degree(pb)
    k<-dim[1]
    j<-dim[2]

    cells<-list(const(pb))
    if(d) 
      for(i in 1:d) 
        cells <- if(i==1) 
                    list(cells[[1]],pb$broad[,i*j+1:j]) else c(cells,list(pb$broad[,i*j+1:j]))
    pc<-list(dim=dim,degree=degree(pb,"m"),symb=pb$symb,cells=cells)
    class(pc)<-c("pMcells","pMatrix")
    return(pc)
}

# ----
pMconvert.b2d <- # broad => dlist
function(pb)
{   dim<-dim(pb)
    k<-dim[1]
    j<-dim[2]
    d1<-degree(pb)+1
    broad<-pb$broad
    dlist<-vector("list", k)
    for(i1 in 1:k) dlist[[i1]]<-vector("list", j)
    for(i1 in 1:k) for(i2 in 1:j)  
      dlist[[i1]][[i2]] <- polynomial(broad[i1,((1:d1)-1)*j+i2])
    pd<-list(dim=dim,degree=degree(pb,"m"),symb=pb$symb,dlist=dlist)
    class(pd) <- c("pMdlist","pMatrix") 
    return(pd) 
}

# ----
pMconvert.c2a <- # cells => array
function(pc)
{   dim<-dim(pc)
    d<-degree(pc)
    k<-dim[1]
    j<-dim[2]
    const<-pc$cells[[1]]
    coefs<-array(0,c(k,j,d))
    if(d) for(i in 1:d) coefs[,,i]<-pc$cells[[i+1]]
    pa<-list(dim=dim,degree=degree(pc,"m"),symb=pc$symb,const=const,array=coefs)
    class(pa)<-c("pMarray","pMatrix","x")
    return(pa)
}

# ----
pMconvert.c2b <- # cells => broad
function(pc)
{
    dim<-dim(pc)
    d<-degree(pc)
    k<-dim[1]
    j<-dim[2]
    broad<-matrix(NA,k,j*(d+1))
    for(i3 in 1:(d+1)) broad[,(i3-1)*j+1:j]<-pc$cells[[i3]]
    pb<-list(dim=dim,degree=degree(pc,"m"),symb=pc$symb,broad=broad)
    class(pb) <- c("pMbroad","pMatrix") 
    return(pb)
}

# ----
pMconvert.c2d <- # cells => dlist
function(pc)
{
    dim<-dim(pc)
    d<-degree(pc)
    k<-dim[1]
    j<-dim[2]
    cells<-pc$cells

    dlist<-vector("list", k)
    for(i1 in 1:k) dlist[[i1]]<-vector("list", j)

    for(i1 in 1:k) for(i2 in 1:j)
      { pn<-NULL; for(i3 in 0:d)  pn<-c(pn,cells[[i3+1]][i1,i2])  
        dlist[[i1]][[i2]] <- polynomial(pn)}

    pd<-list(dim=dim,degree=degree(pc,"m"),symb=pc$symb,dlist=dlist)
    class(pd) <- c("pMdlist","pMatrix") 
    return(pd) 
}

# ----
pMconvert.d2a <- # dlist => array
function(pd)
{
    dm<-dim(pd)
    k<-dm[1]
    j<-dm[2]
    deg<-matrix(NA,k,j)
    d<-degree(pd)
    coef0<-matrix(NA,k,j) 
    for(i1 in 1:dm[1]) for(i2 in 1:dm[2]) 
         coef0[i1,i2] <- const(pd$dlist[[i1]][[i2]])
    coef1<-array(0,c(k,j,d)) 
    for(i1 in 1:k) for(i2 in 1:j) 
      if(length(coefs(pd$dlist[[i1]][[i2]])))  
         coef1[i1,i2,] <- head(c(coefs(pd$dlist[[i1]][[i2]])[-1],rep(0,d)),d)
    pa <- list( dim=dm,
                degree=degree(pd,"m"),
				symb=pd$symb,
                const=coef0,
                array=if(d==0) NULL else coef1)
    class(pa)<-c("pMarray","pMatrix")
    return(pa)
}

# ----
pMconvert.d2b <- # dlist => broad
function(pd)
{   dim<-dim(pd)
    k<-dim[1]
    j<-dim[2]
    d<-degree(pd)
    coefs<-coefs(pd)
    broad<-matrix(NA,k,j*(d+1))
    for(i in 1:(d+1))
      broad[,(i-1)*j+1:j]<-coefs[[i]]
    pb<-list(dim=dim,degree=degree(pd,"m"),symb=pd$symb,broad=broad)
    class(pb) <- c("pMbroad","pMatrix") 
    return(pb)


    class(pb)<-c("pMbroad","pMatrix")
    return(pb)
}

# ----
pMconvert.d2c <- # dlist => cells: list of matrices dim=c(k,j), length=d+1
function(pd)
{   dim<-dim(pd)
    d<-degree(pd)
    k<-dim[1]
    j<-dim[2]
    a<-array(0,dim=c(k,j,d+1))
    for(i1 in 1:k)for(i2 in 1:j)
      { ap<-pd$dlist[[i1]][[i2]]
        a[i1,i2,1:(degree(ap)+1)]<-coefs(ap) }

    cells<-list(a[,,1])
    if(d)
      for(i in 1:d) 
        cells <- if(i==1) 
                    list(cells[[1]],a[,,2]) 
                   else c(cells,list(a[,,i+1]))
    pc<-list(dim=dim,degree=degree(pd,"m"),symb=pd$symb,cells=cells)
    class(pc)<-c("pMcells","pMatrix")
    return(pc)
}

	
# -----
#  2. # MTS2pM     - MTS model list to "pMatrix" converter
#
	
MTS2pM <-
function(M)
    {
      degree <- c(AR=M$ARorder,MA=M$MAorder)
      k <- dim(M$data)[2]
      ARdim <- MAdim <- c(k,k)
      ARbroad <- cbind(diag(k),-M$Phi)
      MAbroad <- cbind(diag(k),-M$Theta)
      ARdegree <- matrix(degree["AR"],k,k) 
      MAdegree <- matrix(degree["MA"],k,k)
      AR <- list(dim=ARdim,degree=ARdegree,symb="x",broad=ARbroad)
      class(AR) <- c("pMbroad","pMatrix")
      MA <- list(dim=MAdim,degree=MAdegree,symb="x",broad=MAbroad)
      class(MA) <- c("pMbroad","pMatrix")
      obj <- (list(degree=degree,AR=AR,MA=MA,Cons=M$Ph0,Sigma=M$Sigma))
      class(obj) <- "pMvarma"
      return(obj)
    }
	
# ----
# fine
