#  1. # polyMconvert - [abcd] representation converter for "polyMatrix" class objects
#    + polyMconvert.a2b  array => broad
#    + polyMconvert.a2c  array => cells
#    + polyMconvert.a2d  array => dlist
#    + polyMconvert.b2a  broad => array
#    + polyMconvert.b2c  broad => cells
#    + polyMconvert.b2d  broad => dlist
#    + polyMconvert.c2a  cells => array
#    + polyMconvert.c2b  cells => broad
#    + polyMconvert.c2d  cells => dlist
#    + polyMconvert.d2a  dlist => array
#    + polyMconvert.d2b  dlist => broad
#    + polyMconvert.d2c  dlist => cells
#
#  2. # converters between "character" and "polynomial"
#    + pn2ch      - "polynomial" to "char" converter
#    + ch2pn      - "char" to "polynomial" converter
#
#  3. # converters between MTS package model list and "polyMatrix"
#    + MTS2pM     - MTS model list to "polyMatrix" converter

# -----
#  1. # polyMconvert - [abcd] representation converter for "polyMatrix" class objects
#
polyMconvert <- function(pm, newclass)
{
  if (!is.polyMatrix(pm)) {
    stop("Expect a 'polyMatrix' object")
  }

  old.c <- substr(class(pm)[1],6,6)
  new.c <- substr(newclass[1],6,6)
  pN <- if(old.c==new.c) pm
        else eval(parse(text=paste0("polyMconvert.",old.c,"2",new.c,"(pm)")))

	return(pN)
}

polyMconvert.dlist <- function(pm)
{
  return(polyMconvert(pm, CLASS_MDLIST))
}

# -------------------------------------------------------------------
# polynom matrix dim=c(k,j), max degree=d

# polyMarray structure
#    $ dim    : c(k,j)
#    $ degree : d
#    $ symb   : 'z'
#    $ const  : matrix dim=dim
#    $ coefs  : array dim=c(dim,d)

# polyMbroad structure
#    $ dim    : c(k,j)
#    $ degree : d
#    $ symb   : 'z'
#    $ broad  : matrix dim=dim[1] x dim[2]*(degree+1)

# polyMcells structure
#    $ dim    : c(k,j)
#    $ degree : d
#    $ symb   : 'z'
#    $ cells  : list of matrices dim==dim, length==degree+1

# polyMdlist structure
#    $ dim    : c(k,j)
#    $ degree : d
#    $ symb   : 'z'
#    $ dlist  : list of lists length==k length of lists==j

# -------------------------------------------------------------------
polyMconvert.a2b <- # array => broad: one matrix of dim=c(k,j*(d+1))
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
    pb<-list(dim=dim,degree=degree_matrix(pa),symb=pa$symb,broad=broad)
    class(pb) <- c("polyMbroad","polyMatrix")
    return(pb)
}

# ----
polyMconvert.a2c <- # array => cells: list of matrices dim=c(k,j), length=d+1
function(pa)
{
    dim<-dim(pa)
    cells<-list(const(pa))
    a.coefs<-pa$array
    if(degree(pa))
      for(i in 1:dim(a.coefs)[3])
        cells <- c(cells,list(a.coefs[,,i]))
    pc<-list(dim=dim,degree=degree_matrix(pa),symb=pa$symb,cells=cells)
    class(pc)<-c("polyMcells","polyMatrix")
    return(pc)
}

# ----
polyMconvert.a2d <- # array => dlist: list of lists length=k, lengnth of lists=j
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
      dlist[[i1]][[i2]] <- polynom::polynomial(c(const[i1,i2],a.coefs[i1,i2,]))
    pd<-list(dim=dim,degree=degree_matrix(pa),symb=pa$symb,dlist=dlist)
    class(pd) <- c("polyMdlist","polyMatrix")
    return(pd)
  }


# ----
polyMconvert.b2a <- # broad=>array:$dim=c(k,j), $degree=d, $const=matrix, $coefs=array(j,k,dmax)
function(pb)
{
    dim<-dim(pb)
    d<-degree(pb)
    const<-const(pb)
    coefs<-coefs(pb)
    a.coefs<-if(d) array(NA,dim=c(dim,d)) else NULL
    if(d) for (i3 in 1:d) a.coefs[,,i3]<-coefs[[i3]]
    pa<-list(dim=dim,degree=degree_matrix(pb),symb=pb$symb,const=const,array=a.coefs)
    class(pa)<-c("polyMarray","polyMatrix")
    return(pa)
}

# ----
polyMconvert.b2c <- # broad => cells: list of matrices dim=c(k,j), length=dmax+1
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
    pc<-list(dim=dim,degree=degree_matrix(pb),symb=pb$symb,cells=cells)
    class(pc)<-c("polyMcells","polyMatrix")
    return(pc)
}

# ----
polyMconvert.b2d <- # broad => dlist
function(pb)
{   dim<-dim(pb)
    k<-dim[1]
    j<-dim[2]
    d1<-degree(pb)+1
    broad<-pb$broad
    dlist<-vector("list", k)
    for(i1 in 1:k) dlist[[i1]]<-vector("list", j)
    for(i1 in 1:k) for(i2 in 1:j)
      dlist[[i1]][[i2]] <- polynom::polynomial(broad[i1,((1:d1)-1)*j+i2])
    pd<-list(dim=dim,degree=degree_matrix(pb),symb=pb$symb,dlist=dlist)
    class(pd) <- c("polyMdlist","polyMatrix")
    return(pd)
}

# ----
polyMconvert.c2a <- # cells => array
function(pc)
{   dim<-dim(pc)
    d<-degree(pc)
    k<-dim[1]
    j<-dim[2]
    const<-pc$cells[[1]]
    coefs<-array(0,c(k,j,d))
    if(d) for(i in 1:d) coefs[,,i]<-pc$cells[[i+1]]
    pa<-list(dim=dim,degree=degree_matrix(pc),symb=pc$symb,const=const,array=coefs)
    class(pa)<-c("polyMarray","polyMatrix","x")
    return(pa)
}

# ----
polyMconvert.c2b <- # cells => broad
function(pc)
{
    dim<-dim(pc)
    d<-degree(pc)
    k<-dim[1]
    j<-dim[2]
    broad<-matrix(NA,k,j*(d+1))
    for(i3 in 1:(d+1)) broad[,(i3-1)*j+1:j]<-pc$cells[[i3]]
    pb<-list(dim=dim,degree=degree_matrix(pc),symb=pc$symb,broad=broad)
    class(pb) <- c("polyMbroad","polyMatrix")
    return(pb)
}

# ----
polyMconvert.c2d <- # cells => dlist
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
        dlist[[i1]][[i2]] <- polynom::polynomial(pn)}

    pd<-list(dim=dim,degree=degree_matrix(pc),symb=pc$symb,dlist=dlist)
    class(pd) <- c("polyMdlist","polyMatrix")
    return(pd)
}

# ----
polyMconvert.d2a <- # dlist => array
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
                degree=degree_matrix(pd),
				symb=pd$symb,
                const=coef0,
                array=if(d==0) NULL else coef1)
    class(pa)<-c("polyMarray","polyMatrix")
    return(pa)
}

# ----
polyMconvert.d2b <- # dlist => broad
function(pd)
{   dim<-dim(pd)
    k<-dim[1]
    j<-dim[2]
    d<-degree(pd)
    coefs<-coefs(pd)
    broad<-matrix(NA,k,j*(d+1))
    for(i in 1:(d+1))
      broad[,(i-1)*j+1:j]<-coefs[[i]]
    pb<-list(dim=dim,degree=degree_matrix(pd),symb=pd$symb,broad=broad)
    class(pb) <- c("polyMbroad","polyMatrix")
    return(pb)


    class(pb)<-c("polyMbroad","polyMatrix")
    return(pb)
}

# ----
polyMconvert.d2c <- # dlist => cells: list of matrices dim=c(k,j), length=d+1
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
    pc<-list(dim=dim,degree=degree_matrix(pd),symb=pd$symb,cells=cells)
    class(pc)<-c("polyMcells","polyMatrix")
    return(pc)
}



# -----
#  2. # converters between "character" and "polynomial"
#
#    + pn2ch      - "polynomial" to "char" converter
#
pn2ch <-
function(x,symb="x", digits = getOption("digits"), decreasing = FALSE, ...)
{
    p <- unclass(x)
    lp <- length(p) - 1 # fokszam
    names(p) <- 0:lp # felcimkezes
    p <- p[p != 0] # csak a nem nullak
    if(length(p) == 0) return("0") # ha elfogyott
    if(decreasing) p <- rev(p) # ha forditva kell
    signs <- ifelse(p < 0, "- ", "+ ") # az egyes tagok elojelei
    signs[1] <- if(signs[1] == "- ") "-" else "" # az elso csak ha -
    np <- names(p) # a hatvanyok
    # p <- as.character(abs(p)) # az egyutthatok
	  p <- abs(p)
      pw <- vector("character",length(p));
      for(j in 1:length(p)) pw[j]<-format(p[j], digits = digits);
      p <- pw;
      rm(pw)
    p[p == "1" & np != "0"] <- "" # nem kell a 1 mint nem-konstans egyutthato
    pow <- paste0(symb,"^", np)
    pow[np == "0"] <- ""  # z^0
    pow[np == "1"] <- symb # z^1
    stars <- rep.int("*", length(p))
    stars[p == "" | pow == ""] <- ""
    return(paste(signs, p, stars, pow, sep = "", collapse = " "))
}


# -----
#
#    + ch2pn      - "char" to "polynomial" converter
#
ch2pn <-
function(chv,symb="x")
  { w<-list()
    for(k in 1:length(chv))
      {

        ch<-chv[k]

        if(class(ch)!="character")  stop("The argumet must be a 'character' class vector!")
        ch<-paste(strsplit(ch," ",fixed=TRUE)[[1]],collapse="") # killing the spaces
        if(ch=="") return(NULL)
        if(substr(ch,1,1)=="+") ch<-substr(ch,2,nchar(ch))

        ch <- strsplit(ch,"+",fixed=TRUE)[[1]] # break by "+"
        ch <- strsplit(ch,"-",fixed=TRUE) # break by "-" => double list

        if(ch[[1]][1]=="") ch[[1]]<-c(paste0("-",ch[[1]][2]),ch[[1]][-(1:2)]) # the first chr == "-"
		# the term signs are "-" in the second order list
        for(i in 1:length(ch))
          if(length(ch[[i]])>1) for(j in 2:length(ch[[i]])) ch[[i]][j]<-paste0("-",ch[[i]][j])

        if(length(ch[[1]])==0) return(NULL)
        for(i in 1:length(ch))
          for(j in 1:length(ch[[i]]))
            if(substr(ch[[i]][j],nchar(ch[[i]][j]),nchar(ch[[i]][j]))==symb)
              ch[[i]][j]<-paste0(ch[[i]][j],"^1") # write "x^1" in place "x"

        for(i in 1:length(ch))
          for(j in 1:length(ch[[i]]))
            if(!sum(charToRaw(ch[[i]][j])==charToRaw(symb)) &
			   class(type.convert(ch[[i]][j],as.is=TRUE))!="character")
              ch[[i]][j]<-paste0(ch[[i]][j],paste0("*",symb,"^0")) # write "x^0" if a term of degree 0

        ch<-unlist(ch)

        for(i in 1:length(ch)) if(nchar(ch[i])>1&(substr(ch[i],1,2)==paste0("-",symb))) # change -x to -1*x
          ch[i]<-paste0("-1*",substr(ch[i],2,nchar(ch[i])))

        symb.terms<-NULL
        for(i in 1:length(ch))
          if(length(which(charToRaw(ch[i])==charToRaw(symb)))!=0)
		     symb.terms<-c(symb.terms,i)
        if(length(symb.terms)==0) {w<-c(w,list(polynom::polynomial(0)));next}
        ch<-ch[symb.terms]# the terms with "symb"

        v<-NULL
        for(i in 1:length(ch))
          v<-c(v,which(charToRaw(ch[i])==charToRaw(symb))) # pozitions of "symb"

        if(length(v)!=length(ch) & length(v)!=0)  stop("Format error!")
        ch[v==1]<-paste0("1*",ch[v==1]);v[v==1]<-3 # "1*" before "symb" if necessary
        # coefs and degrees
        ch.coef<-ch.degr<-rep("",length(ch))
        for(i in 1:length(ch)) ch.coef[i]<-substr(ch[i],1,v[i]-2)
        for(i in 1:length(ch)) ch.degr[i]<-substr(ch[i],v[i]+2,nchar(ch[i]))
        ch.coef<-type.convert(ch.coef)
        ch.degr<-type.convert(ch.degr)

        pch<-rep(0,max(ch.degr)+1)
        pch[trunc(ch.degr+1)]<-ch.coef # the coefficient vector
        w<-c(w,list(polynom::polynomial(pch)))
      }
    return(if(length(w)-1) w else w[[1]])
    }


#

# -----
#  3. # converters between MTS package model list and "polyMatrix"
#
#     + MTS2pM    - MTS model list to "polyMatrix" converter
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
      class(AR) <- c("polyMbroad","polyMatrix")
      MA <- list(dim=MAdim,degree=MAdegree,symb="x",broad=MAbroad)
      class(MA) <- c("polyMbroad","polyMatrix")
      obj <- (list(degree=degree,AR=AR,MA=MA,Cons=M$Ph0,Sigma=M$Sigma))
      class(obj) <- "pMvarma"
      return(obj)
    }


# ----
# fine
