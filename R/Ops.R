# the Ops group method for pMatrix class // (2016.05.25)
# the elements of the Ops group are: + - * / ^ < > <= >= != == %% %/% & | ! 
# --------------------------------------------------------------------------
# library("pMatrix") 

"%*%"<-function(x,y)
 { if(!is.pMatrix(x)&!is.pMatrix(y)) base::"%*%"(x,y)
    else
	if(!(is.pMatrix(x)&is.pMatrix(y))) stop("non-conformable arguments")
     else 
      if(dim(x)[2]!=dim(y)[1]) stop("non-conformable arguments")
       else 
        {
          k<-dim(x)[1]
          j<-dim(y)[2]
          x.y<-vector("list",k)
          for(i1 in 1:k) x.y[[i1]]<-vector("list",j)
          for(i1 in 1:k) for(i2 in 1:j) # row-column scalar product
            x.y[[i1]][[i2]]<-pVsk(pMrow(x,i1),pMcol(y,i2)) 
          d<-matrix(0,k,j)       
          for(i1 in 1:k) for(i2 in 1:j) d[i1,i2] <- degree(x.y[[i1]][[i2]])
          pd<-list(dim=c(k,j),degree=d,symb=x$symb,dlist=x.y)
          class(pd) <- c("pMdlist","pMatrix")		  
		  return(pd)}	  }


equal <- function(first, second)
{
  stopifnot(!missing(first), !missing(first))
  stopifnot(is.pMatrix(second), is.pMatrix(second))
  return(all(first$dim == second$dim) && all(first$degree == second$degree)
         && all(first$const == second$const) && all(first$array == second$array));
}


Ops.pMatrix <- 
function(e1, e2)
{
  # unari operators
  if(missing(e2)) {
      return(
        switch(.Generic,
               "+"=pMconvert(e1,"pMdlist"),
               "-"=pMsgn(pMconvert(e1,"pMdlist")),
               stop("unsupported unary operation")))
  }

  # ---- cmp
  if (.Generic == '==') {
    return(equal(e1, e2))
  } else if (.Generic == '!=') {
    return(!equal(e1, e2))
  }

    if(!is.pMatrix(e1)) {ee<-e2;e2<-e1;e1<-ee;rm(ee)}
    e1<-pMconvert(e1,"pMdlist")	
    {                  
    if(class(e2)[1]=="numeric") 
        { e2 <- if(.Generic ==     "*") pMdiag(polynomial(e2),dim(e1)[2])
                else if(.Generic %in% c("+","-")) pMgen.d(dim(e1)[1],dim(e1)[2],rawData=list(polynom::polynomial(e2))) # pMcons(polynomial(e2),dim(e1)[1],dim(e1)[2])
                else if(.Generic ==     "^") abs(trunc(e2[1])) }
      else { if(class(e2)[1]=="character"& .Generic == "*") e2 <- pMdiag(ch2pn(e2),dim(e1)[2])
      else { if(class(e2)[1]=="polynomial"& .Generic == "*") e2 <- pMdiag(e2,dim(e1)[2])
      else   if(class(e2)[1]%in%c("pMarray","pMbroad","pMcells","pMdlist"))
                 e2<-pMconvert(e2,"pMdlist")
               else { if(class(e2)=="matrix") e2<-M2pM(e2,"pMdlist")
               else stop("The object on the left side is a 'pMatrix'
                          but the object on right side 
                          not a 'numeric','matrix' or 'pMatrix' object")} }}
    }  
    dim<-switch(.Generic,"+"=,"-"=,"^"= dim(e1),"*"=c(dim(e1)[1],dim(e2)[2]))
    k<-dim[1]
    j<-dim[2]
    e1.e2<-vector("list", k)
    for(i1 in 1:k) e1.e2[[i1]]<-vector("list",j)
    e1.op.e2 <-
        switch(.Generic,
               "+" = { for(i1 in 1:k) for(i2 in 1:j) 
                           e1.e2[[i1]][[i2]] <- e1$dlist[[i1]][[i2]]+e2$dlist[[i1]][[i2]];
                       e1.e2},
               "-" = { for(i1 in 1:k) for(i2 in 1:j) 
                           e1.e2[[i1]][[i2]] <- e1$dlist[[i1]][[i2]]-e2$dlist[[i1]][[i2]];
                       e1.e2},
               "*" = { for(i1 in 1:k)
                         for(i2 in 1:j) # row-column scalar product
                           e1.e2[[i1]][[i2]]<-pVsk(pMrow(e1,i1),pMcol(e2,i2)) 
                       e1.e2},
               "^" = { e1.e2 <- if(e2==0) pMdiag(polynomial(1),k,k)$dlist
                                  else if (e2==1) e1$dlist
                                  else { prline<-
                                         paste0("(e1",
                                                paste0(rep("*e1",e2-1),collapse=""),
                                                ")$dlist")
                                  eval(parse(text=prline)) }
                       e1.e2},
           stop("unsupported operation on polynomial matrices")) 
    d<-matrix(0,k,j)       
    for(i1 in 1:k) for(i2 in 1:j) d[i1,i2] <- degree(e1.op.e2[[i1]][[i2]])
    pd<-list(dim=dim,degree=d,symb=e1$symb,dlist=e1.op.e2)       
    class(pd) <- c("pMdlist","pMatrix")
    return(pd)
}

# ----
# fine