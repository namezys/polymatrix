
predict.polynomial <-
function(object,M,meth=c("as.matrix","as.in.the.polynom.package"),...)
{    pc<-as.numeric(object)
     d<-degree(object)
     x<-M # the argument 
     v<-switch(tail(class(x),1),
               "complex"= ,
               "numeric"= { w <- 0
                            pc <- rev(pc)
                            for(pcj in pc) w <- x * w + pcj
                            w 
                          },
               "matrix" = { if(substr(meth[1],1,6)=="as.in.")
                             { w <- 0
                               pc <- rev(pc)
                               for(pcj in pc) w <- x * w + pcj
                               w }
                            else 
                             { xh <- diag(dim(M)[1])
                               w <- pc[1] * xh
                               if(d) for(i in 1:d)
                                 { xh <- xh %*% x
                                   w <- w + pc[i+1] * xh }
                               w }
                          },
               "polyMatrix" = { w <- pMdiag(polynom::polynomial(0),dim(M)[1])
                                pcr<-rev(pc)
                                for(pcrj in pcr) 
                                  w <- w %X% x + pMdiag(polynom::polynomial(pcrj),dim(M)[1])
                                w },
               stop("The argument not a real, complex matrix or 'polyMatrix' class object!")
                             )
     return(v)
}


predict.polyMatrix <-
function(object, M, ...)
{ 
    x <- drop(M)[1]
    if(is.null(x))     return(NULL)
    if(is.nan(x))      return(NaN)
    if(is.na(x))       return(NA)
    if(is.infinite(x)) return(NaN)
    pd <- polyMconvert(object,"polyMdlist")
    k <- dim(pd)[1]
    j <- dim(pd)[2]
    v <- matrix(NA, k, j)
    for (i1 in 1:k) for (i2 in 1:j) { 
        v[i1, i2] <- predict(pd$dlist[[i1]][[i2]], M) 
	}
    return(v)
}

predict.charpn <-
function(object, pM, ...)
{ 
    chpn <- object
	d <- length(chpn)
	I <- Mh <- pMdiag(ch2pn("1"),dim(pM)[1])
    v <- I * pn2ch(chpn[[1]]) 
    for (k in 2:d)
	   { Mh<-Mh %X% pM
         v <- v + Mh * pn2ch(chpn[[k]])	}
    return(v)
}



# ----
# fine
