# ---
# the constant of a polynom or polynomial matrix

const <-
function(p) UseMethod("const")

const.polynomial <-
function(p)  return(as.numeric(p)[1])


const.pMatrix <-
function(p)  
 { switch( class(p)[1],
           "pMarray"=p$const,
           "pMbroad"=p$broad[,1:dim(p)[2]],
           "pMcells"=p$cells[[1]],
           "pMdlist"= { dim<-dim(p)
                        k<-dim[1];j<-dim[2]
                        d.const<-matrix(NA,k,j)
                        for(i1 in 1:k) 
                          for(i2 in 1:j) 
                            d.const[i1,i2]<-as.numeric(p$dlist[[i1]][[i2]])[1]
                        d.const   },
            stop("A not regular 'pMatrix' class object!")
         )
 }


# ----
# fine