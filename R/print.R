# -----
# print utilities
#  1. # print.polyMatrix  - method for "polyMatrix" class objects
#  2. # print.pMvarma  - method for "pMvarma" class objects
#  3. # print.charpn   - print method for "charpn" class objects
#  4. # pprt           - print function for "polynomial" class objects
# --- # -----------


# ---
#  1. # print.polyMatrix  - method for "polyMatrix" class objects
#    printing variations: style=c("matrix","polynom","broad","raw")
#                matrix - as a list of matrices
#                poly   - by a matrix of polynoms
#                broad  - by a broad matrix of coefficients
#                raw    - as it is stored
#
print.polyMatrix <-
function(x,style=c("matrix","polynom","broad","raw"),
           round=NULL,digits = getOption("digits"),shift=3,decreasing = FALSE,...)
  {
  if(!is.null(round))
    {
     orig.class <- class(x)
     x <- polyMconvert(x,"polyMbroad")
     x$broad <- round(x$broad,round)
     x <- polyMconvert(x,orig.class[1])
    }
  style<-substr(style[1],1,1)
     switch(style, # check the first letter only
            "r" = { pm <- x;                                # raw
                    class(pm) <- "list";
                    print(pm,digits=,digits)} ,
            "m" = { pd <- polyMconvert(x,"polyMdlist")            # matrix
                    dlist <- pd$dlist
                    k<-nrow(pd);j<-ncol(pd)
                    chdl <- matrix("" ,k,j)
                    for(i1 in 1:k)for(i2 in 1:j)
                      chdl[i1,i2] <- pn2ch(dlist[[i1]][[i2]], symb = x$symb, digits = digits, decreasing = decreasing)
                    
                    for(i2 in 1:j)
                      { cl<-max(nchar(chdl[,i2]))
                        for(i1 in 1:k)
                        chdl[i1,i2] <- substr(paste0(chdl[i1,i2],
                                       paste(rep(" ",cl), collapse="")),1,cl) }

                    koz <- paste(rep(" ",shift), collapse="")
                    for(i1 in 1:k)
                      cat(" ", paste(chdl[i1,], collapse=koz),"\n")
                  },
            "p" = { dlist <- polyMconvert(x,"polyMdlist")$dlist   # polynom
                    for(i1 in 1:x$dim[1]) for(i2 in 1:x$dim[2])
                    pprt(dlist[[i1]][[i2]],x$symb,(i2-1)*shift,digits)},
            "b" = { pbm<-polyMconvert(x,"polyMbroad")$broad       # broad
                    j<-dim(x)[2]
                    k<-dim(pbm)[1]
                    m<-dim(pbm)[2]
                    dmax1<-m/j
                    ch<-NULL
                    for(i1 in 1:k) for(i3 in 1:m)
                      ch<-c(ch,format(pbm[i1,i3],digits=digits))
                    cl<-max(nchar(ch))
                    koz<-paste(rep(" ",cl), collapse="")
                    for(i4 in 1:(k*m))
                      { w<-paste0(koz,ch[i4])
                        ch[i4]<-substr(w,nchar(w)-cl+1,nchar(w)) }
                    koz<-paste(rep(" ",shift), collapse="")
                    for(i1 in 1:k)
                     { cat(" ")
                       for(i3 in 1:dmax1)
                         cat(paste(ch[(i1-1)*m+(i3-1)*j+1:j], collapse=" "),collapse=koz)
                       cat("\n") }
                  }  ) # end of switch
  }
# ---
#  2. # print.pMvarma  - method for "pMarma" class objects
# printing variations: style=c("matrix","polynom","broad","raw")
#                matrix - as a list of matrices
#                poly   - by a matrix of polynoms
#                broad  - by a broad matrix of coefficients
#                raw    - as it is stored
#
print.pMvarma <-
function(x,style=c("matrix","polynom","broad","raw"),round=NULL,digits = getOption("digits"),shift=3,...)
  {
    cat(paste0("ARMA(",x$degree[1],",",x$degree[2],")\n"))
    cat(paste0("Dim: (",dim(x$AR)[1],"x",dim(x$AR)[2],");(",dim(x$MA)[1],"x",dim(x$MA)[2],")\n"))
	if(!is.null(x$kronecker))
      cat(paste0("Kronecker: (",paste0(x$kronecker,collapse=", "),")\n"))
	if(!is.null(x$final))
      cat(paste0("Final: ",x$final,"\n"))
	if(!is.null(x$scm))
      cat(paste0("Scale Component Modell: ",x$scm,"\n"))
    cat(paste0(rep("=",17),collapse=""),"\n")

    if(!is.na(x$degree[1]))
      {
        cat(paste0("AR:\n",paste0(rep("-",17),collapse=""),"\n"))
        print.polyMatrix(x$AR,style,round,digits,shift)
        cat(paste0(rep("=",17),collapse=""),"\n") }

    if(!is.na(x$degree[2]))
      {
		cat(paste0("MA:\n",paste0(rep("-",17),collapse=""),"\n"))
        print.polyMatrix(x$MA,style,round,digits,shift)
        cat(paste0(rep("=",17),collapse=""),"\n") }
  }

# -----
#  3. # print.charpn     - print method for "charpn" class objects
#
print.charpn <-
 function(x,short=FALSE,...)
   { if(!short) print(unclass(x)) else
     { deg<-length(x)-1
       res<-""
       for(k in 0:deg)
        { if(x[[k+1]]!=0)
           res<-paste0(res,if(k) "+" else "",
                       "(",pn2ch(x[[k+1]]),")",names(x)[k])
        } 
       cat(paste0(res,"\n"))     }
   }

# -----
#  4. # pprt           - print function for "polynomial" class objects
#
pprt <-
function(x, symb="x", shift=0, digits = getOption("digits"), decreasing = FALSE, ...)
{   shift<-trunc(abs(shift))
    sh<- if(shift) paste(rep(" ",shift), collapse = "") else ""
    p <- pn2ch(signif(x, digits = digits), symb, decreasing = decreasing)
    pc <- nchar(p)
    ow <- max(35, getOption("width")-3-shift) # kiiras: aktualis szelesseg de legalabb 35
    m2 <- 0 # az elozo sorba kiirt utolso ch
    while(m2 < pc) {
        m1 <- m2 + 1 # a most kiirando elso
        m2 <- min(pc, m2 + ow) # a most kiirando utolso
        if(m2 < pc) # ha nem a vegeig irnank...
            while(!(substr(p, m2, m2) %in% c("+","-")) && m2 > m1 + 1)
                m2 <- m2 - 1 # vissza egy spaciumig => sorvegi-eleji ++//--
        cat(sh,substring(p, m1, m2), "\n")
        if(m2<pc) m2 <- m2 - 1
    }
}


# ----
# fine
