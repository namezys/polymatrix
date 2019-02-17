# -----
# print utilities
#  1. # print.pMatrix  - method for "pMatrix" class objects
#  2. # print.pMvarma  - method for "pMvarma" class objects
# ?3. # print.chpn     - print method for "chpn" class objects
#  4. # pprt           - print function for "polynomial" class objects
#  5. # pn2ch          - "polynomial" to "char" converter
#  6. # ch2pn          - "char" to "polynomial" converter
# --- # -----------


# ---
#  1. # print.pMatrix  - method for "pMatrix" class objects
# printing variations: style=c("matrix","polynom","broad","raw")
#                matrix - as a list of matrices
#                poly   - by a matrix of polynoms
#                broad  - by a broad matrix of coefficients
#                raw    - as it is stored
#
print.pMatrix <-
function(x,style=c("matrix","polynom","broad","raw"),digits = getOption("digits"),shift=2,...)
  {  style<-substr(style[1],1,1)
     switch(style, # check the first letter
            "r" = { pm <- x;                                # raw
                    class(pm) <- "list";
                    print(pm,digits=,digits)} ,
            "m" = { pd <- pMconvert(x,"pMdlist")            # matrix
                    dlist<-pd$dlist
                    dim<-dim(pd);k<-dim[1];j<-dim[2]
                    chdl<-vector("character" ,k*j)
                    for(i1 in 1:k)for(i2 in 1:j)
                      chdl[(i1-1)*j+i2]<-pn2ch(dlist[[i1]][[i2]],x$symb,digits)
                    cl<-max(nchar(chdl)) 
                    for(i3 in 1:(k*j)) 
                      chdl[i3]<-substr(paste0(chdl[i3],
                                       paste(rep(" ",cl), collapse="")),1,cl)
                    koz<-paste(rep(" ",shift), collapse="")
                    for(i1 in 1:k) 
                      cat(" ", paste(chdl[(i1-1)*j+1:j], collapse=koz),"\n")
                  }, 
            "p" = { dlist <- pMconvert(x,"pMdlist")$dlist   # polynom
                    for(i1 in 1:x$dim[1]) for(i2 in 1:x$dim[2]) 
                    pprt(dlist[[i1]][[i2]],x$symb,(i2-1)*shift,digits)},
            "b" = { pbm<-pMconvert(x,"pMbroad")$broad       # broad
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
function(x,style=c("matrix","polynom","broad","raw"),digits = getOption("digits"),shift=2,...)
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
        print.pMatrix(x$AR,style,digits,shift)
        cat(paste0(rep("=",17),collapse=""),"\n") }

    if(!is.na(x$degree[2]))	  
      { 	
		cat(paste0("MA:\n",paste0(rep("-",17),collapse=""),"\n"))
        print.pMatrix(x$MA,style,digits,shift)
        cat(paste0(rep("=",17),collapse=""),"\n") }
  }

# -----
# ?3. # print.charpn     - print method for "charpn" class objects
#
# print.charpn <-
# function(x,...)
#   { deg<-length(x)-1
#     res<-""
#     for(i in 0:deg)
#      { if(x[[i+1]]!=0)
#         res<-paste0(res,if(i) "+" else "",
#                     "(",pn2ch(x[[i+1]]),")'s^",i,"'")
#      }
#     cat(paste0(res,"\n"))}  

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

# -----
#  5. # pn2ch      - "polynomial" to "char" converter
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
#  6. # ch2pn     - "character" to "polynomial" converter
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
        if(length(symb.terms)==0) {w<-c(w,list(polynomial(0)));next}			 
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
        w<-c(w,list(polynomial(pch)))
      }
    return(if(length(w)-1) w else w[[1]])   
    }

# ----
# fine