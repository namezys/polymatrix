# -----
# pMstr      - consistecy check of a polyMatrix object
# -----

pMstr <-
function(pm)
   {
     # the class attribute
     cl<-class(pm)
     if (length(cl)!=2|cl[2]!="polyMatrix"|
         !(cl[1]%in%c("polyMarray","polyMbroad","polyMcells","polyMdlist")))
       stop("Problem with the object class!")
     cl<-substr(cl[1],3,3)

     # presence of "dim", "degree", "symb" arguments
     if (is.null(pm$dim)|is.null(pm$degree)|is.null(pm$symb))
       stop("One of argument 'dim', 'degree' or 'symb' missing!")

     # presence of the apropriate data arguments
     if (cl=="a"&(is.null(pm$const)|is.null(pm$array)))
       stop("The object is in 'polyMarray' form, but the data part absent!")

     if (cl=="b"&is.null(pm$broad))
       stop("The object is in 'polyMbroad' form, but the data part absent!")

     if (cl=="c"&is.null(pm$cells))
       stop("The object is in 'polyMcells' form, but the data part absent!")

     if (cl=="d"&is.null(pm$dlist))
       stop("The object is in 'polyMdlist' form, but the data part absent!")

     # the structure of the data part
     hibas<-FALSE
     hibas<-switch(cl,"a"=,"b"=,"c"=,"d"=)
     if (hibas)
       stop("Bad structure of the data part!")

     # dim
     hibas<-FALSE
     hibas<-switch(cl,"a"=,"b"=,"c"=,"d"=)
     if (hibas)
       stop("Bad dim argument!")

     # degree
     hibas<-switch(cl,"a"=,"b"=,"c"=,"d"=)
     if (hibas)
       stop("Bad dim argument!")

     # symb
     if (length(pm$symb)!=1|nchar(pm$symb[1])!=1)
       stop("Bad symb argument!")

     # ---
     hibas<-FALSE
     if(pm$degree!=dim(pm$coefs)[3]) {hibas<-TRUE;stop("Problem with degree !")}
     if(all(pm$dim!=dim(pm$const))) {hibas<-TRUE;stop("Problem with constant matrix dimension !")}
     if(all(pm$dim!=dim(pm$coefs)[1:2])) {hibas<-TRUE;stop("Problem with coefs dimension !")}
     while(sum(lead(pm)^2)==0) {hibas<-TRUE;pm$coefs<-pm$coefs[,,-dim(pm$coefs)[3],drop=FALSE];pm$degree<-pm$degree-1;cat("The degree subdued by 1 !\n")}
     if(hibas) cat("No consistent !\n") else cat("Consistent !\n")
     return(pm)
     pd<-pm
     ans <- cns <- "Consistent!!!\n"
     err <- "No consistent !!!\n"
     txt <- "pDdlist class inconsistency -"
     # absence of an element
     n<-length(pd$dlist)
     m<-rep(NA,n)
     for(k in 1:n)
     m[k] <- length (pd$dlist[[k]])
     if(!all(m==m[1]))
                      ans<-paste(txt,"one or more matrix element is missing")
     else
     # class problem of an sublist
     for (i in 1:length(pd$dlist))
        if (!is.list(pd$dlist[[i]]))
                      ans<-paste(txt,"class problem of an sublist")
     else
     # class problem of an element
     for (i in 1:length(pd$dlist))
        for (j in 1: length (pd$dlist[[1]]))
            if (!polynom::is.polynomial(pd$dlist[[i]][[j]]))
                      ans<-paste(txt,"class problem of an element")

    invisible(list(cns=if(ans==cns) TRUE else FALSE,
                   txt=if(ans==cns) cns else err,
                   err=ans))
   }

# -----------------
# fine
