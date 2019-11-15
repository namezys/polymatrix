# ---
# The lead of a polynomial or polynomial matrix

lead <-
function(p,method=c("matrix","column","row","element")) UseMethod("lead")

lead.polynomial <-
function(p,...)  return(as.numeric(p)[length(p)])

lead.polyMatrix <-
function(p,method=c("matrix","column","row","element"))
  {  #  stop("The 'p' input is not a regular 'polyMatrix' object!"))
     modszer<-substr(method[1],1,1)
     pc<-polyMconvert(p,"polyMcells")
     k<-dim(pc)[1];j<-dim(pc)[2];
     mit<- switch(modszer,
                  "m" =  matrix(degree(pc),k,j),                # matrix
                  "c" =  matrix(degree_column(pc),k,j,byrow=TRUE), # column
                  "r" =  matrix(degree_row(pc),k,j),            # row
                  "e" =  mit<-degree_matrix(pc)                    # element
                  )+1 # end of switch
     px <- matrix(NA,k,j)
     for(i1 in 1:k) for(i2 in 1:j)
       px[i1,i2] <- pc$cells[[mit[i1,i2]]][i1,i2]
     return(px)
   }


# ----
# fine
