# ---
# list of coef matrices (as in pMcells class) 

coefs <-
function(p,degree="all") UseMethod("coefs")

coefs.polynomial <-
function(p,degree="all")  
  { 
    if(degree[1]=="all") return(as.numeric(p))
      else return(as.numeric(p)[degree])
  }

coefs.pMatrix <-
function(p,degree="all")  
  { 
    if(degree[1]=="all") return(pMconvert(p,"pMcells")$cells)
      else return(pMconvert(p,"pMcells")$cells[degree]) 
  }


# ----
# fine