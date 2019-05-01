# ---
# list of coef matrices (as in polyMcells class) 

coefs <-
function(p,degree="all") UseMethod("coefs")

coefs.polynomial <-
function(p,degree="all")  
  { 
    if(degree[1]=="all") return(as.numeric(p))
      else return(as.numeric(p)[degree])
  }

coefs.polyMatrix <-
function(p,degree="all")  
  { 
    if(degree[1]=="all") return(polyMconvert(p,"polyMcells")$cells)
      else return(polyMconvert(p,"polyMcells")$cells[degree]) 
  }


# ----
# fine
