# ---
# degree of a polynom or polynomial matrix

degree <-
function(p,...) UseMethod("degree")

degree.polynomial <-
function(p,...)  return(length(p)-1)

degree.polyMatrix <-
function(p,method=c("default","matrix","column","row"),...)  
       {d <- switch(substr(method[1],1,1),
                    "d"=max(p$degree),     # default
                    "m"=p$degree,          # matrix         
                    "c"=colMax(p$degree),  # column
                    "r"=rowMax(p$degree))  # row
        return(d)}
   

# ----
# fine
