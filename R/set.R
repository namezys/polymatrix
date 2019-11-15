# ---
# set a subsection of a polynomial matrix

set <- function(pm, i, j, m, byrow=FALSE) {
  UseMethod("set")
}

set.polyMatrix <-
function(pm,i,j,m,byrow=FALSE)
 {  ij <- length(i)*length(j) # element_2_change
    lm <- length(m) # quantity_new_element
    if(lm != ij)
      { if (lm > ij) m <- m[1:ij]
          else
           {
             k <- ij%/%lm
             h <- ij%%lm
             m <- if(k)
                    { if(h) c(rep(m,k),m[1:h]) else rep(m,k) }
                    else
                    { m[1:h] }
           }
      }
   class <- class(pm)
   dim <- dim(pm)
   degree <- degree_matrix(pm)
   symb <- symb(pm)
   dlist <- polyMconvert(pm,"polyMdlist")$dlist

   k <- 0
   if(byrow)
     {
       for (jk in j)
         for (ik in i)
            {
              k <- k+1
              dlist[[ik]][[jk]] <- m[[k]]
            }
     }
    else
     {
       for (ik in i)
         for (jk in j)
            {
              k <- k+1
              dlist[[ik]][[jk]] <- m[[k]]
            }
     }

   pm_new <- list(class=class,dim=dim,degree=degree,symb=symb,dlist=dlist)
   class(pm_new) <- c("polyMdlist","polyMatrix")
   pm_new <- polyMconvert(pm_new,class[1])

   return(pm_new)
 }


# ----
# fine
