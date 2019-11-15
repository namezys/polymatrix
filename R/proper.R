# ---
# a polynomial matrix is "proper" if the associeted matrix has full rank

proper <-
function(pm,type=c("col","row","both"),print=c(TRUE,FALSE))
  {
    type <- switch(substr(type[1],1,1),c="column",r="row",b="both")

    k <- dim(pm)[1]
    j <- dim(pm)[2]
    lm <- lead(pm,"element")
    m_deg <- degree_matrix(pm)

    # column
    c_deg <- matrix(degree_column(pm),dim(pm)[1],dim(pm)[2],byrow=TRUE)
    c_ass <- matrix(NA,k,j)
    for(i1 in 1:k) for(i2 in 1:j)
      c_ass[i1,i2] <- if(m_deg[i1,i2]==c_deg[i1,i2]) lm[i1,i2] else 0
    c_proper <- sum( abs(eigen(c_ass)$val)>10^(-10) )==j
    c_obj <- eval(parse(text=paste0("list('column-proper'=c_proper,'column-associated matrix'=c_ass)" )))

    # row
    r_deg <- matrix(degree_row(pm),dim(pm)[1],dim(pm)[2],byrow=FALSE)
    r_ass <- matrix(NA,k,j)
    for(i1 in 1:k) for(i2 in 1:j)
      r_ass[i1,i2] <- if(m_deg[i1,i2]==r_deg[i1,i2]) lm[i1,i2] else 0
    r_proper <- sum( abs(eigen(r_ass)$val)>10^(-10) )==k
    r_obj <-eval(parse(text=paste0("list('row-proper'=r_proper,'row-associated matrix'=r_ass)" )))


    if (print[1])
     {
	  if(type=="column") cat(paste("\nThe given matrix is column-proper? ",c_proper,"\n\n"))
	  if(type=="row")    cat(paste("\nThe given matrix is row-proper? ",r_proper,"\n\n"))
	  if(type=="both")   cat(paste("\nThe given matrix is column//row-proper? ",c_proper,r_proper,"\n\n"))
     }
    invisible(c(c_obj,r_obj))
 }



# ----
# fine
