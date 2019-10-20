# CanForm  calls the CanForm.echelon // CanForm.final // CanForm.scm

# CanForm.echelon  varma => echelon
# CanForm.final    varma => final
# CanForm.scm      varma => scm

			   
# ----
CanForm <- # VARMA conversion to echelon//final//scm form
function(pM,form)
 {
        pN <- switch(form,
	           "echelon" = CanForm.echelon(pM),
	           "final"   = CanForm.final(pM),
	           "scm"     = CanForm.scm(pM))
       return(pN)
}
  

# ----
CanForm.echelon <- # VARMA conversion to echelon form
function(pM)
  return(pM)

# ----
CanForm.final <- # VARMA conversion to fimal form
function(pM)
  {
    AR<-pMdiag(list(pMdet(pM$AR)),dim(pM$AR)) 
    MA<-pMadj(pM$AR)%X%pM$MA
    pM$AR<-AR	
    pM$MA<-MA
    pM$degree<-c(degree(AR),degree(MA))
    pM$final<-TRUE
    return(pM)
  }

# ----
CanForm.scm <- # VARMA conversion to scale component form
function(pM)
  return(pM)


# ----
# fine
