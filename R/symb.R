symb <- function(pm)
{
  #' The symbol of polynomials in polymatrix
  check.is.polyMatrix(pm)
  return(pm$symb)
}
