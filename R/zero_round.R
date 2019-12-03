zero_round <-function(x, eps=ZERO_EPS)
{
  #' Round object to zero if it's too small
  UseMethod("zero_round")
}

zero_round.default <- function(x, eps=ZERO_EPS)
{
  x[abs(x) < eps] <- 0
  return(x)
}

zero_round.polynomial <- function(x, eps=ZERO_EPS)
{
  return(polynom::polynomial(zero_round(as.numeric(x), eps=eps)))
}

zero_round.polyMatrix <- function(x, eps=ZERO_EPS)
{
  pm <- polyMconvert.dlist(x)
  for(r in 1:nrow(pm)) {
    for(c in 1:ncol(pm)) {
      pm$dlist[[r]][[c]] <- zero_round(pm$dlist[[r]][[c]], eps=eps)
    }
  }
  return(rebuild_degree(pm))
}
