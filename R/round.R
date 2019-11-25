round <-function(x, digits=0)
{
  UseMethod("round")
}

round.default <- function(x, digits=0)
{
  return(base::round(x, digits=digits))
}

round.polynomial <- function(x, digits=0)
{
  return(polynom::polynomial(round(as.numeric(x), digits=digits)))
}

round.polyMatrix <- function(x, digits=0)
{
  pm <- polyMconvert.dlist(x)
  for(r in 1:nrow(pm)) {
    for(c in 1:ncol(pm)) {
      pm$dlist[[r]][[c]] <- round(pm$dlist[[r]][[c]], digits=digits)
    }
  }
  return(rebuild_degree(pm))
}
