rebuild_degree <- function(pm)
{
  #' Rebuild degree matrix of polyMatrix type dlist
  stopifnot(is.polyMatrix.polyMdlist(pm))
  result_degree <- matrix(NA, nrow(pm), ncol(pm))
  for(r in 1:nrow(pm)) {
    for(c in 1:ncol(pm)) {
      result_degree[r, c] <- degree(pm$dlist[[r]][[c]])
    }
  }
  pm$degree <- result_degree
  return(pm)
}
