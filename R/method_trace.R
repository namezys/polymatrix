# Title     : Method trace
# Created by: namezys
# Created on: 2020. 10. 21.

tr <- function(x) {
  r <- 0
  for(i in seq_len(min(dim(x)))) {
    r <- r + x[i, i]
  }
  return(r)
}

