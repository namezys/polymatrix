# ---
# list of coef matrices (as in polyMcells class)

coefs <- function(p,degree="all")
{
  UseMethod("coefs")
}

coefs.polynomial <- function(p, degree="all")
{
  result <- as.numeric(p)
  if (degree[1] == "all") {
    return(result)
  }
  result <- result[degree + 1]
  result[is.na(result)] <- 0
  return(result)
}

coefs.polyMatrix <- function(p, degree="all")
{
  result <- polyMconvert(p, CLASS_MCELSS)$cells
  if (degree[1] == "all") {
    return(result)
  }
  result <- result[degree + 1]
  result[sapply(result, is.null)] <- list(matrix(0, nrow(p), ncol(p)))
  return(result)
}


# ----
# fine
