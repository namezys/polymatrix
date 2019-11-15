# ---
# degree of a polynom or polynomial matrix

degree <- function(p,...) {
  UseMethod("degree")
}

degree.polynomial <- function(p,...) {
  return(length(p) - 1)
}

degree_matrix <- function(p)
{
  if (!is.polyMatrix(p)) {
    stop("polyMatrix expected")
  }
  return(p$degree)
}

degree_column <- function(p)
{
  return(colMax(degree_matrix(p)))
}

degree_row <- function(p)
{
  return(rowMax(degree_matrix(p)))
}

degree.polyMatrix <- function(p, ...)
{
  return(max(degree_matrix(p)));
}

# ----
# fine
