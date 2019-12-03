# the Ops group method for polyMatrix class // (2016.05.25)
# the elements of the Ops group are: == != + - * ^ %==% %!=% %+% %-% %*%
# --------------------------------------------------------------------------
# library("polyMatrix")

matrix_equal <- function(first, second)
{
  stopifnot(is.polyMatrix.polyMdlist(first), is.polyMatrix.polyMdlist(second))
  if (any(first$dim != second$dim)) {
    return(FALSE)
  }
  if (any(first$degree != second$degree)) {
    return(FALSE)
  }
  for(r in 1:first$dim[1]) {
    for(c in 1:first$dim[2] ) {
      if (first$dlist[[r]][[c]] != second$dlist[[r]][[c]]) {
        return(FALSE);
      }
    }
  }
  return(TRUE);
}

scalar_equal <- function(first, second)
{
  stopifnot(is.polyMatrix.polyMdlist(first))
  for(r in 1:first$dim[1]) {
    for(c in 1:first$dim[2] ) {
      if (first$dlist[[r]][[c]] != second) {
        return(FALSE);
      }
    }
  }
  return(TRUE)
}

build_matrix <- function(rows, columns, dlist, symb)
{
  d <- matrix(0, rows, columns);
  for (r in 1:rows) {
    for(c in 1:columns) {
      d[r, c] <- degree(dlist[[r]][[c]])
    }
  }

  pd <- list(dim=c(rows, columns), degree=d, symb=symb, dlist=dlist)
  class(pd) <- c(CLASS_MDLIST, CLASS_MATRIX)
  return(pd)
}

matrix_b_op <- function(first, second, op)
{
  stopifnot(is.polyMatrix.polyMdlist(first), is.polyMatrix.polyMdlist(second))
  if (any(first$dim != second$dim)) {
    stop("Arguments have diferent dim")
  }

  dlist <- vector("list", first$dim[1])
  for(r in 1:first$dim[1]) {
    dlist[[r]] = vector("list", second$dim[2])
  }

  for(r in 1:first$dim[1]) {
    for(c in 1:first$dim[2]) {
      dlist[[r]][[c]] = op(first$dlist[[r]][[c]], second$dlist[[r]][[c]])
    }
  }

  return(build_matrix(first$dim[1], first$dim[2], dlist, first$symb))
}

scalar_b_op <- function(first, second, op)
{
  stopifnot(is.polyMatrix.polyMdlist(first))

  dlist <- vector("list", first$dim[1])

  for(r in 1:first$dim[1]) {
    dlist[[r]] = vector("list", first$dim[2])
    for(c in 1:first$dim[2]) {
      dlist[[r]][[c]] <- op(first$dlist[[r]][[c]], second)
    }
  }

  return(build_matrix(first$dim[1], first$dim[2], dlist, first$symb))
}

matrix_mul <- function(first, second)
{
  stopifnot(is.polyMatrix.polyMdlist(first), is.polyMatrix.polyMdlist(second))

  if (first$dim[2] != second$dim[1]) {
    stop("non-conformable arguments of matrix multiplication")
  }

  rows = first$dim[1]
  columns = second$dim[2]
  result <- vector("list", rows)
  for(r in 1:rows) {
    result[[r]] <- vector("list", columns)
    for(c in 1:columns) {
      v <- polynom::polynomial(0)
      for(i in 1:first$dim[2]) {
        v <- v + first$dlist[[r]][[i]] * second$dlist[[i]][[c]]
      }
      result[[r]][[c]] <- v
    }
  }

  return(build_matrix(rows, columns, result, first$symb))
}

matrix_pow <- function(left, right)
{
  if (!is.polyMatrix(left)) {
    stop("Operator ^ is defined only for matrix as left operand")
  }
  if (!is.numeric(right) || right %% 1 != 0) {
    stop("Operator ^ is defined only for integer as right operand")
  }
  if (right < 0) {
    stop("Power of matrix can't be negative")
  }
  if (left$dim[1] != left$dim[2]) {
    stop("Power of matrix is defined only for square matrixes")
  }
  if (right == 0) {
    return(pMdiag(polynom::polynomial(1), left$dim[1], left$dim[2]))
  }
  if (right == 1) {
    return(left)
  }
  r <- left
  for(i in 2:right) {
    r <- r * left
  }
  return(r)
}

pOps <- function(left, right, operator)
{
  if (is.character(left)) {
    left <- ch2pn(left)
  }
  if (is.polyMatrix(left)) {
    left <- polyMconvert.dlist(left)
  }

  # unary operators
  if(missing(right)) {
    left <- polyMconvert.dlist(left)
    if (operator == "+") {
      return(left)
    } else if (operator == "-") {
      return(pMsgn(left))
    } else {
      stop(paste("unsupported unary operation: ", operator))
    }
  }

  if (is.character(right)) {
    right <- ch2pn(right)
  }
  if (is.polyMatrix(right)) {
    right <- polyMconvert(right, CLASS_MDLIST)
  } else {
    stopifnot(is.polyMatrix(left));
  }

  if (operator == "-") {
    operator <- "+"
    right <- -right
  }

  if (operator == "^") {
    return(matrix_pow(left, right))
  }

  if (!is.polyMatrix(left) || !is.polyMatrix(right)) {
    stopifnot(is.polyMatrix(left) || is.polyMatrix(right))

    if (is.polyMatrix(right)) {
      stopifnot(!is.polyMatrix(left))
      tmp <- left
      left <- right
      right <- tmp
    }

    result <- switch (operator,
      "==" = scalar_equal(left, right),
      "!=" = !scalar_equal(left, right),
      "+" = scalar_b_op(left, right, function(a, b) {a + b}),
      "*" = scalar_b_op(left, right, function(a, b) {a * b}),
      stop("Unknown operator")
    )
    return(result)
  }

  stopifnot(is.polyMatrix.polyMdlist(left), is.polyMatrix.polyMdlist(right))

  result <- switch (operator,
    "==" = matrix_equal(left, right),
    "!=" = !matrix_equal(left, right),
    "+" = matrix_b_op(left, right, function(a, b) {a + b}),
    "*" = matrix_mul(left, right),
    stop("Unknown operator")
  )

  return(result)
}

# define custom operators

Ops.polyMatrix <- function(e1, e2)
{
  return(pOps(e1, e2, .Generic))
}

"%==%" <- function(left, right)
{
  return(pOps(left, right, "=="))
}

"%!=%" <- function(left, right)
{
  return(pOps(left, right, "!="))
}

"%+%" <- function(left, right)
{
  return(pOps(left, right, "+"))
}

"%-%" <- function(left, right)
{
  return(pOps(left, right, "-"))
}

"%X%" <- function(left, right)
{
  return(pOps(left, right, "*"))
}

# ----
# fine
