# the Ops group method for pMatrix class // (2016.05.25)
# the elements of the Ops group are: + - * / ^ < > <= >= != == %% %/% & | !
# --------------------------------------------------------------------------
# library("pMatrix")

"%*%"<-function(x,y)
 { if(!is.pMatrix(x)&!is.pMatrix(y)) base::"%*%"(x,y)
    else
	if(!(is.pMatrix(x)&is.pMatrix(y))) stop("non-conformable arguments")
     else
      if(dim(x)[2]!=dim(y)[1]) stop("non-conformable arguments")
       else
        {
          k<-dim(x)[1]
          j<-dim(y)[2]
          x.y<-vector("list",k)
          for(i1 in 1:k) x.y[[i1]]<-vector("list",j)
          for(i1 in 1:k) for(i2 in 1:j) # row-column scalar product
            x.y[[i1]][[i2]]<-pVsk(pMrow(x,i1),pMcol(y,i2))
          d<-matrix(0,k,j)
          for(i1 in 1:k) for(i2 in 1:j) d[i1,i2] <- degree(x.y[[i1]][[i2]])
          pd<-list(dim=c(k,j),degree=d,symb=x$symb,dlist=x.y)
          class(pd) <- c("pMdlist","pMatrix")		
		  return(pd)}	  }


matrix_equal <- function(first, second)
{
  stopifnot(!missing(first), !missing(second))
  stopifnot(is.pMatrix(second), is.pMatrix(second))
  stopifnot(all(class(first) == class(second)))
  stopifnot(all(class(first) == c("pMdlist", "pMatrix")))
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
  stopifnot(!missing(first), !missing(second))
  stopifnot(is.pMatrix(first), !is.pMatrix(second))
  stopifnot(all(class(first) == c("pMdlist", "pMatrix")))
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
  class(pd) <- c("pMdlist","pMatrix")
  return(pd)
}

matrix_b_op <- function(first, second, op)
{
  stopifnot(!missing(first), !missing(second))
  stopifnot(is.pMatrix(second), is.pMatrix(second))
  stopifnot(all(class(first) == class(second)))
  stopifnot(all(class(first) == c("pMdlist", "pMatrix")))
  if (any(first$dim != second$dim)) {
    stop("Argemnts have a diferent dim")
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
  stopifnot(!missing(first), !missing(second))
  stopifnot(is.pMatrix(first), !is.pMatrix(second))
  stopifnot(all(class(first) == c("pMdlist", "pMatrix")))
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
  stopifnot(!missing(first), !missing(second))
  stopifnot(is.pMatrix(second), is.pMatrix(second))
  stopifnot(all(class(first) == class(second)))
  stopifnot(all(class(first) == c("pMdlist", "pMatrix")))

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
  if (!is.pMatrix(left)) {
    stop("Operator ^ is defined only for matrix as left operand")
  }
  if (!is.numeric(right)) {
    stop("Operator ^ is defined only for integer as right operand")
  }
  if (right %% 1 != 0) {
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

Ops.pMatrix <- function(e1, e2)
{
  # unari operators
  if(missing(e2)) {
    result <- switch (.Generic,
      "+" = pMconvert(e1,"pMdlist"),
      "-" = pMsgn(pMconvert(e1,"pMdlist")),
      stop("unsupported unary operation")
    )
    return(result)
  }

  if (is.pMatrix(e1)) {
    e1 <- pMconvert(e1, "pMdlist")
  }

  if (is.pMatrix(e2)) {
    e2 <- pMconvert(e2, "pMdlist")
  }

  if (is.character(e1)) {
    e1 <- ch2pn(e1)
  }

  if (is.character(e2)) {
    e2 <- ch2pn(e2)
  }

  stopifnot(is.pMatrix(e1) || is.pMatrix(e2))

  if (.Generic == "-") {
    .Generic <- "+"
    e2 <- -e2
  }

  if (.Generic == "^") {
    return(matrix_pow(e1, e2))
  }

  if (!is.pMatrix(e1) || !is.pMatrix(e2)) {
    # we got only one matrix, but all operation is communicative or anticomunicative
    stopifnot(is.pMatrix(e1) || is.pMatrix(e2))
    anitcomunicative <- FALSE
    if (is.pMatrix(e2)) {
      stopifnot(!is.pMatrix(e1))
      tmp <- e1
      e1 <- e2
      e2 <- tmp
    }

    result <- switch (.Generic,
      "==" = scalar_equal(e1, e2),
      "!=" = !scalar_equal(e1, e2),
      "+" = scalar_b_op(e1, e2, function(a, b) {a + b}),
      "*" = scalar_b_op(e1, e2, function(a, b) {a * b}),
      stop("Unknown operator")
    )
    return(result)
  }

  stopifnot(is.pMatrix(e1), is.pMatrix(e2))

  result <- switch (.Generic,
    "==" = matrix_equal(e1, e2),
    "!=" = !matrix_equal(e1, e2),
    "+" = matrix_b_op(e1, e2, function(a, b) {a + b}),
    "*" = matrix_mul(e1, e2),
    stop("Unknown operator")
  )

  return(result)
}

# ----
# fine