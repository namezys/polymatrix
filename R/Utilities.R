# -----------------
# Matrix utilities
#
#  0. # is.polyMatrix - consistecy check of a polyMatrix object
#  1. # pMcol     - a column of a polynomial matrix
#  2. # pMrow     - a row of a polynomial matrix
#  3. # pMbas     - bastion vector, the permutation: 'ki' (calc of det)
#  4. # pMsub     - sub-matrix of a polynomial-matrix
#  5. # pMprod    - product of the elements of a polynomial matrix
#  6. # pVsk      - scalar product of two polynomial vector
#  7. # pMsgn     - sign-change of a polynomial matrix
#  8. # ssetNext  - next subset of a set
#  9. # permNext  - lexicographical next permutation
# 10. # permSign  - the sign of a permutation
# 11. # colMax    - column maximum values of a matrix
#     # rowMax    - row maximum values of a matrix
# 12. # colMin    - column minimum values of a matrix
#     # rowMin    - row minimum values of a matrix
# 13. # cycFill   - cyclic fill by a given material
# 14. # pMdiag    - diagonal polynomial matrix
# 15. # GCD LCM   - greatest common divisor and least common multiple
# --- # -----------

# -----------------
#  1. # pMcol     - a column of a polynomial matrix


pMcol <- function(pm, which=1)
{
  plist <- polyMconvert.dlist(pm)
  pdim <- dim(plist)[1]
  dlist <- vector("list", pdim)
  degree <- matrix(NA, pdim, 1)

  for (i in 1:pdim) {
    dlist[[i]] <- list(plist$dlist[[i]][[which]])
    degree[i, 1] <- degree(dlist[[i]][[1]])
  }

  return(structure(list(dim=c(pdim, 1), degree=degree, symb="x", dlist=dlist),
                   class=c(CLASS_MDLIST, CLASS_MATRIX)))
}


# -----------------
#  2. # pMrow     - a row of a polynomial matrix

pMrow <- function(pm, which=1)
{
  plist <- polyMconvert.dlist(pm)
  pdim <- dim(plist)[2]
  dlist <- vector("list", 1)
  dlist[[1]] <- vector("list", pdim)
  degree <- matrix(NA, 1, pdim)

  for (i in 1:pdim) {
    dlist[[1]][[i]] <- plist$dlist[[which]][[i]]
    degree[1, i] <- degree(dlist[[1]][[i]])
  }

  return(structure(list(dim=c(1, pdim), degree=degree, symb="x", dlist=dlist),
                   class=c(CLASS_MDLIST, CLASS_MATRIX)))
}

# -----------------
#  3. # pMbas     - bastion vector, the permutation: 'ki'

pMbas <- function(pm, ki, byrow)
{
  size <- length(ki)

  if (byrow & size > dim(pm)[1] | (!byrow & size > dim(pm)[2])) {
    stop("Index vector too long!")
  }

  pd <- polyMconvert(pm, "polyMdlist")

  if(dim(pd)[1] == 1) {
    pd <- t(pd)
  }

  if(dim(pd)[2] == 1) {
    v <- pd$dlist[ki]
    degree <- degree_matrix(pm)[ki, 1, drop=FALSE]
  } else {
    v <- vector("list", size)
    if (byrow) {
      for (i in 1:size) {
        v[[i]][1] <- list(pd$dlist[[i]][[ki[i]]])
      }
    } else {
      for (i in 1:size) {
        v[[i]][1] <- list(pd$dlist[[ki[i]]][[i]])
      }
    }

    degree <- vector("numeric", size)

    for (i in 1:size) {
      degree[i] <- degree(v[[i]][[1]])
    }
  }

  return(structure(list(dim=c(size, 1), degree=matrix(degree, size, 1), symb="x", dlist=v), class=c("polyMdlist", "polyMatrix")))
}


# -----------------
#  4. # pMsub     - sub-matrix of a polynomial-matrix

pMsub <- function(pm, i, j=i)
{
  pd <- polyMconvert(pm, "polyMdlist")

  if (length(i) > 0) {
    i <- i[i!=0]
    i <- i[i<=pd$dim[1]]

	 if (length(i) > 0) {
    if ((!all(i < 0)) & (!all(i > 0))) {
      stop("The row selection indices have different signs!")
    }

      pd$dlist <- pd$dlist[i] # retain or delete the i.th row(s)

    if (i[1] < 0) {
      pd$dim[1] <- pd$dim[1]-length(i)
    } else {
        pd$dim[1] <- length(i)
    }
   }
  }

  if (length(j) > 0) {
   j <- j[j!=0]
   j <- j[j<=pd$dim[2]]

   if (length(j) > 0) {
    if ((!all(j < 0)) & (!all(j > 0))) {
     stop("The column selection indices have different signs!")
    }

    for(k in 1:length(pd$dlist)) {
      pd$dlist[[k]] <- pd$dlist[[k]][j] # retain or delete the j.th column from the k.th row
    }

    if (j[1] < 0) {
	   pd$dim[2] <- pd$dim[2] - length(j)
	  } else {
	   pd$dim[2] <- length(j)
	  }
	 }
  }

  rows <- if (length(i)) i else 1:pd$dim[1]
  columns <- if (length(j)) j else 1:pd$dim[2]

  pd$degree <- pd$degree[rows, columns, drop=FALSE]
  pm <- polyMconvert(pd, class(pm)[1])

  return(pm)
}

# -----------------
#  5. # pMprod    - a product of the elements of a polynomial vector

pMprod <- function(pm)
{
  pm <- polyMconvert(pm, "polyMdlist")

  dimMax <- max(dim(pm))
  pr <- polynom::polynomial(1)

  for (i in 1:dimMax) {
    if (dimMax == dim(pm)[1]) {
      value <- pm$dlist[[i]][[1]]
    } else {
      value <- pm$dlist[[1]][[i]]
    }
    pr <- pr * value
  }

  return(pr)
}


# -----------------
#  6. # pVsk      - scalar product of two polynomial vectors

pVsk <- function(pMx, pMy=NULL)
{
  if (is.null(pMy)) {
    pMy <- pMx
  }

  if (max(min(dim(pMx)), min(dim(pMy))) != 1) {
       stop("The scalar product works only for two vector 'polyMatrix' object!")
  }

  if(max(dim(pMx)) != max(dim(pMy))) {
       stop("The scalar product works only for two equal length vectors!")
  }

  dimMax <- max(dim(pMx))
  pdx <- polyMconvert(pMx, "polyMdlist")
  pdy <- polyMconvert(pMy, "polyMdlist")

  if (dimMax == dim(pdx)[2]) {
    pdx <- t(pdx)
  }

  if (dimMax == dim(pdy)[2]) {
    pdy <- t(pdy)
  }

  p <- polynom::polynomial(0)

  for (i in 1:dimMax) {
    p <- p + pdx$dlist[[i]][[1]] * pdy$dlist[[i]][[1]]
  }

  return(p)
}


# -----------------
#  7. # pMsgn     - sign-change of a polynomial matrix

pMsgn <- function(pm)
{
  check.is.polyMatrix(pm)
  if (is.polyMatrix.polyMarray(pm)) {
    pm$const <- -pm$const
    pm$array <- -pm$array
  } else if (is.polyMatrix.polyMbroad(pm)) {
    pm$broad <- -pm$broad
  } else if (is.polyMatrix.polyMcells(pm)) {
    for(i in 0:degree(pm)) {
        pm$cells[[i+1]] <- -pm$cells[[i+1]]
    }
  } else if (is.polyMatrix.polyMdlist(pm)) {
    for(r in 1:nrow(pm)) {
      for(c in 1:ncol(pm)) {
        pm$dlist[[r]][[c]] <- -pm$dlist[[r]][[c]]
      }
    }
  } else {
    stop("Not a regular 'polyMatrix' class object!")
  }
  return(pm)
}


# -----------------
#  8. # ssetNext  - next subset of a set

ssetNext <- function(set)
{
  size <- length(set)

  if (size == 1) {
    if (set == 0) {
      return(size)
    } else {
      return(rep(0, set))
    }
  }

  if (all(set != 0)) {
    return(size)
  } else {
    k <- tail(which(set==0), 1)
    set[k] <- 1
    if (k < size) {
      set[(k + 1):size] <- 0
    }
    return(set)
  }
}


# -----------------
#  9. # permNext  - lexicographical next permutation

permNext <- function(prm)
{
  if (length(prm) == 1) {
    return(1:prm)
  }

  if (!all(sort(prm) == 1:length(prm))) {
    stop("The given 'prm' is not a permutation")
  }

  p <- tail(which(diff(prm) > 0), 1)

  if (length(p) == 0) {
    return(length(prm))
  }

  a <- prm[p]
  b <- prm[(p + 1):length(prm)]
  c <- min(b[b > a])
  b <- b[b != c]
  prm[p] <- c
  prm[(p + 1):length(prm)] <- sort(c(a, b))
  return(prm)
}


# -----------------
# 10. # permSign  - the sign of a permutation

permSign <- function(prm)
{
  if(!all(sort(prm) == 1:length(prm))) {
    stop("The given 'r' is not a permutation")
  }

  size <- length(prm)
  valueToReturn <- 1

  for(i in (size - 1):1) {
    for(j in 1:i) {
      if(prm[j] > prm[j + 1]) {
        prm[j + 0:1] <- prm[j + 1:0]
        valueToReturn <- -valueToReturn
      }
    }
  }

  return(valueToReturn)
}


# -----------------
# 11. # colMax    - column maximum values of a matrix
      # rowMax    - row maximum values of a matrix

colMax <- function(matrix)
{
  check.is.matrix(matrix)
  numVector <- vector("numeric", ncol(matrix))

  for(i in 1:ncol(matrix)) {
    numVector[i] <- max(matrix[, i])
  }

  return(numVector)
}

rowMax <- function(matrix)
{
  check.is.matrix(matrix)
  numVector <- vector("numeric", nrow(matrix))

  for(i in 1:nrow(matrix)) {
    numVector[i] <- max(matrix[i,])
  }

  return(numVector)
}

# -----------------
# 12. # colMin    - column minimum values of a matrix
      # rowMin    - row minimum values of a matrix

colMin <- function(matrix)
{
  check.is.matrix(matrix)
  numVector <- vector("numeric", ncol(matrix))

  for(i in 1:ncol(matrix)) {
    numVector[i] <- min(matrix[, i])
  }

  return(numVector)
}

rowMin <- function(matrix)
{
  check.is.matrix(matrix)
  numVector <- vector("numeric", nrow(matrix))

  for(i in 1:nrow(matrix)) {
    numVector[i] <- min(matrix[i,])
  }

  return(numVector)
}

# -----------------
# 13. # cycFill   - cyclic fill a vector//list of given length

cycFill <-
function(data, size)
{
  lengthOfData <- length(data)

  if(size == lengthOfData) {
    return(data)
  }

  stopifnot(class(data) == "list" || class(data) == "numeric" || class(data) == "polynomial"
            || class(data) == "matrix" || class(data) == "array" || class(data) == "integer")

  if(class(data) == "list") {
    dataToReturn <- list()
  } else if(class(data) == "matrix") {
    dataToReturn <- matrix()
  } else if(class(data) == "array") {
    dataToReturn <- array()
  } else {
    dataToReturn <- c()
  }

   if(size %/% lengthOfData >= 1) {
     for(i in 1:(size %/% lengthOfData)) {
      offset <- (i-1) * lengthOfData
      dataToReturn[offset + (1:lengthOfData)] <- data[1:lengthOfData]
    }
   }


   if(size %% lengthOfData) {
     offset <- lengthOfData * size %/% lengthOfData
     chunk_size <- size %% lengthOfData
    dataToReturn[offset + (1:(chunk_size))] <- data[1:(chunk_size)]
   }

   return(dataToReturn)
 }

# -----------------
# 14. # pMdiag    - diagonal polynomial matrix

pMdiag <-function(p, diag_dim, symb="x")
{
  if (!polynom::is.polynomial(p) && !is.list(p)) {
    # try to covert
    if (length(p) != 1 || !is.numeric(p[1])) {
      stop("Only constant can be converted to list of polynimails")
    }
    p <- polynom::polynomial(p[1])
  }

  if (!is.list(p)) {
    p <- list(p)
  }

  if (all(sapply(p, polynom::is.polylist))) {
    stop("Expect list of polynomials")
  }

  if (length(diag_dim) > 2 | (length(diag_dim) == 2 && (diag_dim[1] != diag_dim[2]))) {
    stop("We work for square matrices only!")
  }
	if (length(diag_dim) == 2) {
	  diag_dim <- diag_dim[1]
	}
  stopifnot(length(diag_dim) == 1)

  result <- polyMgen.d(rawData=list(polynom::polynomial(0)),
                       nrow=diag_dim, ncol=diag_dim, symb=symb)
  source <- cycFill(p, diag_dim)
  for(i in 1:diag_dim) {
    result$dlist[[i]][[i]] <- source[[i]]
    result$degree[i, i] <- degree(source[[i]])
  }
  return(result)
}

typed_operation = function(x, type, operation)
{
  M <- polyMconvert(x, "polyMdlist")
  if (!any(type == OPERAIION_TYPES)) {
    stop("Unexpected operation type ")
  }

  if (type == OPERAIION_TYPE_COLUMN) {
    x <- t(x)
  }

  per_rows <- lapply(x$dlist, function(y) {operation(polynom::as.polylist(y))})
  if (type == OPERATION_TYPE_ROW || type == OPERAIION_TYPE_COLUMN) {
    return(polynom::as.polylist(per_rows))
  }
  stopifnot(type == OPERATION_TYPE_TOTAL)
  return(operation(polynom::as.polylist(per_rows)))
}

# -----------------
# 15. # GCD LCM   - greatest common divisor and least common multiple

GCD <- function(x, ...) { UseMethod("GCD"); }
LCM <- function(x, ...) { UseMethod("LCM"); }

GCD.polyMatrix <- function(x, type=OPERATION_TYPE_TOTAL, ...)
{
  return(typed_operation(x, type, polynom::GCD))
}

LCM.polyMatrix <- function(x, type=OPERATION_TYPE_TOTAL,...)
{
  return(typed_operation(x, type, polynom::LCM))
}

# -----------------
# fine
