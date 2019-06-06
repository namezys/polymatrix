# -----------------
# Matrix utilities
#
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
#      is.polyMatrix - consistecy check of a polyMatrix object
# --- # -----------

is.polyMatrix.polyMarray <- function(x)
{
  return(all(class(x) == c("polyMarray", "polyMatrix")))
}

is.polyMatrix.polyMbroad <- function(x)
{
  return(all(class(x) == c("polyMbroad", "polyMatrix")))
}

is.polyMatrix.polyMcells <- function(x)
{
  return(all(class(x) == c("polyMcells", "polyMatrix")))
}

is.polyMatrix.polyMdlist <- function(x)
{
  return(all(class(x) == c("polyMdlist", "polyMatrix")))
}

is.polyMatrix <- function(x)
{
  return(is.polyMatrix.polyMarray(x) || is.polyMatrix.polyMbroad(x)
         || is.polyMatrix.polyMcells(x) || is.polyMatrix.polyMdlist(x))
}


# -----------------
#  1. # pMcol     - a column of a polynomial matrix


pMcol <- function(pm, which=1)
{
  if (class(pm)[2] != "polyMatrix") {
    stop("The 'pm' parameter must be a 'polyMatrix' object")
  }

  plist <- polyMconvert(pm, "polyMdlist")
  pdim <- dim(plist)[1]
  dlist <- vector("list", pdim)
  degree <- matrix(NA, pdim, 1)

  for (i in 1:pdim) {
    dlist[[i]] <- list(plist$dlist[[i]][[which]])
    degree[i,1] <- degree(dlist[[i]][[1]])
  }

  return(structure(list(dim=c(pdim, 1), degree=degree, symb="x", dlist=dlist), class=c("polyMdlist", "polyMatrix")))
}


# -----------------
#  2. # pMrow     - a row of a polynomial matrix

pMrow <- function(pm, which=1)
{

  plist <- polyMconvert(pm, "polyMdlist")
  pdim <- dim(plist)[2]
  dlist <- vector("list", 1)
  dlist[[1]] <- vector("list", pdim)
  degree <- matrix(NA, 1, pdim)

  for (i in 1:pdim) {
    dlist[[1]][[i]] <- plist$dlist[[which]][[i]]
    degree[1,i] <- degree(dlist[[1]][[i]])
  }

  return(structure(list(dim=c(1, pdim), degree=degree, symb="x", dlist=dlist), class=c("polyMdlist", "polyMatrix")))
}

# -----------------
#  3. # pMbas     - bastion vector, the permutation: 'ki'

pMbas <-   # bastion vector of a matrix or a subvector by the index: 'ki'
function(pm,ki,byrow)
  { n <- length(ki)
    if(byrow & n>dim(pm)[1]|(!byrow & n>dim(pm)[2])) stop("Index vector too long!")
    pd <- polyMconvert(pm,"polyMdlist")
    if(dim(pd)[1]==1) pd<-t(pd) # if vector let it a column
    if(dim(pd)[2]==1)
          {v<-pd$dlist[ki]
           degree<-degree(pm,"m")[ki,1,drop=FALSE]}
     else {
            v <- vector("list",n)
            if(byrow)
              for (k in 1:n) v[[k]][1] <- list(pd$dlist[[k]][[ki[k]]])
             else
              for (k in 1:n) v[[k]][1] <- list(pd$dlist[[ki[k]]][[k]])
            degree <- vector("numeric",n)
            for (k in 1:n) degree[k] <- degree(v[[k]][[1]])
           }
    return(
    structure(list(dim = c(n,1),
                   degree = matrix(degree,n,1),
                   symb = "x",
                   dlist = v),
              class = c("polyMdlist","polyMatrix")))
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

pMprod <-
function(pm)
  {
    pm <- polyMconvert(pm,"polyMdlist")
    if(min(dim(pm)) != 1) stop("The input must be a vector!")
    m <- max(dim(pm))
    pr <- polynom::polynomial(1)
    for (k in 1:m)
      pr <- pr*(if(m==dim(pm)[1]) pm$dlist[[k]][[1]] else pm$dlist[[1]][[k]])
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
  switch(class(pm)[1],
         polyMarray = {
           pm$const <- -pm$const
           pm$array <- -pm$array
           pmToReturn <- pm
         },
         polyMbroad = {
           pm$broad <- -pm$broad
           pmToReturn <- pm
         },
         polyMcells = {
           for(i in 0:degree(pm)) {
             pm$cells[[i+1]] <- -pm$cells[[i+1]]
             pmToReturn <- pm
           }
         },
         polyMdlist = {
           for(i in 1:dim(pm)[1]) {
             for(j in 1:dim(pm)[2]) {
               pm$dlist[[i]][[j]] <- -pm$dlist[[i]][[j]]
             }
           }
           pmToReturn <- pm
         },
         stop("Not a regular 'polyMatrix' class object!")
  )
  
  return(pmToReturn)
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

colMax <-
function(matrix)
{
  if(class(matrix) != "matrix") {
     stop("The 'colMax' function requires a 'matrix' class argument!")
  }

  size <- dim(matrix)[2]
  numVector <- vector("numeric", size)

  for(i in 1:size) {
    numVector[i] <- max(matrix[, i])
  }

  return(numVector)
}

rowMax <-
function(matrix)
{
  if(class(matrix) != "matrix") {
    stop("The 'rowMax' function requires a 'matrix' class argument!")
  }

  size <- dim(matrix)[1]
  numVector <- vector("numeric", size)

  for(i in 1:size) {
    numVector[i] <- max(matrix[i,])
  }

  return(numVector)
}

# -----------------
# 12. # colMin    - column minimum values of a matrix
      # rowMin    - row minimum values of a matrix

colMin <-
function(matrix)
{
  if(class(matrix) != "matrix") {
    stop("The 'colMin' function requires a 'matrix' class argument!")
  }

  size <- dim(matrix)[2]
  numVector <- vector("numeric", size)

  for(i in 1:size) {
    numVector[i] <- min(matrix[, i])
  }

  return(numVector)
}

rowMin <-
function(matrix)
{
  if(class(matrix) != "matrix") {
    stop("The 'rowMin' function requires a 'matrix' class argument!")
  }

  size <- dim(matrix)[1]
  numVector <- vector("numeric", size)

  for(i in 1:size) {
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

pMdiag <-
function(p,k,symb="x")
  {
    mclass<-if (class(p)=="polynomial") "OK" else
     if (class(p)=="list") if(all(sapply(p,class)=="polynomial")) "OK" else "BAD" else "BAD"
    if(mclass=="BAD") stop("The input not a 'polynomial' or a 'list' class object of 'polynomial's!")
	if(length(k)>2|(length(k)==2&(k[1]!=k[2]))) stop("We for only square matrices!")
	if(length(k)==2) k<-k[1]
    p <- if (class(p)=="polynomial") list(p) else p
    K<-length(p)
    rD <- list()
    if(k%/%K>0) for(i in 1:(k%/%K)) rD <- c(rD,p)
    if(k%%K!=0) rD <- c(rD,p[1:(k%%K)])
    rawData<-rD[1]
    if(k>1)
      for(i1 in 2:k)
        { for(i2 in 1:k)
            rawData <- c(rawData,list(ch2pn("0")))
          rawData <- c(rawData,rD[i1])  	    }
    return(polyMgen.d(k,k,rawData=rawData,symb=symb))
  }

GCD <- function (...) {
  UseMethod("GCD")
}

GCD.polyMatrix <- function(x, ...)
{
  if(missing("x")) {
    stop("Expected polyMatrix")
  }
  x <- polyMconvert(x, "polyMdlist")
  per_rows <- lapply(x$dlist, function(x) {polynom::GCD(polynom::as.polylist(x))})
  return(polynom::GCD(polynom::as.polylist(per_rows)))
}

LCM <- function (...) {
  UseMethod("LCM")
}

LCM.polyMatrix <- function(x, ...)
{
  if(missing("x")) {
    stop("Expected polyMatrix")
  }
  x <- polyMconvert(x, "polyMdlist")
  per_rows <- lapply(x$dlist, function(x) {polynom::LCM(polynom::as.polylist(x))})
  return(polynom::LCM(polynom::as.polylist(per_rows)))
}

# -----------------
# fine

