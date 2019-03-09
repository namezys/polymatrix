# -----
# Generators for pMatrix class polynomial matrices

#  0. # pMgen          - call the generators

#  1. # pMgen.a       - pMatrix in pMarray form
#  2. # pMgen.b       - pMatrix in pMbroad form
#  3. # pMgen.c       - pMatrix in pMcells form
#  4. # pMgen.d       - pMatrix in pMdlist form

#  5. # pMgen.varma   - Generate a pMvarma class polynomial matrix pair

# ----------------------
#  0. # pMgen          - call the generators

pMgen <-
function(k,j,rawData,symb,rand,degree,byrow,sm="pMarray",...)
{
  method <- substr(sm,3,3)
  if (!method %in% c("a","b","c","d")) {
    stop("the admissible storage methods are: 'pMarray','pMbroad','pMcells' and 'pMdlist'!")
  }
  if (missing("degree")) {
    degree <- 2
  }
  if (missing("k")) {
    k <- 2
  }
  if (missing("j")) {
    j <- 3
  }
  if (missing("symb")) {
    symb <- "x"
  }
  if (missing("rand")) {
    rand <- FALSE
  }
  if (missing("byrow")) {
    byrow <- FALSE
  }
  if (missing("rawData")) {
    rawData <- switch (method,
      a=1:(k * j * (max(degree) + 1)),
      b=1:(k * j * (degree + 1)),
      c=1:(k * j * (max(degree)+1)),
      d=list(polynom::polynomial(1:3))
    )
  }
  fabric = switch(method, a=pMgen.a, b=pMgen.b, c=pMgen.c, d=pMgen.d)
  return(fabric(k=k, j=j, rawData=rawData, symb=symb, rand=rand, degree=degree, byrow=byrow))
}


# ----------------------
#  1. # pMgen.a       - pMatrix in pMarray form

pMgen.a <-
function(k=2, j=3, rawData=1:(k * j * (max(degree) + 1)), symb="x", rand=FALSE, degree=2, byrow=FALSE)
{
  # fill default arguments
  rows <- k
  columns <- j
  if (missing("degree")) {
      degree <- matrix(sample(0:3, rows * columns, prob=c(1, 2, 3, 2), replace=TRUE), rows, columns)
  } else if(!is.matrix(degree)) {
      degree <- matrix(degree, rows, columns)
  }
  max_degree <- max(degree)
  raw_data_size <- rows * columns + sum(degree)
  if (is.function(rand)) {
    rawData <- rand(raw_data_size)
  }
  if (is.logical(rand) && rand) {
    rawData <- rnorm(raw_data_size)
  }
  # prepareind dato for matrix
  rawData <- cycFill(rawData,raw_data_size)
  in_pos <- 1
  out_pos <- 1
  rawDataSupp <- rep(0, rows * columns * (max_degree + 1))
  for (ij in 1:columns) {
    for (ik in 1:rows) {
      rawDataSupp[out_pos:(out_pos + degree[ik, ij])] <- rawData[in_pos:(in_pos + degree[ik, ij])];
	    in_pos <- in_pos + degree[ik, ij] + 1
	    out_pos <- out_pos + max_degree + 1
    }
  }
  # build constants
  const_idx <- seq(1, by=max_degree + 1, length=rows * columns)
  const_names <- list(paste0("x", 1:rows), paste0("y", 1:columns))
  const <- matrix(rawDataSupp[const_idx], rows, columns, dimnames=const_names, byrow=byrow)
  if (max_degree == 0) {
    array <- NULL
  } else {
    if (max_degree == 1) {
      dimnames <- list(paste0("x", 1:rows), paste0("y", 1:columns), symb)
    } else {
      dimnames <- list(paste0("x", 1:rows), paste0("y", 1:columns), c(symb, paste0(symb, "^", 2:max_degree)))
    }
    coefficient_data <- rawDataSupp[-const_idx]
    coefficient_matrix <- t(matrix(coefficient_data, max_degree))
    array <- array(as.vector(coefficient_matrix), dim=c(rows, columns, max_degree), dimnames=dimnames)
  }
  pm <- list(dim=c(rows, columns), degree=degree, symb=symb, const=const, array=array)
  class(pm) <- c("pMarray","pMatrix")
  return(pm)
}

# ----------------------
#  2. # pMgen.b       - pMatrix in pMbroad form

pMgen.b <-
function(k=2, j=3, rawData=1:(k * j * (degree + 1)), symb="x", rand=FALSE, degree=2, byrow=FALSE)
{
  pa <- pMgen.a(k=k, j=j, degree=degree, rawData=rawData, symb=symb, rand=rand)
  pb <- pMconvert(pa,"pMbroad")
  return(pb)
}

# ----------------------
#  3. # pMgen.c       - pMatrix in pMcells form

pMgen.c <-
function(k=2, j=3, rawData=1:(k * j * (max(degree) + 1)), symb="x", rand=FALSE, degree=2, byrow=FALSE)
{
  pa <- pMgen.a(k=k, j=j, degree=degree, rawData=rawData, symb=symb, rand=rand)
  pc <- pMconvert(pa,"pMcells")
  return(pc)
}


generate_random_d <- function(rand, degree, byrow, data_size)
{
  if (is.null(rand) || rand == FALSE) {
    # use default
    return(list(polynomial(1:3)))
  }
  # we have to generate rawData
  if (is.logical(rand)) {
    stopifnot(rand == TRUE)
    dgm <- function() { rgeom(1,.33) + 1 }
  } else {
    stopifnot(is.function(rand))
    dgm <- rand
  }
  
  data <- vector("list", data_size)
  deg <- as.numeric(if(byrow) t(degree) else degree)
  for (i1 in 1:data_size) {
    p_coef <- vector("numeric", deg[i1] + 1)
    for (i2 in 0:deg[i1]) {
      p_coef[i2 + 1] <- dgm()
    }
    data[[i1]] <- polynom::polynomial(p_coef)
  }
  return(data)
}

convert_to_polynome <- function(data)
{
  result = vector("list", length(data))
  for(i in 1:length(data)) {
    if (is.polynomial(data[[i]])) {
      result[[i]] <- data[[i]]
    } else {
      result[[i]] <- polynom::polynomial(data[[i]])
    }
  }
  return(result)
}

# ----------------------
#  4. # pMgen.d       - pMatrix in pMdlist form

pMgen.d <- function(k=2, j=3, rawData=NULL, symb="x", rand=NULL, degree=NULL, byrow=FALSE)
{
  rows <- k
  columns <- j
  data_size <- rows * columns
  
  # generate rawData if it's empty
  if (is.null(rawData)) {
    # generate degree matrix if necessary, it is used for generate rawData
    if (is.null(degree)) {
      degree <- matrix(sample(0:3, data_size, c(1, 2, 3, 2), replace=TRUE), rows, columns)
    }
    if (!is.matrix(degree)) {
      degree <- matrix(degree, raws, columns)
    }
    rawData <- generate_random_d(rand=rand, degree=degree, byrow=byrow, data_size=data_size)
  } else {
    # if rawData was provided, rand and degree would be invalid arguments
    if (!is.null(degree)) {
      stop("degree is invalid argument with rawData")
    }
    if (!is.null(rand)) {
      stop("rand is invalid argument with rawData")
    }
  }
  # convert rawData if necessary
  rawData <- convert_to_polynome(rawData)
  # fill empty elements
  rawData<-cycFill(rawData, data_size)

  dlist <- vector("list", rows)
  if (byrow) {
    for (i1 in 1:rows) {
      dlist[[i1]] <- vector("list", j)
    }
    for (i in 1:data_size - 1) {
      dlist[[i %/% columns + 1]][[i %% columns + 1]] <- rawData[[i + 1]]
    }
  } else {
    for (i1 in 1:rows) {
      # by col data
      dlist[[i1]] <- rawData[(1:columns - 1) * rows + i1]
    }
  }

  # refill
  d <- matrix(0, rows, columns);
  for (r in 1:rows) {
    for(c in 1:columns) {
      d[r, c] <- degree(dlist[[r]][[c]])
    }
  }
  
  # build
  pd <- list(dim=c(rows, columns), degree=d, symb=symb, dlist=dlist)
  class(pd) <- c("pMdlist","pMatrix")
  return(pd)
}


# ----------------------
#  5. # pMgen.varma       - Generate a pMvarma class polynomial matrix pair

pMgen.varma <- 
function(k=3,j=3,rawData=list(polynomial(1:3)),symb="x",rand=FALSE,degree=c(1,1), byrow = FALSE)   
{ 
  if(length(degree)!=2) stop("Use two degree parameters (p,q)!")
  AR <- if(!is.na(degree[1])) pMgen.d(k,j,rand=TRUE,degree=degree[1]) else NA
  MA <- if(!is.na(degree[2])) pMgen.d(k,j,rand=TRUE,degree=degree[2]) else NA
  varma<-list(degree=c(ar=degree[1],ma=degree[2]),AR=AR,MA=MA)
  class(varma)<-"pMvarma"
  return(varma) 
}



# -----
# fine
