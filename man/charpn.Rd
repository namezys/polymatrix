\name{charpn}
\alias{charpn}

\title{The characteristic polynom of a matrix or a polynomial matrix}

\description{The characteristic polynom of a polynomial matrix is a polynom with polynomial coefficients.}

\usage{charpn(M)}

\arguments{
           \item{M}{polynomial matrix object of \code{polyMatrix} class}
}


\value{\code{det(sI-M)}.
When the input is a \code{matrix} class object then the value is a \code{polynomial} object.
When the input is a \code{polyMatrix} class object then a value is \code{charpn} class object,
which is a list of \code{polynomial} objects.
}

\note{The solution of the algorithm is simple one, a signed sum of products.
      There are more elegant and efficient solutions.}

\seealso{\code{\link{predict.charpn}}}

\keyword{utilities}

\examples{
M1<-matrix(c(2,1,-1,0),2,2,byrow=TRUE)
class(M1)  # "matrix"
pM1<-M2pM(M1,"polyMdlist") # conversion matrix => polyMatrix
class(pM1) # "polyMdlist" "polyMatrix"

M2<-matrix(c(1,0,-1,2,1,-1,0,1,1),3,3,byrow=TRUE)
class(M2)  # "matrix"
pM2<-M2pM(M2,"polyMdlist") # conversion matrix => polyMatrix
class(pM2) # "polyMdlist" "polyMatrix"

pM3<-polyMgen.d(2,2,rawData=ch2pn(c("1","x^2","x","0")))
class(pM3) # "polyMdlist" "polyMatrix"

pM4<-polyMgen.d(2,2,rawData=ch2pn(c("x","1","x^2","0")))
class(pM4) # "polyMdlist" "polyMatrix"

pM5<-polyMgen.d(3,3,rawData=ch2pn(c("x","1","x^2","0","x","1","x^2","0","x^2")))
class(pM5) # "polyMdlist" "polyMatrix"

pM6<-polyMgen.d(3,3,rawData=ch2pn(c("x","1","x^2","0","x^2","0","x^2","0","x^2")))
class(pM6) # "polyMdlist" "polyMatrix"

ls() # M1, pM1, M2, pM2, pM3, pM4, pM5, pM6

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# the eigenvalues of a matrix are
#   roots of the characteristic polynomial

pn1 <- charpn(M1) # the characteristic polynom of M1
ev <- eigen(M1)$values
ev # 1 1
predict(pn1,ev) # OK: zero matrix

pn2 <- charpn(M2) # the characteristic polynom of M2
ev <- eigen(M2)$values
round(ev,3) # 1.5+1.323i 1.5-1.323i 0.0+0.000i
round(predict(pn2,ev)) # OK:  0+0i 0+0i 0+0i

# clean up
# rm(pn1, pn2, ev)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# the characteristic polynomial and
#  the Cayley-Hamilton theorem for 'matrix' and 'polyMatrix' class objects

# ---
# M1, pM1: a 2x2 constant matrix

class(M1) # "matrix"
pn1 <- charpn(M1) # the characteristic polynom of M1
predict(pn1,M1) # OK: zero matrix

class(pM1) # "polyMdlist" "polyMatrix"
pn1p <- charpn(pM1) # the characteristic polynom of pM1
predict(pn1p,pM1) # OK: zero matrix

# ---
# M2, pM2: a 3x3 constant matrix

class(M2) # "matrix"
pn2 <- charpn(M2) # the characteristic polynom of M2
predict(pn2,M2) # OK: zero matrix

class(pM2) # "polyMdlist" "polyMatrix"
pn2p<-charpn(pM2) # the characteristic polynom of pM2
predict(pn2p,pM2) # OK: zero matrix

# ---
# pM3, pM4: an arbitrary 2x2 polyMatrix

class(pM3) # "polyMdlist" "polyMatrix"
pn3p<-charpn(pM3) # the characteristic polynom of pM3
predict(pn3p,pM3) # OK: zero matrix

class(pM4) # "polyMdlist" "polyMatrix"
pn4p<-charpn(pM4) # the characteristic polynom of pM4
predict(pn4p,pM4) # OK: zero matrix

# ---
# pM5, pM6: an arbitrary 3x3 polyMatrix

class(pM5) # "polyMdlist" "polyMatrix"
pn5p<-charpn(pM5) # the characteristic polynom of pM4
predict(pn5p,pM5) # OK: zero matrix

class(pM6) # "polyMdlist" "polyMatrix"
pn6p<-charpn(pM6) # the characteristic polynom of pM4
predict(pn6p,pM6) # OK: zero matrix


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ls()

#            1.     2.   |  3.     4.   |   5.     6.  |   7.     8.
# case      real const.pn| real const.pn| arb.pn arb.pn| arb.pn arb.pn
# size      2x2    2x2   | 3x3    3x3   |  2x2    2x2  |  3x3    3x3
# matrices: M1     pM1   | M2     pM2   |  pM3    pM4  |  pM5    pM6
# char.pn:  pn1    pn1p  | pn2    pn2p  |  pn3p   pn4p |  pn5p   pn6p

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# clean up
# rm(M1, M2, pM1, pM2, pM3, pM4, pM5, pM6)
# rm(pn1, pn1p, pn2, pn2p, pn3p, pn4p, pn5p, pn6p)
}



