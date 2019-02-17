\name{charpn}
\alias{charpn}

\title{The characteristic polynom of a matrix or polynomial matrix}

\description{In the case of  polynomial matrix it is a polynom by polynomial coefficients}

\usage{charpn(M)}

\arguments{
           \item{M}{an \code{pMatrix} class polynomial matrix object}
} 

\details{more details than the description above \code{det(sI-M)}}

\value{
When the input is a \code{matrix} class object then a \code{polynomial} object.
When the input is a \code{pMatrix} class object then a \code{charpn} class object, 
which is a list of \code{polynomial} objects
}

\note{The solution of the algorith is very primitive, a signed sum of products.
      Exists more elegant and efficient methods.}

\seealso{\code{\link{predict.charpn}}}

\keyword{utilities}

\examples{
rm(list=ls())

M1<-matrix(c(2,1,-1,0),2,2,byrow=TRUE)
class(M1)  # "matrix"
pM1<-M2pM(M1,"pMdlist") # conversion matrix => pMatrix
class(pM1) # "pMdlist" "pMatrix"

M2<-matrix(c(1,0,-1,2,1,-1,0,1,1),3,3,byrow=TRUE)
class(M2)  # "matrix"
pM2<-M2pM(M2,"pMdlist") # conversion matrix => pMatrix
class(pM2) # "pMdlist" "pMatrix"

pM3<-pMgen.d(2,2,rawData=ch2pn(c("1","x^2","x","0")))
class(pM3) # "pMdlist" "pMatrix"

pM4<-pMgen.d(2,2,rawData=ch2pn(c("x","1","x^2","0")))
class(pM4) # "pMdlist" "pMatrix"

pM5<-pMgen.d(3,3,rawData=ch2pn(c("x","1","x^2","0","x","1","x^2","0","x^2")))
class(pM5) # "pMdlist" "pMatrix"

pM6<-pMgen.d(3,3,rawData=ch2pn(c("x","1","x^2","0","x^2","0","x^2","0","x^2")))
class(pM6) # "pMdlist" "pMatrix"
      
ls() # M1, pM1, M2, pM2, pM3, pM4, pM5, pM6


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# the characteristic polynomial and 
#  the Cayley-Hamilton theorem for 'matrix' and 'pMatrix' class objects

# ---
# M1, pM1: a 2x2 constant matrix

class(M1) # "matrix"
pn1 <- charpn(M1) # the characteristic polynom of M1 
predict(pn1,M1) # OK: zero matrix

class(pM1) # "pMdlist" "pMatrix"
pn1p <- charpn(pM1) # the characteristic polynom of pM1 
predict(pn1p,pM1) # OK: zero matrix

# ---
# M2, pM2: a 3x3 constant matrix

class(M2) # "matrix"
pn2 <- charpn(M2) # the characteristic polynom of M2 
predict(pn2,M2) # OK: zero matrix

class(pM2) # "pMdlist" "pMatrix"
pn2p<-charpn(pM2) # the characteristic polynom of pM2 
predict(pn2p,pM2) # OK: zero matrix

# ---
# pM3, pM4: an arbitrary 2x2 pMatrix

class(pM3) # "pMdlist" "pMatrix"
pn3p<-charpn(pM3) # the characteristic polynom of pM3
predict(pn3p,pM3) # OK: zero matrix

class(pM4) # "pMdlist" "pMatrix"
pn4p<-charpn(pM4) # the characteristic polynom of pM4 
predict(pn4p,pM4) # OK: zero matrix

# ---
# pM5, pM6: an arbitrary 3x3 pMatrix

class(pM5) # "pMdlist" "pMatrix"
pn5p<-charpn(pM5) # the characteristic polynom of pM4 
predict(pn5p,pM5) # OK: zero matrix

class(pM6) # "pMdlist" "pMatrix"
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


rm(M1,M2,pM1,pM2,pM3,pM4,pM5,pM6)
rm(pn1,pn1p,pn2,pn2p,pn3p,pn4p,pn5p,pn6p)
 
}


   
