\name{Ops.pMarray}
\alias{Ops.pMarray}
\alias{Ops.pMblist}
\alias{Ops.pMcells}
\alias{Ops.pMdlist}
\title{Arithmetic Ops Group Methods for pMatrix class objects}
\description{
  Allows arithmetic operations by a left hand side polynomial matrices.
  Contains operators such as addition, multiplication, division, etc.
}
\usage{
\method{Ops}{pMatrix}(e1,e2)}
\arguments{\item{e1}{an object of class \code{pMatrix}.}
           \item{e2}{an object of class \code{numeric}, \code{character} or  \code{pMatrix}.}}
\value{A \code{c("pMdlist","pMatrix")} class polynomial matrix got by 
       performing the operation on the two arguments}
\author{Prohle Tamas}
\seealso{objects to See Also as \code{\link{pMatrix-package}}, \code{\link{Ops}}}

\keyword{internal}

\examples{
rm(list=ls())
X <- pMgen.d(2,2, rawData=ch2pn(c("1","1+x^2","x","0")))
Y <- pMgen.d(2,2, rawData=ch2pn(c("2","2*x+x^2","x","-1")))


X  # signing a pMatrix
-X
+X
 
X+1 #  pMatrix + numeric
X-1

1+X #  numeric + pMatrix  
1-X

(1-X)*2 #  pMatrix * numeric 
(-2)*(1-X)

X           # pMatrix + matrix
diag(2)+X+diag(2) 
-diag(2)-X-diag(2) 

X-M2pM(diag(2)) # pMatrix - pMatrix -- OK
X+Y
X-Y


# pMatrix * polynom -- three solutions:
X*"1+3*x+2*x^2" # as a string 
"1+3*x+2*x^2"*X 

X*diag(list(ch2pn("1+3*x+2*x^2")),dim(X)[2])  # as a diagonal pMatrix
diag(list(ch2pn("1+3*x+2*x^2")),dim(X)[2])*X 

# X*polynom::polynomial(c(1,3,2)) # as a polynomial
# polynom::polynomial(c(1,3,2))*X # does not works!

# pMatrix product 
Z<-X*Y
Z
const(X)
const(Y) 
const(X)\%*\%const(Y) # conventional multiplication of two matrices
const(Z) # similar: the constant of the product pMatrix
const(X)*const(Y) # dissimilar: element wise product of two conventional matrices 

# pMatrix power 
X
X^0
X^1
X^2
X*X
X^3

rm(X,Y,Z)
}

