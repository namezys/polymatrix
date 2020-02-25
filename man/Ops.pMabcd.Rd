\name{Ops.polyMarray}
\alias{Ops.polyMarray}
\alias{Ops.pMblist}
\alias{Ops.polyMcells}
\alias{Ops.polyMdlist}
\title{Arithmetic Ops Group Methods for polyMatrix class objects}
\description{
  Allows arithmetic operations by a left hand side polynomial matrices.
  Contains operators such as addition, multiplication, division, etc.
}
\usage{
\method{Ops}{polyMatrix}(e1,e2)}
\arguments{\item{e1}{an object of class \code{polyMatrix}.}
           \item{e2}{an object of class \code{numeric}, \code{character} or  \code{polyMatrix}.}}
\value{
A \code{c("polyMdlist","polyMatrix")} class polynomial matrix
which is the result obtained by performing the operation on the two arguments.
}
\author{Prohle Tamas}
\seealso{objects to See Also as \code{\link{polyMatrix-package}}, \code{\link{Ops}}}

\keyword{internal}

\examples{
X <- polyMgen.d(2,2, rawData=ch2pn(c("1","1+x^2","x","0")))
Y <- polyMgen.d(2,2, rawData=ch2pn(c("2","2*x+x^2","x","-1")))


X  # signing a polyMatrix
-X
+X

X+1 #  polyMatrix + numeric
X-1

1+X #  numeric + polyMatrix
1-X

(1-X)*2 #  polyMatrix * numeric
(-2)*(1-X)

X           # polyMatrix + matrix
diag(2)+X+diag(2)
-diag(2)-X-diag(2)

X-M2pM(diag(2)) # polyMatrix - polyMatrix -- OK
X+Y
X-Y


# polyMatrix * polynom -- three solutions:
X*"1+3*x+2*x^2" # as a string
"1+3*x+2*x^2"*X

X*diag(list(ch2pn("1+3*x+2*x^2")),dim(X)[2])  # as a diagonal polyMatrix
diag(list(ch2pn("1+3*x+2*x^2")),dim(X)[2])*X

# X*polynom::polynomial(c(1,3,2)) # as a polynomial
# polynom::polynomial(c(1,3,2))*X # does not works!

# polyMatrix product
Z<-X*Y
Z
const(X)
const(Y)
const(X)\%*\%const(Y) # conventional multiplication of two matrices
const(Z) # similar: the constant of the product polyMatrix
const(X)*const(Y) # dissimilar: element wise product of two conventional matrices

# polyMatrix power
X
X^0
X^1
X^2
X*X
X^3

# clean up
# rm(X, Y, Z)
}

