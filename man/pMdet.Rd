\name{pMdet}
\alias{pMdet}

\title{Calculate the determinant of a real or polynomial matrix}

\description{The determinant of the given square matrix.}

\usage{pMdet(pm)}
\arguments{
  \item{pm}{a \code{polyMatrix} class object}
  }

\value{A \code{numeric} or \code{polynomial} class polynom.}

\seealso{\code{\link{pMsub}}, \code{\link{pMadj}}}

\keyword{math}

\examples{

rd <- c(3,0:7)

D <- polyMgen.a(3,3,rawData=rd,degree=1,symb="x")
D
#  3        5 + 6*x  2 + 3*x
#  1 + 2*x  7 + 3*x  4 + 5*x
#  3 + 4*x  x        6 + 7*x

pMdet(D) # 114 + 150*x + 22*x^2 + 6*x^3

elem <- function(x,k,j) (pMbas(x,c(rep(1,k-1),j),byrow=TRUE)$dlist[[k]])[[1]]
d11 <- elem(D,1,1);d12 <- elem(D,1,2);d13 <- elem(D,1,3)
d21 <- elem(D,2,1);d22 <- elem(D,2,2);d23 <- elem(D,2,3)
d31 <- elem(D,3,1);d32 <- elem(D,3,2);d33 <- elem(D,3,3)

polyMgen.d(3, 3, rawData=list(d11,d12,d13,d21,d22,d23,d31,d32,d33), symb="x", byrow=TRUE)

# direct det calculation
d11*d22*d33+d12*d23*d31+d13*d21*d32-(d13*d22*d31+d11*d23*d32+d12*d21*d33)
# 114 + 150*x + 22*x^2 + 6*x^3


D <- polyMgen.a(3,3,rawData=rd,degree=0)
D

M <- matrix(rd, ncol = 3)
M

c(pMdet(D),det(M))  # det of a polyMatrix == det of a real matrix  (!)

# clean up
# rm(D, d11, d12, d13, d21, d22, d23, d31, d32, d33, elem, M, rd)

}

