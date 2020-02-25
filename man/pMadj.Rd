\name{pMadj}
\alias{pMadj}

\title{The adjugate of the given polynomial matrix}

\description{The adjugate of the given matrix.
Also the transpose of the matrix
formed from the determinants of submatrices
multiplied by the chessboard rule signs.}

\usage{pMadj(pm)}
\arguments{\item{pm}{a 'polyMatrix' class object} }


\value{A \code{polyMatrix} class object, the adjugate of the given polynomial matrix.}

\seealso{\code{\link{pMdet}}, \code{\link{pMsub}}}
\examples{
(A<-polyMgen.a(2,2))
# 1 + 2*x + 3*x^2           7 + 8*x + 9*x^2 + 10*x^3
# 4 + 5*x + 6*x^2           11 + 12*x
pMdet(A)
# -17 - 33*x - 61*x^2 - 97*x^3 - 104*x^4 - 60*x^5
pMsub(A,1,1)*pMsub(A,2,2)-pMsub(A,1,2)*pMsub(A,2,1)
# -17 - 33*x - 61*x^2 - 97*x^3 - 104*x^4 - 60*x^5
(B<-pMadj(A))
# 11 + 12*x                  -7 - 8*x - 9*x^2 - 10*x^3
#  -4 - 5*x - 6*x^2           1 + 2*x + 3*x^2
A*B         # diagonal
pMdet(A)

B*A
pMdet(A)

# real matrices

X<-matrix(c(3,0:7), ncol = 3)
X
#   3    2    5
#   0    3    6
#   1    4    7

det(X) # -12
solve(X)*det(X)
#  -3    6   -3
#   6   16  -18
#  -3  -10    9
pMadj(M2pM(X)) # the same

# clean up
# rm(A, B, X)

}
\keyword{math}

