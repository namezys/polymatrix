\name{pMsub}
\alias{pMsub}
\title{A submatrix of a polynomial matrix}
\description{Retains or delets the \code{i}-th rows and the \code{j}-th column,
              depending on the sign of indices.}
\usage{pMsub(pm, i, j)}
\arguments{
  \item{pm}{a polynomial matrix}
  \item{i}{the number of rows to retain or delete}
  \item{j}{the number of columns to retain or delete}          }
\details{
If the row or colum number is NULL, then the result is all element from the given columns or rows.
If \code{j} is not given, then the adequate symmetric submatrix.}
\value{A polynomial matrix which is a submatrix of the given matrix in '\code{polyMdlist}' format.}

\seealso{\code{\link{pMdet}}, \code{\link{pMadj}}}

\examples{
A<-polyMgen.a(3,4,1:24,degree=1)
A
pMsub(A, 2, 3)
pMsub(A, 1,NULL)
pMsub(A, NULL, 3)
pMsub(A, 1:2, 2:4)
pMsub(A, -3, 2:5)
pMsub(A, 1:2)

rm(A)
}
\keyword{manip}

