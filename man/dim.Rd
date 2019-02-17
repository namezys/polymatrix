\name{dim}
\alias{dim}
\alias{dim.pMatrix}
\title{The dimension of a polynomial matrix}
\description{Retuns the value of the $dim element of the given \code{pMatrix} object.}

\usage{\method{dim}{pMatrix}(x)}

\arguments{ \item{x}{a \code{pMatrix} class object} }

\details{A method for \code{pMatrix} objects. Does notcontrol the validity of the
$dim element.}

\value{A 2 element vector: the dimension parameter of the input pMatrix.}


\examples{
dim(matrix(1:12,3,4)) # dim of a real matrix

A <- pMgen.a()
class(A)# pMarray
dim(A) # dim of a pMatrix

B <- pMconvert(A,"pMdlist")
class(B)# pMdlist
dim(B)

dim(pMgen.a())
dim(pMgen.b())
dim(pMgen.c())
dim(pMgen.d())

rm(A,B)
}

