\name{dim}
\alias{dim}
\alias{dim.polyMatrix}
\title{The dimension of a polynomial matrix}
\description{Retuns the value of the $dim element of the given \code{polyMatrix} object.}

\usage{\method{dim}{polyMatrix}(x)}

\arguments{ \item{x}{a \code{polyMatrix} class object} }

\details{A method for \code{polyMatrix} objects.
Does not check the validity of the $dim element.}

\value{A 2 element vector which contains the dimension parameters of the input \code{polyMatrix}.}


\examples{
dim(matrix(1:12,3,4)) # dim of a real matrix

A <- polyMgen.a()
class(A)# polyMarray
dim(A) # dim of a polyMatrix

B <- polyMconvert(A,"polyMdlist")
class(B)# polyMdlist
dim(B)

dim(polyMgen.a())
dim(polyMgen.b())
dim(polyMgen.c())
dim(polyMgen.d())

# clean up
# rm(A, B)
}

