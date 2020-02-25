\name{\%X\%}

\alias{\%X\%}

\title{Multiplication of two \code{polyMatrix} class polynomial matrices}

\description{
Matrix multipliction for \code{polyMatrix} class polynomial matrices.
Multiplies the two matrices only if they are conformable.
}

\usage{left \%X\% right}

\arguments{
\item{left}{a \code{polyMatrix} class object, scalar number or polynomial}
\item{right}{a \code{polyMatrix} class object, scalar number or polynomial}
}

\details{The number of columns in \code{left} must be equal to number of rows in \code{right}.}

\value{A \code{polyMatrix} class object which is the product of
the \code{polyMatrix} class \code{left} and \code{right} matrices.}

\seealso{\code{\link{polyMatrix-package}}}

\keyword{math}

\examples{
pm <- polyMgen.a()
pm * t(pm)

# two constante matrices
a <- matrix(1:12,3,4)
b <- matrix(1:20,4,5)
a \%*\% b

a <- M2pM(a)
b <- M2pM(b)
a * b
a \%X\% b

pM2M(a) \%*\% pM2M(b)

# clean up
# rm(pm, a, b)

}

