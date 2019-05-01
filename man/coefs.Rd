\name{coefs}
\alias{coefs}

\title{The coefficients of a polynom or polynomial matrix}

\description{Returns the coefficients of the given polynom or polynomial matrix.}

\usage{coefs(p,degree="all")}

\arguments{  
\item{p}{a \code{polynomial} or \code{polyMatrix} class object}
\item{degree}{"all" or the serial numben of requested coefficients.}   
}

\details{
Two methods. One for a \code{polynomial} objects, 
the other for \code{polyMatrix} objects.
}

\value{
 A \code{vector} of requested coefficients of the input polynom or 
 a \code{list} of requested coefficient matrices of the input \code{polyMatrix}.
}

\seealso{
  \code{\link{const}}, \code{\link{lead}}, 
  \code{\link{coefs.polynomial}}, \code{\link{coefs.polyMatrix}}
}

\keyword{utilities}

\examples{
coefs(polynom::polynomial(12:0))
coefs(polynom::polynomial(12:0),degree=c(2,3))
coefs(polyMgen.a())
coefs(polyMgen.b())
coefs(polyMgen.c())
coefs(polyMgen.d())
coefs(polyMgen.d(),degree=1)
}

