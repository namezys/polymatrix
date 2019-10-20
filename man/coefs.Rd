\name{coefs}
\alias{coefs}

\title{The coefficients of a polynom or polynomial matrix}

\description{Returns the coefficients of the given polynom or polynomial matrix.}

\usage{coefs(p,degree="all")}

\arguments{  
\item{p}{a \code{polynomial} or \code{polyMatrix} class object}
\item{degree}{"all" or the serial number of the requested coefficients.}   
}

\details{
The \code{coefs} consists of two methods. 
One for a \code{polynomial} objects, 
the other one for \code{polyMatrix} objects.
}

\value{
 A \code{vector} of the requested coefficients of the input polynom or 
 a \code{list} of the requested coefficient matrices of the input \code{polyMatrix}.
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

