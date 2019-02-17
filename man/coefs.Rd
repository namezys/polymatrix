\name{coefs}
\alias{coefs}

\title{The coefficients of a polynom or polynomial matrix}

\description{Returns the coefficients of the given polynom or polynomial matrix.}

\usage{coefs(p,degree="all")}

\arguments{  
\item{p}{a \code{polynomial} or \code{pMatrix} class object}
\item{degree}{"all" or the serial numben of requested coefficients.}   
}

\details{
Two methods. One for a \code{polynomial} objects, 
the other for \code{pMatrix} objects.
}

\value{
 A \code{vector} of requested coefficients of the input polynom or 
 a \code{list} of requested coefficient matrices of the input \code{pMatrix}.
}

\seealso{
  \code{\link{const}}, \code{\link{lead}}, 
  \code{\link{coefs.polynomial}}, \code{\link{coefs.pMatrix}}
}

\keyword{utilities}

\examples{
coefs(polynom::polynomial(12:0))
coefs(polynom::polynomial(12:0),degree=c(2,3))
coefs(pMgen.a())
coefs(pMgen.b())
coefs(pMgen.c())
coefs(pMgen.d())
coefs(pMgen.d(),degree=1)
}

