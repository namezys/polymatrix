\name{const}
\alias{const}

\title{The constant of a polynom or a polynomial matrix}

\description{Returns the constant of the given polynom or polynomial matrix.}

\usage{const(p)}

\arguments{\item{p}{A \code{polynomial} or \code{polyMatrix} class object}}

\details{Get out 
         the constant of a \code{polynom} class object or 
         the constant matrix of a \code{polyMatrix} class object.}

\value{A \code{numeric} object, containing the constant of the given polynom, or 
       a \code{matrix} class object, containing the constant matrix of the given polynomial matrix.}

\seealso{\code{\link{coefs}}, 
         \code{\link{lead}}, 
         \code{\link{const.polyMatrix}}, 
         \code{\link{const.polynomial}}}

\keyword{utilities}

\examples{
const(polynom::polynomial(0:12))
const(polyMgen.a())
const(polyMgen.b())
const(polyMgen.c())
const(polyMgen.d())
}
