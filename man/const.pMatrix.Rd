\name{const.polyMatrix}
\alias{const.polyMatrix}
\alias{const.polynomial}

\title{The constant of a polynom or a polynomial matrix}

\description{
Returns the constant of a \code{polynom} class object or
the constant matrix of a \code{polyMatrix} class object.
}

\usage{const(p)
       \method{const}{polynomial}(p)
       \method{const}{polyMatrix}(p)
}

\arguments{\item{p}{a \code{polynomial} or \code{polyMatrix} class object}}

\value{
A \code{numeric} object, containing the constant of the given
polynom, or of a \code{matrix} class object which contains the
constant matrix of the given polynomial matrix.
}

\seealso{\code{\link{coefs}}, \code{\link{lead}}}

\keyword{internal}

\examples{
p<-polynom::polynomial(0:12)
const(p)

pa<-polyMgen.a(2,2)
pa
const(pa)

pb<-polyMgen.b(2,2)
pb
const(pb)

pc<-polyMgen.c(2,2)
pc
const(pc)

pd<-polyMgen.d(2,2)
pd
const(pd)

# clean up
# rm(p, pa, pb, pc, pd)
}

