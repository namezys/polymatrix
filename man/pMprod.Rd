\name{pMprod}
\alias{pMprod}

\title{Product of the elements of the given polynomial vector
}

\description{The product of the elements of the vector \code{pm} given in
\code{polyMatrix} class object format.
}

\usage{pMprod(pm)}

\arguments{\item{pm}{a \code{polyMatrix} class object} }

\details{
The input must be a \code{polyMatrix} class vector object.
The result is a \code{polynomial} class polynom.
}

\value{
A \code{polynomial} which is the product of
the elements of the argument \sQuote{\code{pm}}.
}

\seealso{\code{\link{pVsk}}}

\keyword{math}

\examples{
set.seed(32)
A <- polyMgen.a(1,2)
A
pMprod(A)

# clean up
# rm(A)
}

